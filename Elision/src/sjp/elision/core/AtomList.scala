/*======================================================================
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 * The Elision Term Rewriter
 * 
 * Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com)
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met: 
 * 
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer. 
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution. 
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
======================================================================*/
package sjp.elision.core

import scala.collection.immutable.HashMap
import java.util.LinkedList
import scala.collection.mutable.MapBuilder
import scala.collection.SeqLike

/**
 * Encapsulate an ordered list of atoms.
 * 
 * ==Structure and Syntax==
 * 
 * ==Type==
 * All atom lists have ^TYPE as their type.  This may be subject to change
 * later.
 * 
 * ==Equality and Matching==
 * Two lists are equal iff their properties, length, and elements are equal.
 * The lists match iff their properties are the same and their elements can be
 * matched.  This can happen in several ways.
 *  - Unspecified properties are treated as if they are negated.  So if
 *    a list's associativity is unspecified, the list is treated as non-
 *    associative.
 *  - A commutative but not associative list matches iff the elements can be
 *    matched in any order.
 *  - An associative list matches iff the pattern has no more elements than
 *    the subject, and the elements of the subject can be "parenthesized" to
 *    single elements in such a way that the elements of the lists match.
 *  - An associative and commutative list matches iff its elements can be
 *    reordered and parenthesized in such a way that the elements all match.
 * 
 * @param props		The specified operator properties.
 * @param xatoms	The list of atoms.  Note that order may be important.
 */
class AtomList(val props: AlgProp, xatoms: OmitSeq[BasicAtom])
extends BasicAtom with SeqLike[BasicAtom, AtomList] {
  require(xatoms != null)
  
  /** Whether this list should be regarded as associative. */
  val associative = props.associative.getOrElse(false)
  
  /** Whether this list should be regarded as commutative. */
  val commutative = props.commutative.getOrElse(false)
  
  /** Whether this list should be regarded as idempotent. */
  val idempotent = props.idempotent.getOrElse(false)
  
  /** The atoms in this list. */
  val atoms: OmitSeq[BasicAtom] = if (idempotent) xatoms.distinct else xatoms

  /**
   * Get an element from the list by index.
   * 
   * @param idx	The zero-based index.
   * @return	The requested element.
   */
  def apply(idx: Int) = atoms(idx)
  
  /**
   * Get an iterator over the atoms in the list.
   * 
   * @return	An iterator over the atoms.
   */
  def iterator = atoms.iterator
  
  /**
   * Get the length of the list.
   * 
   * @return	The number of atoms in the list.
   */
  def length = atoms.length
  
  /**
   * View this as a sequence.
   * 
   * @return	The sequence view of this list.
   */
  def seq = atoms
  
  /**
   * Get a new builder for atom lists.
   * 
   * @return	The new builder.
   */
  protected def newBuilder = new AtomListBuilder
  
  import scala.collection.mutable.Builder
  /**
   * Implement the builder for atom lists.
   */
  class AtomListBuilder extends Builder[BasicAtom, AtomList] {
    import scala.collection.immutable.VectorBuilder
    private val _builder = new VectorBuilder[BasicAtom]()
    def +=(elem: BasicAtom) = { _builder += elem ; this }
    def clear() {_builder.clear()}
    def result() = new AtomList(props, _builder.result())
  }
    
  // Map each constant to its index.
  val constantMap = scala.collection.mutable.HashMap[BasicAtom, Int]()
  for (i <- 0 until atoms.length)
    if (atoms(i).isConstant) constantMap(atoms(i)) = i
  
  // The type of all lists is the type universe.  This may be changed later.
  val theType = TypeUniverse
  
  // The list is constant iff all elements are constant.
  val isConstant = atoms.forall(_.isConstant)
  
  /** The constant pool is derived from the children of the list. */
  val constantPool = Some(BasicAtom.buildConstantPool(1, atoms:_*))
  
  // The De Bruijn index is equal to the maximum index of the atoms in the
  // sequence.  Compute that now.
  val deBruijnIndex = atoms.foldLeft(0)(_ max _.deBruijnIndex)
  
  /**
   * The depth is equal to the maximum depth of the child atoms, plus one.
   */
  val depth = atoms.foldLeft(0)(_ max _.depth) + 1
  
  /**
   * Attempt to match this atom list against the provided atom list.
   * 
   * @param subject	The subject to match.
   * @param binds		Bindings which must be honored on any match.
   * @param op			Optional operator to use for associative matching.
   * @return				Matching outcome.
   */
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      op: Option[Any]) = {
    val operator = op match {
      case oper: Some[Operator] => oper
      case _ => None
    }
    
    // Ordered lists only match other ordered lists with matching elements in
    // the same order.
    subject match {
    	case al:AtomList =>
  	    // The properties must be the same, or they cannot match.
  	    if (props != al.props)
  	      Fail("List properties do not match.", this, subject)
  	    else
  	      // Now all the items in the list much match.  We use the sequence
  	      // matcher for that.
  	      if (associative)
  	        if (commutative)
  	        	matcher.ACMatcher.tryMatch(this, al, binds, operator)
  	        else
  	        	matcher.AMatcher.tryMatch(this, al, binds, operator)
	        else
	          if (commutative)
	          	matcher.CMatcher.tryMatch(this, al, binds)
	          else
	          	SequenceMatcher.tryMatch(atoms, al.atoms, binds)
      case _ => Fail("Not implemented.")
    }
  }

  def rewrite(binds: Bindings) = {
    // We must rewrite every child atom, and collect them into a new sequence.
    val (newlist, changed) = SequenceMatcher.rewrite(atoms, binds)
    if (changed) (new AtomList(props, newlist), true) else (this, false)
  }

  // An atom list is just the list of atoms, separated by commas.  The list may
  // have properties set; if so, those are indicated here.
  def toParseString = atoms.mkParseString(
      "%" + (props.toShortString) + "(" , ", ", ")")
  
  /**
   * Provide a "naked" version of the list, without the parens and property
   * indicators.
   * @return	The elements of the list, separated by commas.  Items internal
   * 					to the list may themselves be lists; that is okay, since the
   * 					parse string is used for those atoms.
   */
  def toNakedString = atoms.mkParseString("", ", ", "")
  
  override def toString = "AtomList(" + props + ", " +
  		atoms.mkParseString("OmitSeq(", ",", ")") + ")"
  
  override lazy val hashCode = atoms.hashCode
  
  override def equals(other: Any) = other match {
    case AtomList(oprops, oatoms) if (oatoms == atoms && oprops == props) => true
    case _ => false
  }
}

/**
 * Provide convenient construction and extraction.
 */
object AtomList {
  
  /**
   * Process the atoms and build the new list.  This reduces any included
   * associative lists.
   */
  private def process(props: AlgProp, atoms: OmitSeq[BasicAtom]) =
    if (props.associative.getOrElse(false)) {
      // The list is associative.  Flatten any included lists.
      var newlist = IndexedSeq[BasicAtom]()
      for (atom <- atoms) {
  		  atom match {
			    case AtomList(oprops, args) if props == oprops =>
			      // Add the arguments directly to this list.  We can assume it has
			      // already been processed, so no deeper checking is needed.
			      newlist ++= args
			    case _ =>
			    	// Add this item to the list.
			      newlist :+= atom
			  }
      } // Flatten lists.
      new AtomList(props, newlist)
    } else {
      // The list is not associative.  Construct and return the list.
      new AtomList(props, atoms.toIndexedSeq[BasicAtom])
    }

  
  /**
   * Make a new atom list.
   * 
   * @param atoms	The atoms.
   * @param props	The optional list properties, in the from (associative,
   * 							commutative).
   * @return	The requested atom.
   */
  def apply(props: AlgProp, atoms: OmitSeq[BasicAtom]) =
    process(props, atoms)

  /**
   * Make a new atom list.
   * 
   * @param atoms	The atoms.
   * @param props	The optional list properties, in the from (associative,
   * 							commutative).
   * @return	The requested atom.
   */
  def apply(props: AlgProp)(atoms: BasicAtom*) =
    process(props, OmitSeq(atoms:_*))
  
  /**
   * Extract the pieces of an atom list.
   * 
   * @param list	The atom list.
   * @return	A pair of the actual list of atoms and the properties pair.
   */
  def unapply(list: AtomList) = Some(list.props, list.atoms)
}

/**
 * Provide alternate injection and extractor methods useful with arguments to
 * an operator.
 */
object Args {
  /**
   * Get the atoms in the list as a sequence.
   * 
   * @param list	 The sequence to extract.
   * @return	The extracted sequence.
   */
	def unapplySeq(al:AtomList) = Some(al.atoms)
}
