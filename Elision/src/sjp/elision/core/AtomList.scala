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

/**
 * Encapsulate an ordered list of atoms.
 * 
 * ==Structure and Syntax==
 * An atom list is just that: a list of atoms.  It can come in two forms:
 *  - An ''untyped'' list of atoms, ordered.
 *  - A ''typed'' list of atoms.
 *  
 * The latter incorporates information about the structure of the list;
 * specifically associativity and commutativity.
 * 
 * Represented as a stand-alone atom, an atom list is indicated starting with
 * a percent sign.  This is followed by property indicators, and then by the
 * actual list of atoms, comma separated, in parentheses.
 * 
 * The property indicators are the following.
 *  - A single question mark `?` indicates that no properties have been set,
 *    and this is an untyped list.
 *  - The characters `AC` indicate that the list is both associative and
 *    commutative.
 *  - The single character `A` indicates that the list is associative, but not
 *    commutative.
 *  - The single character `C` indicates that the list is commutative, but not
 *    associative.
 *  - An absence of characters between the percent sign and the opening
 *    parenthesis indicates that the list is neither commutative nor
 *    associative.
 *    
 * Examples are the following.
 *  - `%(5,4,3)` - neither associative nor commutative
 *  - `%?(5,4)` - untyped list
 *  - `%AC(fred,jim,beth)` - associative and commutative
 * 
 * ==Type==
 * All atom lists have ^TYPE as their type.  This may be subject to change
 * later.
 * 
 * ==Equality and Matching==
 * Two lists are equal iff their properties, length, and elements are equal.
 * The lists match iff their properties are the same and their elements can be
 * matched.  This can happen in several ways.
 *  - A list that is untyped is treated as if it were explicitly not associative
 *    or commutative.
 *  - A commutative but not associative list matches iff the elements can be
 *    matched in any order.
 *  - An associative list matches iff the pattern has no more elements than
 *    the subject, and the elements of the subject can be "parenthesized" to
 *    single elements in such a way that the elements of the lists match.
 *  - An associative and commutative list matches iff its elements can be
 *    reordered and parenthesized in such a way that the elements all match.
 * 
 * @param atoms		The list of atoms.  Note that order may be important.
 * @param props		The optional operator properties.  If specified, this must
 * 								be a pair whose first element is associativtiy, and whose
 * 								second element is commutativity.
 */
class AtomList(val atoms: OmitSeq[BasicAtom],
    val props: Option[(Boolean,Boolean)]) extends BasicAtom {
  require(atoms != null)
  
  /** If true, regard this list is associative. */
  val associative = props.getOrElse((false, false))._1
  
  /** If true, regard this list as commutative. */
  val commutative = props.getOrElse((false, false))._2
  
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
      case oper: Operator => Some(oper)
      case _ => None
    }
    
    // Ordered lists only match other ordered lists with matching elements in
    // the same order.
    subject match {
    	case al:AtomList =>
    	  // The lists must be the same length, or they cannot match.
    	  if (atoms.length != al.atoms.length)
    	    Fail("Lists are different sizes.", this, subject)
    	  else
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
    if (changed) (AtomList(newlist, props), true) else (this, false)
  }

  // An atom list is just the list of atoms, separated by commas.  The list may
  // have properties set; if so, those are indicated here.
  def toParseString = atoms.mkParseString(
      "%" + (props match {
        case None => "?"
        case Some((false, false)) => ""
        case Some((true, false)) => "A"
        case Some((false, true)) => "C"
        case Some((true, true)) => "AC"
      }) + "(" , ", ", ")")
  
  /**
   * Provide a "naked" version of the list, without the parens and property
   * indicators.
   * @return	The elements of the list, separated by commas.  Items internal
   * 					to the list may themselves be lists; that is okay, since the
   * 					parse string is used for those atoms.
   */
  def toNakedString = atoms.mkParseString("", ", ", "")
  
  override def toString = "AtomList(" +
  		atoms.mkParseString("OmitSeq(", ",", ")") + "," + props + ")"
  
  override lazy val hashCode = atoms.hashCode
  
  override def equals(other: Any) = other match {
    case AtomList(oatoms, oprops) if (oatoms == atoms && oprops == props) => true
    case _ => false
  }
}

/**
 * Provide convenient construction and extraction.
 */
object AtomList {
  /**
   * Make a new atom list.
   * 
   * @param atoms	The atoms.
   * @param props	The optional list properties, in the from (associative,
   * 							commutative).
   * @return	The requested atom.
   */
  def apply(atoms: OmitSeq[BasicAtom], props: Option[(Boolean, Boolean)] = None) =
    if (props.getOrElse((false,false))._1) {
      // The list is associative.  Flatten any included lists.
      var newlist = IndexedSeq[BasicAtom]()
      for (atom <- atoms) {
  		  atom match {
			    case AtomList(args,oprops) if props == oprops =>
			      // Add the arguments directly to this list.  We can assume it has
			      // already been processed, so no deeper checking is needed.
			      newlist ++= args
			    case _ =>
			    	// Add this item to the list.
			      newlist :+= atom
			  }
      } // Flatten lists.
      new AtomList(newlist, props)
    } else {
      // The list is not associative.  Construct and return the list.
      new AtomList(atoms, props)
    }
  
  /**
   * Extract the pieces of an atom list.
   * 
   * @param list	The atom list.
   * @return	A pair of the actual list of atoms and the properties pair.
   */
  def unapply(list: AtomList) = Some(list.atoms, list.props)
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