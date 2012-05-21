/*======================================================================
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 * The Elision Term Rewriter
 * 
 * Copyright (c) 2012 by UT-Battelle, LLC.
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
 * Collection of administrative costs for redistribution of the source code or
 * binary form is allowed. However, collection of a royalty or other fee in excess
 * of good faith amount for cost recovery for such redistribution is prohibited.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER, THE DOE, OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
======================================================================*/
package ornl.elision.core

import scala.collection.IndexedSeq
import ornl.elision.core.matcher._

/**
 * Fast access to an untyped empty sequence.
 */
object EmptySeq extends AtomSeq(AlgProp(), IndexedSeq())

/**
 * An atom sequence is just that: a sequence of atoms.
 * 
 * == Properties ==
 * Atom sequences may have properties like operators.  That is, they may be
 * commutative, associative, idempotent, and may have absorbers and identities.
 * These properties have an effect on the list when it is constructed.  For
 * example, a list that is associative, commutative, and idempotent is
 * essentially a set.  If not idempotent, it is a multiset.  Properties are
 * specified with an algebraic properties object.
 * 
 * == Use ==
 * Create a list by specifying the properties and then providing a list of 
 * atoms to include in the list.  The atoms are specified using an instance
 * of an `IndexedSeq`.
 * 
 * @param props		The properties for this atom sequence.
 * @param xatoms	The sequence of atoms in this sequence.  Note that, depending
 * on the specified properties, the stored sequence may be different.
 */
class AtomSeq(val props: AlgProp, xatoms: IndexedSeq[BasicAtom])
extends BasicAtom with IndexedSeq[BasicAtom] {
  require(xatoms != null)
  require(props != null)
  
  /**
   * Whether this sequence is specified to be associative.  Note that false here
   * just means the sequence was not marked as associative; it's associativity
   * may be unspecified.
   */
  val associative = props.isA(false)
  
  /**
   * Whether this sequence is specified to be commutative.  Note that false here
   * just means the sequence was not marked as commutative; it's associativity
   * may be unspecified.
   */
  val commutative = props.isC(false)
  
  /**
   * Whether this sequence is specified to be idempotent.  Note that false here
   * just means the sequence was not marked as idempotent; it's associativity
   * may be unspecified.
   */
  val idempotent = props.isI(false)
  
  /**
   * The absorber for this sequence, if any.
   */
  val absorber = props.absorber
  
  /**
   * The identity for this sequence, if any.
   */
  val identity = props.identity
  
  /**
   * The atoms in this sequence.
   */
  val atoms = AtomSeq.process(props, xatoms)

  /**
   * This is a mapping from constants in the sequence to the (zero-based)
   * index of the constant.
   */
  val constantMap = scala.collection.mutable.HashMap[BasicAtom, Int]()
  for (i <- 0 until atoms.length)
    if (atoms(i).isConstant) constantMap(atoms(i)) = i
  
  import SymbolicOperator.LIST
  
  /**
   * The type of a sequence is derived from looking at the types of the
   * elements of the sequence.  If the elements all have the same type,
   * then the result is that type.  If the elements have different types,
   * or the sequence is empty, then the type is ANY.
   */
  lazy val theType = {
      if (atoms.length == 0) LIST(ANY) else {
		    val aType = atoms(0).theType
		    if (atoms.forall(aType == _.theType)) LIST(aType) else LIST(ANY)
      }
    }
  val isConstant = atoms.forall(_.isConstant)
  val isTerm = atoms.forall(_.isTerm)
  val constantPool = Some(BasicAtom.buildConstantPool(1, atoms:_*))
  val deBruijnIndex = atoms.foldLeft(0)(_ max _.deBruijnIndex)
  val depth = atoms.foldLeft(0)(_ max _.depth) + 1
  
  /**
   * Get an element of this sequence by (zero-based) index.
   * 
   * @param idx	The index.
   * @return	The requested element.
   */
  def apply(idx: Int) = atoms(idx)
  
  /** The length of this sequence. */
  def length = atoms.length
  
  /**
   * Try to match this atom sequence, as a pattern, against the provided atom.
   * Atom sequences only match other atom sequences, and they only match if
   * they have the same properties and elements.  The provided bindings must be
   * honored by any successful match, and the hint may be used to specify an
   * operator.  The operator is then used during associative matching to
   * correctly type subsequences.
   */
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]): Outcome = {
    // We only care if the hint is an operator.  We do this in two steps, since
    // the "obvious" way to do it doesn't work because of type erasure.  Boo!
    val operator = hints match {
      case Some(value) => value match {
        case oper: Operator => Some(oper)
        case oper: OperatorRef => Some(oper.operator)
        case _ => None
      }
      case _ => None
    }
    
    // Atom sequences only match other atom sequences.
    subject match {
      case as: AtomSeq =>
        // Local function to complete sequence matching by matching the actual
        // sequences using the appropriate matching algorithm based on the
        // properties.
        def doMatchSequences(usebinds: Bindings): Outcome = {
        	// Now we have to decide how to compare the two sequences.  Note that
          // if the properties matching changes, this will like have to change,
          // too, to use the matched properties.
          if (associative)
            if (commutative)
              ACMatcher.tryMatch(this, as, usebinds, operator)
            else
              AMatcher.tryMatch(this, as, usebinds, operator)
          else
            if (commutative)
              CMatcher.tryMatch(this, as, usebinds)
            else
              SequenceMatcher.tryMatch(this, as, usebinds)
        }

        // Match properties.  This may alter the bindings.
        props.tryMatch(as.props, binds) match {
          case fail: Fail => Fail("Sequence properties do not match.",
          		this, subject, Some(fail))
          case Match(newbinds) => doMatchSequences(newbinds)
          case Many(iter) => Outcome.convert(iter ~> (doMatchSequences _),
              Fail("Sequence properties do not match.", this, subject))
        }
      case _ => Fail("An atom sequence may only match another atom sequence.",
          this, subject)
    }
  }
	
	//////////////////// GUI changes
  def rewrite(binds: Bindings): (AtomSeq, Boolean) = {
	// get the node representing this atom that is being rewritten
	val rwNode = RWTree.current.addChild("AtomSeq rewrite")
	
    // Rewrite the properties.
	val propsNode = rwNode.addChild("Properties: ").addChild(props)
	RWTree.current = propsNode
    val (newprop, pchanged) = props.rewrite(binds)
	
    // We must rewrite every child atom, and collect them into a new sequence.
	RWTree.current = rwNode.addChild("Atoms: ")
    val (newseq, schanged) = SequenceMatcher.rewrite(atoms, binds)
	
    // If anything changed, make a new sequence.
    if (pchanged || schanged) {
		RWTree.current = rwNode
		val newAS = new AtomSeq(newprop, newseq)
		rwNode.addChild(newAS)
		(newAS, true)
	}
    else (this, false)
  }
  //////////////////// end GUI changes

  def toParseString = atoms.mkParseString(props.toParseString + "(" , ", ", ")")

  /**
   * Provide a "naked" version of the sequence, without the parens and property
   * indicators.
   * 
   * @return	The elements of the sequence, separated by commas.  Items internal
   * 					to the sequence may themselves be lists; that is okay, since the
   * 					parse string is used for those atoms.
   */
  def toNakedString = atoms.mkParseString("", ", ", "")

  /**
   * Make a Scala-parseable version of this atom.
   */
  override def toString = "AtomSeq(" + props + ", " +
  		atoms.mkString("Vector(", ",", ")") + ")"
  
  override lazy val hashCode = atoms.hashCode

  /**
   * Two sequences are equal iff their properties and atoms are equal.
   */
  override def equals(other: Any) = other match {
    case AtomSeq(oprops, oatoms) if (oatoms == atoms && oprops == props) => true
    case _ => false
  }
}

/**
 * Simplified construction and matching for atom sequences.
 */
object AtomSeq {
  
  /** An empty atom sequence with no properties. */
  val EmptySeq = new AtomSeq(NoProps, IndexedSeq[BasicAtom]())
  
  /** Get an empty atom sequence with no properties. */
  def apply() = EmptySeq
  
  /**
   * Match an atom sequence's parts.
   * 
   * @param seq	The sequence.
   * @return	The properties and the atoms.
   */
  def unapply(seq: AtomSeq) = Some(seq.props, seq.atoms)
  
  /**
   * Make a new atom sequence with the given properties and children.
   * 
   * @param props	The properties.
   * @param atoms	The atoms.
   */
  def apply(props: AlgProp, seq: IndexedSeq[BasicAtom]) =
    new AtomSeq(props, seq)
  
  /**
   * Make a new atom sequence with the given properties and children.
   * 
   * @param props	The properties.
   * @param atoms	The atoms.
   */
  def apply(props: AlgProp, atoms: BasicAtom*) =
    new AtomSeq(props, atoms.toIndexedSeq[BasicAtom])
  
  /**
   * Process the atoms and build the new sequence.  This reduces any included
   * associative sequences, and incidentally makes sure the result is an
   * `OmitSeq`.
   * 
   * This method is used during instance construction.
   * 
   * @param props	The properties.
   * @param atoms	The atoms.
   * @return	The possibly-new sequence.
   */
  private def process(props: AlgProp,
      atoms: IndexedSeq[BasicAtom]): OmitSeq[BasicAtom] = {
    // If the list is associative, has an identity, or has an absorber, we
    // process it.  Idempotency is handled at the very end.
    val assoc = props.isA(false)
    val ident = props.identity.isDefined
    val absor = props.absorber.isDefined
    if (assoc || ident || absor) {
      var newseq = OmitSeq[BasicAtom]()
      for (atom <- atoms) {
        if (absor && props.absorber.get == atom) {
          // Found the absorber.  Nothing else to do.
          newseq = OmitSeq[BasicAtom]() :+ atom
          return newseq
        }
        if (!ident || props.identity.get != atom) {
          if (assoc) atom match {
            case AtomSeq(oprops, args) if props == oprops =>
              // Add the arguments directly to this list.  We can assume this
              // list has already been processed, so no deeper checking is
              // needed.
              newseq ++= args
            case _ =>
              // Add this atom to the list.
              newseq :+= atom
          }
        }
      } // Loop over atoms.
      if (props.isI(false)) newseq.distinct else newseq
    } else {
      // The list is fine as it is.
      if (props.isI(false)) atoms.distinct else atoms
    }
  }
}

/**
 * Improve matching of atom sequences as lists of atoms.
 * 
 * This is intended for use in matching.  The general form is:
 * {{{
 * args match {
 *   case Args(item1: Lambda, item2: Variable) => //...
 *   //...
 * }
 * }}}
 */
object Args {
  /**
   * Allow matching of an atom sequence as a sequence.
   * 
   * @param seq	The atom sequence.
   * @return	The children as a matchable sequence.
   */
  def unapplySeq(seq: AtomSeq) = Some(seq.atoms)
}
