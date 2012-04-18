/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package sjp.elision.core

import scala.collection.IndexedSeq
import sjp.elision.core.matcher._

/**
 * An atom sequence is just that: a sequence of atoms.
 * 
 * Atom sequences may have properties like operators.  That is, they may be
 * commutative, associative, and idempotent.  These properties have an effect
 * on the atom sequence when it is constructed.
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
  val atoms = AtomSeq.process(props, if (idempotent) xatoms.distinct else xatoms)
  
  /**
   * Map each constant in the sequence to its index.
   */
  val constantMap = scala.collection.mutable.HashMap[BasicAtom, Int]()
  for (i <- 0 until atoms.length)
    if (atoms(i).isConstant) constantMap(atoms(i)) = i
    
  /** The type of all sequences is the type universe. */
  val theType = TypeUniverse
  
  /** A sequence is constant iff all elements are constant. */
  val isConstant = atoms.forall(_.isConstant)
  
  /** This sequence is a term iff all elements are terms. */
  val isTerm = atoms.forall(_.isTerm)
  
  /** The constant pool is derived from the children of the sequence. */
  val constantPool = Some(BasicAtom.buildConstantPool(1, atoms:_*))
  
  /** The De Bruijn index is equal to the maximum index of the children. */
  val deBruijnIndex = atoms.foldLeft(0)(_ max _.deBruijnIndex)
  
  /** The depth is equal to the maximum depth of the child atoms, plus one. */
  val depth = atoms.foldLeft(0)(_ max _.depth) + 1
  
  /**
   * Get an element of this sequence by index.
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
   * operator.
   * 
   * @param subject	The atom to match.
   * @param binds		Any bindings to honor.
   * @param hints		An optional operator to use for associative matching.
   * @return	The match outcome.
   */
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]): Outcome = {
    // We only care if the hint is an operator.  We do this in two steps, since
    // the "obvious" way to do it doesn't work because of type erasure.  Boo!
    val operator = hints match {
      case Some(value) => value match {
        case oper: Operator => Some(oper)
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

  /**
   * Rewrite this atom sequence based on the provided bindings.  At present the
   * properties cannot be rewritten.
   * 
   * @param binds	The bindings.
   * @return	A pair consisting of an atom and a flag that is true iff the
   * 					rewrite "succeeded."
   */
  def rewrite(binds: Bindings) = {
    // We must rewrite every child atom, and collect them into a new sequence.
    val (newseq, changed) = SequenceMatcher.rewrite(atoms, binds)
    if (changed) (new AtomSeq(props, newseq), true) else (this, false)
  }

  /**
   * An atom sequence uses a percent sign followed by the properties
   * specification, followed by a comma-separated list of atoms in parentheses.
   * 
   * @return	The parseable string.
   */
  def toParseString = atoms.mkParseString(
      (if (props.isConstant) props.toShortString else props.toParseString) +
      "(" , ", ", ")")

  /**
   * Provide a "naked" version of the sequence, without the parens and property
   * indicators.
   * 
   * @return	The elements of the sequence, separated by commas.  Items internal
   * 					to the sequence may themselves be lists; that is okay, since the
   * 					parse string is used for those atoms.
   */
  def toNakedString = atoms.mkParseString("", ", ", "")
  
  override def toString = "AtomSeq(" + props + ", " +
  		atoms.mkString("Vector(", ",", ")") + ")"
  
  override lazy val hashCode = atoms.hashCode
  
  override def equals(other: Any) = other match {
    case AtomSeq(oprops, oatoms) if (oatoms == atoms && oprops == props) => true
    case _ => false
  }
}

/**
 * Simplified construction and matching
 */
object AtomSeq {
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
   * @param props	The properties.
   * @param atoms	The atoms.
   * @return	The possibly-new sequence.
   */
  private def process(props: AlgProp,
      atoms: IndexedSeq[BasicAtom]): OmitSeq[BasicAtom] = {
    if (props.isA(false)) {
      // The list is associative.  Flatten any included lists.
      var newseq = OmitSeq[BasicAtom]()
      for (atom <- atoms) {
        if (props.absorber.isDefined && props.absorber.get == atom) {
          newseq = OmitSeq[BasicAtom]()
          newseq :+= atom
          return newseq
        }
      	if (props.identity.isEmpty || props.identity.get != atom) {
	  		  atom match {
				    case AtomSeq(oprops, args) if props == oprops =>
				      // Add the arguments directly to this list.  We can assume it has
				      // already been processed, so no deeper checking is needed.
				      newseq ++= args
				    case _ =>
				    	// Add this item to the list.
				      newseq :+= atom
				  }
      	}
      } // Flatten lists.
      newseq
    } else {
      // The list is not associative.  Construct and return the list.
      atoms
    }
  }
}

/**
 * Improve matching of atom sequences as lists of atoms.
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