/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2013 by Stacy Prowell (sprowell@gmail.com).
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
package ornl.elision.core

import ornl.elision.util.PropertyManager

/**
 * Compare basic atoms.
 * 
 * This object orders atoms in a defined way with one exception: variables are
 * ordered by name only, since variables with the same name are considered
 * equal.
 */
object BasicAtomComparator extends Ordering[BasicAtom] {
  
  /**
   * Get the ordinal for an atom.
   * 
   * @param atom  The atom.
   * @return  The ordinal.
   */
  private def getOrdinal(atom: BasicAtom) = atom match {
    case x: Literal[_] => 0
    case x: AlgProp => 1
    case x: MetaVariable => 2
    case x: Variable => 3
    case x: Apply => 4
    case x: AtomSeq => 5
    case x: BindingsAtom => 6
    case x: Lambda => 7
    case x: MapPair => 8
    case x: MatchAtom => 9
    case x: SpecialForm => 10
    case x: RulesetRef => 11
    case x: OperatorRef => 12
    case _ => -1
  }
  
  /**
   * Compare two atoms.
   * 
   * The comparison works as follows.  Atoms are first mapped to ordinals and
   * these are compared.  If the atoms have the same ordinal then a more
   * restrictive test is needed.  First we compare the types of the atoms.
   * Next we compare the parts of the atoms.
   * 
   * @param left  First atom.
   * @param right Second atom.
   * @return  -1 if left < right, 0 if left = right, and 1 if left > right.
   */
  def apply(left: BasicAtom, right: BasicAtom) = compare(left, right)

    /**
   * Whether to compute equality faster but in a riskier fashion.
   */
  var _riskyEqual: Boolean = false;

  /**
   * Whether to use custom equality functions.
   */
  var _customEqual: Boolean = false;
  
  /** 
   * Declare the Elision property for setting whether to do risky
   * equality checking. 
   */
  knownExecutor.declareProperty("risky_equality_check",
      "Whether to do fast, but risky, equality checking of atoms.",
      _riskyEqual,
      (pm: PropertyManager) => {
        _riskyEqual =
          pm.getProperty[Boolean]("risky_equality_check").asInstanceOf[Boolean]
      })
      
  /** 
   * Declare the Elision property for setting whether to do custom
   * equality checking. 
   */
  knownExecutor.declareProperty("custom_equality_check",
      "Whether to use custom equality functions to check atoms.",
      _customEqual,
      (pm: PropertyManager) => {
        _customEqual =
          pm.getProperty[Boolean]("custom_equality_check").asInstanceOf[Boolean]
      })


  /**
   * Perform "fast equality checking" on two atoms.  This performs basic
   * structural comparson of the atoms.  If this cannot prove that the two
   * atoms are either equal to unequal, then the closure `other` is invoked
   * to resolve.
   * 
   * @param atom1   The first atom.
   * @param atom2   The second atom.
   * @param other   Other checking to perform, if the fast check is
   *                indeterminate.
   * @return  True if equal, false if not.
   */
  def feq(atom1: BasicAtom, atom2: BasicAtom, other: => Boolean = false) = {
    (atom1 eq atom2) || 
    (
      (atom1.isConstant == atom2.isConstant) &&
      (atom1.hashCode == atom2.hashCode) &&
      (if (_riskyEqual) true else
        (atom1.depth == atom2.depth) &&
        (atom1.isTerm == atom2.isTerm) &&
        (atom1.otherHashCode == atom2.otherHashCode)
      ) && (if(_customEqual) other else true)
    )
  }

  /**
   * Perform a comparison of optional atoms.  None is less than Some.
   * 
   * @param left  Left atom.
   * @param right Right atom.
   * @return  -1 if left < right, 0 if left = right, and 1 if left > right.
   */
  def compare(left: Option[BasicAtom], right: Option[BasicAtom]): Int = {
    left match {
      case None => right match {
        case None => 0
        case Some(_) => -1
      }
      case Some(latom) => right match {
        case None => 1
        case Some(ratom) => compare(latom, ratom)
      }
    }
  }
  
  /**
   * Compare two atoms.
   * 
   * The comparison works as follows.  Atoms are first mapped to ordinals and
   * these are compared.  If the atoms have the same ordinal then a more
   * restrictive test is needed.  First we compare the types of the atoms.
   * Next we compare the parts of the atoms.
   * 
   * @param left  First atom.
   * @param right Second atom.
   * @return  -1 if left < right, 0 if left = right, and 1 if left > right.
   */
  def compare(left: BasicAtom, right: BasicAtom): Int = {

    // Compare the atoms and store the result for possible later use.
    // This explicitly breaks a potential unbounded
    // recursion caused by the LIST(x) operator.  The problems looks like this
    // (for reference): LIST(x) => LIST:OPREF . %(x), but the argument is also
    // a LIST(x), so we have an unbounded recursion.
    if (feq(left, right)) return 0
    
    // Check the ordinals.
    val lo = getOrdinal(left)
    var sgn = (getOrdinal(right) - lo).signum
    if (sgn != 0) return sgn

    // Watch for root types!
    if (left == TypeUniverse) {
      if (right == TypeUniverse) return 0
      else return 1
    } else if (right == TypeUniverse) return -1
    
    //If ordinals and root types don't get us anywhere, use the atom types
    if (!(left.theType eq right.theType)) {
      if (!(left.theType eq left) && !(right.theType eq right)) {
        sgn = compare(left.theType, right.theType)
        if (sgn != 0) return sgn
      }
    }
    left.otherHashCode compare right.otherHashCode
  }
}
