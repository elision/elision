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

  def fcmp(atom1: BasicAtom, atom2: BasicAtom) = {
    if (((atom1.hashCode compare atom2.hashCode) == 0) &&
        ((atom1.otherHashCode compare atom2.otherHashCode) == 0)) {
        0
    }
    else {
        atom1.hashCode compare atom2.hashCode
    }
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

   return fcmp(left, right)

   // Too slow.
   /*
    // First check the ordinals.
    val lo = getOrdinal(left)
    var sgn = (lo - getOrdinal(right)).signum
    if (sgn != 0) return sgn
    
    // Watch for root types!
    if (left == TypeUniverse) {
      if (right == TypeUniverse) return 0
      else return 1
    } else if (right == TypeUniverse) return -1
    
    // Test for fast equality.  This explicitly breaks a potential unbounded
    // recursion caused by the LIST(x) operator.  The problems looks like this
    // (for reference): LIST(x) => LIST:OPREF . %(x), but the argument is also
    // a LIST(x), so we have an unbounded recursion.
    if (feq(left, right, false)) {
      return 0
    }
    
    // Ordinals did not solve the problem; the two atoms have the same ordinal.
    // Try to order the atoms by their types.  Watch for recursion!
    if (!(left.theType eq right.theType)) {
      if (!(left.theType eq left) && !(right.theType eq right)) {
        sgn = compare(left.theType, right.theType)
        if (sgn != 0) return sgn
      }
    }
    
    // The types did not resolve anything.  We have to try more specific
    // tests.
    lo match {
      case 0 =>
        // Compare literals by their values.  We already know the two literals
        // have the same type... but might be different classes!
        return left match {
          case llit: IntegerLiteral => right match {
            case rlit: IntegerLiteral => llit.value.compare(rlit.value)
            case _ => -1
          }
          case llit: BitStringLiteral => right match {
            case rlit: BitStringLiteral =>
              val (l1, l2) = llit.value
              val (r1, r2) = rlit.value
              sgn = l1 compare r1
              if (sgn != 0) return sgn
              sgn = l2 compare r2
              return l2 compare r2
          }
          case llit: BooleanLiteral => right match {
            case rlit: BooleanLiteral => llit.value.compare(rlit.value)
            case _ => -1
          }
          case llit: StringLiteral => right match {
            case rlit: IntegerLiteral => 1
            case rlit: StringLiteral => llit.value.compare(rlit.value)
            case _ => -1
          }
          case llit: SymbolLiteral => right match {
            case rlit: IntegerLiteral => 1
            case rlit: StringLiteral => 1
            case rlit: SymbolLiteral => llit.value.name.compare(rlit.value.name)
            case _ => -1
          }
          case llit: FloatLiteral => right match {
            case rlit: IntegerLiteral => 1
            case rlit: StringLiteral => 1
            case rlit: SymbolLiteral => 1
            case rlit: FloatLiteral =>
              val (l1,l2,l3) = llit.value
              val (r1,r2,r3) = rlit.value
              sgn = l1 compare r1
              if (sgn != 0) return sgn
              sgn = l2 compare r2
              if (sgn != 0) return sgn
              return l3 compare r3
          }
        }
        
      case 1 =>
        // Comparing algebraic properties.  We just compare them piece by
        // piece.
        val lap = left.asInstanceOf[AlgProp]
        val rap = right.asInstanceOf[AlgProp]
        sgn = compare(lap.absorber, rap.absorber)
        if (sgn != 0) return sgn
        sgn = compare(lap.associative, rap.associative)
        if (sgn != 0) return sgn
        sgn = compare(lap.commutative, rap.commutative)
        if (sgn != 0) return sgn
        sgn = compare(lap.idempotent, rap.idempotent)
        if (sgn != 0) return sgn
        return compare(lap.identity, rap.identity)
        
      case 2 =>
        // Compare metavariables.  We just order them by name and ignore all else.
        return left.asInstanceOf[MetaVariable].name compare right.asInstanceOf[MetaVariable].name
        
      case 3 =>
        // Compare variables.  We just order them by name and ignore all else.
        return left.asInstanceOf[Variable].name compare right.asInstanceOf[Variable].name
        
      case 4 =>
        // Compare two applies.  Compare the operators and then the arguments.
        val lap = left.asInstanceOf[Apply]
        val rap = right.asInstanceOf[Apply]
        sgn = compare(lap.op, rap.op)
        if (sgn != 0) return sgn
        return compare(lap.arg, rap.arg)
        
      case 5 =>
        // Compare two atom sequences.  We order by algebraic properties, then
        // by length, and finally by order of the items.
        val las = left.asInstanceOf[AtomSeq]
        val ras = right.asInstanceOf[AtomSeq]
        sgn = compare(las.props, ras.props)
        if (sgn != 0) return sgn
        sgn = (las.length - ras.length).signum
        if (sgn != 0) return sgn
        // Now we have to order by the elements.  We already know the sequences
        // are the same length.  Walk them and try to find some case where they
        // are distinct.
        var index = 0
        for (index <- 0 until las.length) {
          sgn = compare(las(index), ras(index))
          if (sgn != 0) return sgn
        } // Compare all elements of the sequences.
        return 0
        
      case 6 =>
        // Compare two bindings atoms.  Ordering bindings atoms is tricky.
        // First try ordering them my number of bindings.
        val lbinds = left.asInstanceOf[BindingsAtom].mybinds
        val rbinds = right.asInstanceOf[BindingsAtom].mybinds
        sgn = (lbinds.size - rbinds.size).signum
        if (sgn != 0) return sgn
        // Now we have to actually compare the bindings.  To do this we
        // merge the key sets and then order the result.  Then we compare
        // the values for every key.
        val keyset = lbinds.keySet.union(rbinds.keySet).toIndexedSeq.sorted
        for (key <- keyset) {
          sgn = compare(lbinds.get(key), rbinds.get(key))
          if (sgn != 0) return sgn
        } // Compare all elements.
        return 0
        
      case 7 =>
        // Compare two lambdas.  We order them by body only, since the
        // DeBruijn indices replace the parameters.
        return compare(left.asInstanceOf[Lambda].body,
            right.asInstanceOf[Lambda].body)
            
      case 8 =>
        // Compare two map pairs.  We just compare the elements.
        val lmp = left.asInstanceOf[MapPair]
        val rmp = right.asInstanceOf[MapPair]
        sgn = compare(lmp.left, rmp.left)
        if (sgn != 0) return sgn
        return compare(lmp.right, rmp.right)
        
      case 9 =>
        // Compare two match atoms.
        return compare(left.asInstanceOf[MatchAtom].pattern,
            right.asInstanceOf[MatchAtom].pattern)
            
      case 10 =>
        // Compare two special forms.
        val lsf = left.asInstanceOf[SpecialForm]
        val rsf = right.asInstanceOf[SpecialForm]
        sgn = compare(lsf.tag, rsf.tag)
        if (sgn != 0) return sgn
        return compare(lsf.content, rsf.content)
        
      case 11 =>
        // Comparing two ruleset references.  They sort by name.
        return left.asInstanceOf[RulesetRef].name.compare(
            right.asInstanceOf[RulesetRef].name)
        
      case 12 =>
        // Comparing two operator references.  They sort by name.
        return left.asInstanceOf[OperatorRef].name.compare(
            right.asInstanceOf[OperatorRef].name)
        
      case _ =>
        // Something annoying has happened.
        throw new ornl.elision.util.ElisionException(left.loc,
            "ERROR: No sort order defined for: " + left.toParseString)
    }
    
    // If we get here, something is wrong.  Bail out.
    */
  }
}
