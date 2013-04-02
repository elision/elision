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
package ornl.elision.generators

import ornl.elision.core._

/**
 * Generate the Elision code to create an atom.
 * 
 * To use this simply pass an atom to the `apply` method along with an
 * optional appendable to get the result.
 * 
 * The result is assumed to be in an environment where necessary precursor
 * context contents have been declared.
 */
object ElisionGenerator extends Generator {

  /**
   * Generate the Elision-parseable string for a literal.
   * 
   * @param atom    The literal atom.
   * @param buf     A buffer to get the string.
   * @param limit   The current nesting limit of this atom.
   * @return  The appendable, for chaining.
   */
  private def _gen(atom: Literal[_], buf: Appendable, limit: Int): Appendable = {
    atom match {
      case sl: SymbolLiteral => sl match {
        case ANY => buf.append("ANY")
        case BINDING => buf.append("BINDING")
        case BOOLEAN => buf.append("BOOLEAN")
        case FLOAT => buf.append("FLOAT")
        case INTEGER => buf.append("INTEGER")
        case NONE => buf.append("NONE")
        case OPREF => buf.append("OPREF")
        case RSREF => buf.append("RSREF")
        case RULETYPE => buf.append("RULETYPE")
        case STRATEGY => buf.append("STRATEGY")
        case STRING => buf.append("STRING")
        case SYMBOL => buf.append("SYMBOL")
        
        // Handle the type universe and named root types.
        case TypeUniverse =>
          buf.append("^TYPE")
        case NamedRootType(name) =>
          buf.append(toESymbol(name))
          
        // Handle anything else.
        case _ =>
          buf.append(toESymbol(sl.value.name))
          apply(sl.typ, buf.append(":"), limit)
      }
        
      // Process other literals.
      case IntegerLiteral(typ, value) =>
        buf.append(value.toString)
        if (typ != INTEGER || BasicAtom.printTypeInfo) {
          apply(typ, buf.append(":"), limit)
        } else {
          buf
        }
      case StringLiteral(typ, value) =>
        buf.append(toEString(value))
        if (typ != STRING || BasicAtom.printTypeInfo) {
          apply(typ, buf.append(":"), limit)
        } else {
          buf
        }
      case BooleanLiteral(typ, value) =>
        buf.append(value.toString)
        if (typ != BOOLEAN || BasicAtom.printTypeInfo) {
          apply(typ, buf.append(":"), limit)
        } else {
          buf
        }
      case fl:FloatLiteral =>
        buf.append(fl.numberString)
        if (fl.theType != FLOAT || BasicAtom.printTypeInfo) {
          apply(fl.theType, buf.append(":"), limit)
        } else {
          buf
        }
    }
  }
  
  /**
   * Generate the Scala code required to create an `AlgProp`.  Certain
   * well-known properties are handled directly and simply.
   * 
   * @param atom    The atom.
   * @param buf     The buffer to get the result.
   * @param limit   The current nesting limit of this atom.
   * @return        The result.
   */
  private def _gen(prop: AlgProp, buf: Appendable, limit: Int): Appendable = {
    buf.append("%")
    prop.associative match {
      case Some(Literal.TRUE) => buf.append("A")
      case Some(Literal.FALSE) => buf.append("!A")
      case Some(atom) => apply(atom, buf.append("A["), limit-1).append("]")
      case _ =>
    }
    prop.commutative match {
      case Some(Literal.TRUE) => buf.append("C")
      case Some(Literal.FALSE) => buf.append("!C")
      case Some(atom) => apply(atom, buf.append("C["), limit-1).append("]")
      case _ =>
    }
    prop.idempotent match {
      case Some(Literal.TRUE) => buf.append("I")
      case Some(Literal.FALSE) => buf.append("!I")
      case Some(atom) => apply(atom, buf.append("I["), limit-1).append("]")
      case _ =>
    }
    prop.absorber match {
      case None =>
      case Some(atom) => apply(atom, buf.append("B["), limit-1).append("]")
    }
    prop.identity match {
      case None =>
      case Some(atom) => apply(atom, buf.append("D["), limit-1).append("]")
    }
    buf
  }
  
  /**
   * Generate the Scala code required to create a special form.  Certain
   * special forms get specialized processing.
   * 
   * @param atom    The atom.
   * @param buf     The buffer to get the result.
   * @param limit   The current nesting limit of this atom.
   * @return        The result.
   */
  private def _gen(atom: SpecialForm, buf: Appendable, limit: Int): Appendable = {
    if (atom.tag.isInstanceOf[SymbolLiteral] && atom.content.isInstanceOf[BindingsAtom]) {
      // Use the expanded form for this special form.
      val kind = atom.tag.asInstanceOf[SymbolLiteral].value.name
      buf.append("{").append(toESymbol(kind))
      for (pair <- atom.content.asInstanceOf[BindingsAtom]) {
        buf.append(" #").append(toESymbol(pair._1))
        apply(pair._2, buf.append("="), limit-1)
      } // Write all bindings.
      buf.append("}")
    } else {
      // Use the pair form.
      apply(atom.tag, buf.append("{:"), limit-1).append(" ")
      apply(atom.content, buf, limit-1).append(":}")
    }
  }
  
  /**
   * Generate the Scala code required to create an atom.  Certain well-known
   * root types are handled directly and simply.
   * 
   * @param atom    The atom.
   * @param buf     The buffer to get the result.  If not provided, one is
   *                created.
   * @param limit   The nesting limit of this atom.  If the limit is zero, then
   *                an ellipsis is printed instead of the atom.  Otherwise
   *                the limit is decreased for each parenthesized and bracketed
   *                pair, until zero is reached.  If the limit is negative,
   *                then there is effectively no limit.
   * @return        The result.
   */
  def apply(atom: BasicAtom,
      buf: Appendable = new StringBuffer(), limit: Int = -1): Appendable = {
    if (limit == 0) return buf.append("...")
    atom match {
      // Process literals.
      case lit: Literal[_] => _gen(lit, buf, limit)
      
      // Process algebraic properties.
      case ap: AlgProp => _gen(ap, buf, limit)
        
      // Process special forms.
      case sf: SpecialForm => _gen(sf, buf, limit)
      
      // Process specialized operators.
      case OperatorRef(operator) =>
        buf.append(toESymbol(operator.name)).append(":OPREF")
        
      // Process all atoms.
      case OpApply(op, args, _) =>
        // There are two possibilities.  The first is that the operator
        // used here is a known operator in the operator library.  If so,
        // then we can just use the name.  The second is that the operator
        // used here is not stored in the library, or is different from the
        // one used in the library.  In this latter case we must include
        // the entire operator definition.
        val kop = knownExecutor.context.operatorLibrary.get(op.name)
        if (kop.isDefined && kop.get == op) {
          // This is a well-known operator.  We can simply use the name.
          buf.append(toESymbol(op.name))
          buf.append("(")
        } else {
          // This is not a well-known operator.  We must write out the
          // definition in full.
          apply(op.operator, buf, limit-1).append(".%(")
        }
        // If the limit will be exceeded by the argument list, don't print
        // several elipses separated by commas, but just one for the list.
        if (limit == 1) {
          buf.append("...")
        } else {

          // Sort the atom sequence if it is commutative.
          var printAtoms = args.atoms
          if (args.props.isC(false)) {
            printAtoms = 
              args.atoms.sortWith( (x : BasicAtom, y : BasicAtom) =>
                ((x.otherHashCode != y.otherHashCode) && 
                 (x.otherHashCode < y.otherHashCode)) ||
                ((x.otherHashCode == y.otherHashCode) && 
                 (x.hashCode < y.hashCode)))
          }
          var index = 0
          while (index < printAtoms.size) {
            if (index > 0) buf.append(",")
            apply(printAtoms(index), buf, limit-1)
            index += 1
          } // Add all atoms.
        }
        buf.append(")")
        
      case Apply(lhs, rhs) =>
        buf.append("(")
        if (lhs.isInstanceOf[IntegerLiteral])
          apply(lhs, buf.append("("), limit).append(")")
        else if (lhs.isInstanceOf[NamedRootType])
          apply(lhs, buf, limit).append(": ^TYPE")
        else
          apply(lhs, buf, limit)
        apply(rhs, buf.append(".")).append(")")
        
      case AtomSeq(props, atoms) =>
        apply(props, buf, limit)
        buf.append("(")
        // If the limit will be exceeded by the argument list, don't print
        // several elipses separated by commas, but just one for the list.
        if (limit == 1) {
          buf.append("...")
        } else {
          var index = 0
          while (index < atoms.size) {
            if (index > 0) buf.append(",")
            apply(atoms(index), buf, limit-1)
            index += 1
          } // Add all atoms.
        }
        buf.append(")")
        
      case BindingsAtom(binds) =>
        buf.append("{ binds ")
        // If the limit will be exceeded by the bindings list, don't print
        // several elipses separated by spaces, but just one for the list.
        if (limit == 1) {
          buf.append("...")
        } else {
          binds foreach {
            pair =>
              apply(pair._2, buf.append(toESymbol(pair._1)).append(" -> "),
                  limit-1).append(" ")
          } // Add each individual bind pair.
        }
        buf.append("}")
        
      case Lambda(lvar, body) =>
        apply(lvar, buf.append("\\"), limit)
        apply(body, buf.append("."), limit)
        
      case MapPair(left, right) =>
        // If the limit will be exceeded by the map pair, don't print
        // two elipses separated by the arrow, but just one for the pair.
        if (limit == 1) {
          buf.append("(...)")
        } else {
          apply(left, buf.append("("), limit-1)
          apply(right, buf.append(" -> "), limit-1).append(")")
        }
        
      case RulesetRef(name) =>
        buf.append(toESymbol(name)).append(":RSREF")
        
      case vari: Variable =>
        buf.append(vari.prefix)
        buf.append(if (vari.byName) toEString(vari.name)
            else toESymbol(vari.name))
        if (vari.guard != Literal.TRUE) {
          apply(vari.guard, buf.append("{"), limit-1).append("}")
        }
        if ((vari.theType != ANY) || BasicAtom.printTypeInfo) {
          apply(vari.theType, buf.append(":"), limit-1)
        }
        vari.labels foreach {
          label =>
            buf.append("@").append(toESymbol(label))
        }
    }
    buf
  }
}
