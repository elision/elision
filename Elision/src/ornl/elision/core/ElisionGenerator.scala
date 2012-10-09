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
package ornl.elision.core

/**
 * Generate the Elision code to create an atom.
 * 
 * To use this simply pass an atom to the `apply` method along with an
 * optional appendable to get the result.
 * 
 * The result is assumed to be in an environment where necessary precursor
 * context contents have been declared.
 */
object ElisionGenerator {

  def gen(atom: Literal[_], buf: Appendable): Appendable = {
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
          apply(sl.typ, buf.append(": "))
      }
        
      // Process other literals.
      case IntegerLiteral(typ, value) =>
        buf.append(value.toString)
        if (typ != INTEGER || BasicAtom.printTypeInfo) {
          apply(typ, buf.append(": "))
        } else {
          buf
        }
      case StringLiteral(typ, value) =>
        buf.append(toEString(value))
        if (typ != STRING || BasicAtom.printTypeInfo) {
          apply(typ, buf.append(": "))
        } else {
          buf
        }
      case BooleanLiteral(typ, value) =>
        buf.append(value.toString)
        if (typ != BOOLEAN || BasicAtom.printTypeInfo) {
          apply(typ, buf.append(": "))
        } else {
          buf
        }
      case fl:FloatLiteral =>
        buf.append(fl.numberString)
        if (fl.theType != FLOAT || BasicAtom.printTypeInfo) {
          apply(fl.theType, buf.append(": "))
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
   * @return        The result.
   */
  def gen(prop: AlgProp, buf: Appendable): Appendable = {
    buf.append("%")
    prop.associative match {
      case Some(Literal.TRUE) => buf.append("A")
      case Some(Literal.FALSE) => buf.append("!A")
      case Some(atom) => apply(atom, buf.append("A[")).append("]")
      case _ =>
    }
    prop.commutative match {
      case Some(Literal.TRUE) => buf.append("C")
      case Some(Literal.FALSE) => buf.append("!C")
      case Some(atom) => apply(atom, buf.append("C[")).append("]")
      case _ =>
    }
    prop.idempotent match {
      case Some(Literal.TRUE) => buf.append("I")
      case Some(Literal.FALSE) => buf.append("!I")
      case Some(atom) => apply(atom, buf.append("I[")).append("]")
      case _ =>
    }
    prop.absorber match {
      case None =>
      case Some(atom) => apply(atom, buf.append("B[")).append("]")
    }
    prop.identity match {
      case None =>
      case Some(atom) => apply(atom, buf.append("D[")).append("]")
    }
    buf
  }
  
  /**
   * Generate the Scala code required to create a special form.  Certain
   * special forms get specialized processing.
   * 
   * @param atom    The atom.
   * @param buf     The buffer to get the result.
   * @return        The result.
   */
  def gen(atom: SpecialForm, buf: Appendable): Appendable = {
    apply(atom.tag, buf.append("{: ")).append(" ")
    apply(atom.content, buf).append(" :}")
  }
  
  /**
   * Generate the Scala code required to create a literal.  Certain well-known
   * root types are handled directly and simply.
   * 
   * @param atom    The atom.
   * @param buf     The buffer to get the result.  If not provided, one is
   *                created.
   * @return        The result.
   */
  def apply(atom: BasicAtom,
      buf: Appendable = new StringBuffer()): Appendable = {
    atom match {
      // Process literals.
      case lit: Literal[_] => gen(lit, buf)
      
      // Process algebraic properties.
      case ap: AlgProp => gen(ap, buf)
        
      // Process special forms.
      case sf: SpecialForm => gen(sf, buf)
      
      // Process specialized operators.
      case OperatorRef(operator) =>
        buf.append(toESymbol(operator.name)).append(": OPREF")
        
      // Process all atoms.
      case OpApply(op, args, _) =>
        buf.append(toESymbol(op.name)).append("(")
        var index = 0
        while (index < args.size) {
          if (index > 0) buf.append(", ")
          apply(args(index), buf)
          index += 1
        } // Add all arguments.
        buf.append(")")
        
      case Apply(lhs, rhs) =>
        buf.append("(")
        if (lhs.isInstanceOf[IntegerLiteral])
          apply(lhs, buf.append("(")).append(")")
        else if (lhs.isInstanceOf[NamedRootType])
          apply(lhs, buf).append(": ^TYPE")
        else
          apply(lhs, buf)
        apply(rhs, buf.append(".")).append(")")
        
      case AtomSeq(props, atoms) =>
        apply(props, buf)
        buf.append("(")
        var index = 0
        while (index < atoms.size) {
          if (index > 0) buf.append(", ")
          apply(atoms(index), buf)
          index += 1
        } // Add all atoms.
        buf.append(")")
        
      case BindingsAtom(binds) =>
        buf.append("{ binds ")
        binds foreach {
          pair =>
            apply(pair._2, buf.append(toESymbol(pair._1)).append(" -> ")).append(" ")
        }
        buf.append("}")
        
      case Lambda(lvar, body) =>
        apply(lvar, buf.append("\\"))
        apply(body, buf.append("."))
        
      case MapPair(left, right) =>
        apply(left, buf.append("("))
        apply(right, buf.append(" -> ")).append(")")
        
      case RulesetRef(name) =>
        buf.append(toESymbol(name)).append(": RSREF")
        
      case vari: Variable =>
        buf.append(vari.prefix).append(toESymbol(vari.name))
        if (vari.guard != Literal.TRUE) {
          apply(vari.guard, buf.append("{")).append("}")
        }
        if ((vari.theType != ANY) || BasicAtom.printTypeInfo) {
          apply(vari.theType, buf.append(": "))
        }
        vari.labels foreach {
          label =>
            buf.append(" @").append(toESymbol(label))
        }
    }
    buf
  }
}