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
 * Generate the Scala code to create an atom.
 *
 */
object ScalaGenerator {
  def apply(atom: BasicAtom,
      context: Boolean,
      buf: Appendable = new StringBuffer()): Appendable = {
    atom match {
      // Handle known constants and types.
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
        buf.append("TypeUniverse")
      case NamedRootType(name) =>
        buf.append("NamedRootType(")
        buf.append(toEString(name))
        buf.append(")")
      // Process literals.
      case lit: Literal[_] => lit match {
        case IntegerLiteral(typ, value) =>
          buf.append("IntegerLiteral(")
          apply(typ, context, buf).append(",")
          buf.append(value.toString).append(")")
        case StringLiteral(typ, value) =>
          buf.append("StringLiteral(")
          apply(typ, context, buf).append(",")
          buf.append(toEString(value)).append(")")
        case SymbolLiteral(typ, value) =>
          buf.append("SymbolLiteral(")
          apply(typ, context, buf).append(",")
          buf.append(toESymbol(value.name)).append(")")
        case BooleanLiteral(typ, value) =>
          buf.append("BooleanLiteral(")
          apply(typ, context, buf).append(",")
          buf.append(value.toString).append(")")
        case FloatLiteral(typ, significand, exponent, radix) =>
          buf.append("FloatLiteral(")
          apply(typ, context, buf).append(",")
          buf.append(significand.toString).append(",")
          buf.append(exponent.toString).append(",")
          buf.append(radix.toString).append(")")
      }
      // Process very basic algebraic properties.
      case Absorber(atom) =>
        buf.append("Absorber(")
        apply(atom, context, buf).append(")")
      case Associative(atom) =>
        buf.append("Associative(")
        apply(atom, context, buf).append(")")
      case Commutative(atom) =>
        buf.append("Commutative(")
        apply(atom, context, buf).append(")")
      case Idempotent(atom) =>
        buf.append("Idempotent(")
        apply(atom, context, buf).append(")")
      case Identity(atom) =>
        buf.append("Identity(")
        apply(atom, context, buf).append(")")
      case NoProps =>
        buf.append("NoProps")
      // Process specialized operators.
      case CaseOperator(name, typ, cases, description, detail) =>
        buf.append("CaseOperator(")
        buf.append(toEString(name)).append(",")
        apply(typ, context, buf).append(",")
        apply(cases, context, buf).append(",")
        buf.append(toEString(description)).append(",")
        buf.append(toEString(detail)).append(")")
      case TypedSymbolicOperator(name, typ, params, description, detail) =>
        buf.append("CaseOperator(")
        buf.append(toEString(name)).append(",")
        apply(typ, context, buf).append(",")
        apply(params, context, buf).append(",")
        buf.append(toEString(description)).append(",")
        buf.append(toEString(detail)).append(")")
      case OperatorRef(operator) =>
        if (context) {
          // We assume a context exists, and use it.
          buf.append("_context.operatorLibrary(")
          buf.append(toEString(operator.name))
          buf.append(")")
        } else {
          // No context is available.
          buf.append("OperatorRef(")
          apply(operator, context, buf).append(")")
        }
      // Process all atoms.
      case AlgProp(asso, comm, idem, abso, iden) =>
        buf.append("AlgProp(")
        option(asso, context, buf).append(",")
        option(comm, context, buf).append(",")
        option(idem, context, buf).append(",")
        option(abso, context, buf).append(",")
        option(iden, context, buf).append(")")
      case Apply(lhs, rhs) =>
        buf.append("Apply(")
        apply(lhs, context, buf).append(",")
        apply(rhs, context, buf).append(")")
      case AtomSeq(props, atoms) =>
        buf.append("AtomSeq(")
        apply(props, context, buf)
        atoms foreach {
          apply(_, context, buf.append(","))
        }
        buf.append(")")
      case BindingsAtom(binds) =>
        buf.append("Bindings(")
        binds foreach { pair =>
          apply(pair._1, context, buf.append(",")).append(" -> ")
          apply(pair._2, context, buf)
        }
        buf.append(")")
      case Lambda(lvar, body) =>
        buf.append("Lambda(")
        apply(lvar, context, buf).append(",")
        apply(body, context, buf).append(")")
      case MapPair(left, right) =>
        buf.append("MapPair(")
        apply(left, context, buf).append(",")
        apply(right, context, buf).append(")")
      case RewriteRule(pat, rew, gua, rs, syn) =>
        buf.append("RewriteRule(")
        apply(pat, context, buf).append(",")
        apply(rew, context, buf).append(",")
        buf.append("Seq(")
        var tail = false
        gua foreach { guard =>
          if (tail) buf.append(",")
          apply(guard, context, buf)
          tail = true
        }
        buf.append("),")
        buf.append(rs.map(toEString(_)).toString).append(",")
        buf.append(syn.toString).append(")")
      case RulesetRef(name) =>
        buf.append("RulesetRef(")
        buf.append("_context,")
        buf.append(toEString(name))
        buf.append(")")
      case SpecialForm(tag, content) =>
        buf.append("SpecialForm(")
        apply(tag, context, buf).append(",")
        apply(content, context, buf).append(")")
      case MetaVariable(typ, name, guard, labels) =>
        buf.append("Variable(")
        apply(typ, context, buf).append(",")
        buf.append(toEString(name)).append(",")
        apply(guard, context, buf).append(",")
        buf.append(labels.map(toEString(_)).mkString("Set(", ",", ")"))
      case Variable(typ, name, guard, labels) =>
        buf.append("Variable(")
        apply(typ, context, buf).append(",")
        buf.append(toEString(name)).append(",")
        apply(guard, context, buf).append(",")
        buf.append(labels.map(toEString(_)).mkString("Set(", ",", ")"))
    }
    buf
  }
  
  /**
   * Add an optional atom to the provided appendable.
   * 
   * @param opt   The optional atom.
   * @param buf   The buffer to get the string.
   * @return  The buffer.
   */
  def option(opt: Option[BasicAtom], context: Boolean,
      buf: Appendable) = opt match {
    case None => buf.append("None")
    case Some(atom) => apply(atom, context, buf.append("Some(")).append(")")
  }
}