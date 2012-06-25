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

/**
 * Generate the Scala code to create an atom.
 * 
 * To use this, simply pass an atom to the `apply` method along with an
 * optional appendable to get the result.
 * 
 * The result is assumed to be in an environment where
 * `import ornl.elision.core._` has been executed.
 */
object ScalaGenerator {
  
  /**
   * Generate the Scala code required to create a literal.  Certain well-known
   * root types are handled directly and simply.
   * 
   * @param atom    The atom.
   * @param context If true, a context named `_context` is available.  If false,
   *                it is not, and any context must be generated in place.
   * @param buf     The buffer to get the result.
   * @return        The result.
   */
  def gen(atom: Literal[_],
      context: Boolean,
      buf: Appendable): Appendable = {
    atom match {
      case sl: SymbolLiteral => sl match {
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
          
        // Handle anything else.
        case _ =>
          buf.append("SymbolLiteral(")
          apply(sl.typ, context, buf).append(",")
          buf.append("Symbol("+toEString(sl.value.name)+")").append(")")
      }
        
      // Process other literals.
      case IntegerLiteral(typ, value) =>
        buf.append("IntegerLiteral(")
        apply(typ, context, buf).append(",")
        buf.append(value.toString).append(")")
      case StringLiteral(typ, value) =>
        buf.append("StringLiteral(")
        apply(typ, context, buf).append(",")
        buf.append(toEString(value)).append(")")
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
  }
  
  /**
   * Generate the Scala code required to create an `AlgProp`.  Certain
   * well-known properties are handled directly and simply.
   * 
   * @param atom    The atom.
   * @param context If true, a context named `_context` is available.  If false,
   *                it is not, and any context must be generated in place.
   * @param buf     The buffer to get the result.
   * @return        The result.
   */
  def gen(atom: AlgProp,
      context: Boolean,
      buf: Appendable): Appendable = {
    atom match {
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
      case AlgProp(asso, comm, idem, abso, iden) =>
        buf.append("AlgProp(")
        option(asso, context, buf).append(",")
        option(comm, context, buf).append(",")
        option(idem, context, buf).append(",")
        option(abso, context, buf).append(",")
        option(iden, context, buf).append(")")        
    }
  }
  
  /**
   * Generate the Scala code required to create a special form.  Certain
   * special forms get specialized processing.
   * 
   * @param atom    The atom.
   * @param context If true, a context named `_context` is available.  If false,
   *                it is not, and any context must be generated in place.
   * @param buf     The buffer to get the result.
   * @return        The result.
   */
  def gen(atom: SpecialForm,
      context: Boolean,
      buf: Appendable): Appendable = {
    atom match {
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
        // TODO Handlers are not handled!
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
      case SpecialForm(tag, content) =>
        buf.append("SpecialForm(")
        apply(tag, context, buf).append(",")
        apply(content, context, buf).append(")")
    }
  }
  
  /**
   * Generate the Scala code required to create a literal.  Certain well-known
   * root types are handled directly and simply.
   * 
   * @param atom    The atom.
   * @param context If true, a context named `_context` is available.  If false,
   *                it is not, and any context must be generated in place.
   * @param buf     The buffer to get the result.  If not provided, one is
   *                created.
   * @return        The result.
   */
  def apply(atom: BasicAtom,
      context: Boolean,
      buf: Appendable = new StringBuffer()): Appendable = {
    atom match {
      // Process literals.
      case lit: Literal[_] => gen(lit, context, buf)
      
      // Process algebraic properties.
      case ap: AlgProp => gen(ap, context, buf)
        
      // Process special forms.
      case sf: SpecialForm => gen(sf, context, buf)
      
      // Process specialized operators.
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
        var tail = false
        binds foreach { pair =>
          if (tail) buf.append(",")
          buf.append(toEString(pair._1)).append(" -> ")
          apply(pair._2, context, buf)
          tail = true
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
        
      case RulesetRef(name) =>
        buf.append("RulesetRef(")
        buf.append("_context,")
        buf.append(toEString(name))
        buf.append(")")
        
      case mvari: MetaVariable =>
        buf.append("MetaVariable(")
        apply(mvari.theType, context, buf).append(",")
        buf.append(toEString(mvari.name)).append(",")
        apply(mvari.guard, context, buf).append(",")
        buf.append(mvari.labels.map(toEString(_)).mkString("Set(", ",", ")"))
        buf.append(")")
        
      case vari: Variable =>
        buf.append("Variable(")
        apply(vari.theType, context, buf).append(",")
        buf.append(toEString(vari.name)).append(",")
        apply(vari.guard, context, buf).append(",")
        buf.append(vari.labels.map(toEString(_)).mkString("Set(", ",", ")"))
        buf.append(")")
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
