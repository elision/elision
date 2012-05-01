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

import scala.collection.mutable.ListBuffer

/**
 * The common root for all application atoms.
 * 
 * An ''apply'' takes two atoms and applies the first (as an operator,
 * rewriter, etc.) to the second (as the argument).  Elements common across
 * all types are found here.
 * 
 * @param op		The left-hand element of the apply (operator).
 * @param arg		The right-hand element of the apply (argument).
 */
abstract class Apply(val op: BasicAtom, val arg: BasicAtom) extends BasicAtom {
  val isConstant = op.isConstant && arg.isConstant
  val isTerm = op.isTerm && arg.isTerm
  val constantPool = Some(BasicAtom.buildConstantPool(2, op, arg))
  val depth = (op.depth max arg.depth) + 1
  val deBruijnIndex = op.deBruijnIndex max arg.deBruijnIndex
  
  /** The hash code for this apply. */
  override lazy val hashCode = op.hashCode * 31 + arg.hashCode
  
  override def equals(other: Any) = other match {
    case Apply(oop, oarg) => oop == op && oarg == arg
    case _ => false
  }
  
  def rewrite(binds: Bindings) = {
    val (nop, nof) = op.rewrite(binds)
    val (narg, naf) = arg.rewrite(binds)
    if (nof || naf) (Apply(nop, narg), true) else (this, false)
  }
  
  /**
   * By default applications match iff their parts match.  The trick here is
   * that the argument lists have to know what the top-level operator is in
   * order to successfully associatively match.
   * 
   * Hints passed in are ignored, and the operator is passed along as the hint.
   */
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) =
    // Only applies match other applies.
    subject match {
      case Apply(oop, oarg) => {
        // Try to match the operators, and then the arguments.  If both match,
        // then this matches.  If not, then this does not match.
        op.tryMatch(oop, binds, Some(op)) match {
          case fail: Fail =>
            Fail("Operators do not match.", this, subject, Some(fail))
          case Match(newbinds) =>
            // The operators match.  Now try to match the arguments.
            arg match {
              case as:AtomSeq => as.tryMatch(oarg, newbinds, Some(op))
              case _ => arg.tryMatch(oarg, newbinds, Some(op))
            }
            
          case Many(matches) =>
            // The operators match in multiple ways.  This seems unlikely, but
            // we consider it here anyway.
            Many(MatchIterator(arg.tryMatch(oarg, _, Some(op)), matches))
        }
      }
      case _ => Fail("Applications only match other applications.",
          this, subject)
    }
}

/**
 * Provide construction and extraction for an `Apply`.
 */
object Apply {
  /**
   * Extract the components of an apply and return them.
   * 
   * @param apply	The apply.
   * @return	A pair consisting of the operator and argument, in order.
   */
  def unapply(apply: Apply) = Some(apply.op, apply.arg)
  
  /**
   * Construct an application.  We key off the left hand side (the operator)
   * to decide how to handle this.
   * 
   * @param op			The lhs of the apply, typically an operator.
   * @param arg			The rhs of the apply, typically an argument.
   * @param bypass	If true, bypass native operator handler invocations.
   * @return	The basic atom resulting from the application.
   */
  def apply(op: BasicAtom, arg: BasicAtom, bypass: Boolean = false): BasicAtom = {
    // Do not try to compute if metaterms are present.
    if (!arg.isTerm) SimpleApply(op, arg)
    else {
	    op match {
	      case oper: SymbolicOperator =>
	        // The lhs is an operator.  The rhs must be an atom sequence.
	        arg match {
	          case as:AtomSeq => oper.doApply(as, bypass)
	          case _ => SimpleApply(oper, arg)
	        }
		    case app:Applicable =>
		      // The lhs is applicable; invoke its apply method.  This will return
		      // some atom, and that atom is the overall result.
		      app.doApply(arg)
		    case rew:Rewriter =>
		      // The lhs is a rewriter; invoke its rewrite method.  This will return
		      // a pair.  We need to convert the pair to a binding.
		      val (r_atom, r_flag) = rew.doRewrite(arg)
		      BindingsAtom(Bindings() +
		          ("atom" -> r_atom) +
		          ("flag" -> (if (r_flag) Literal.TRUE else Literal.FALSE)))
		    case _ =>
		      // The lhs is something else.  It may be a variable or some other
		      // expression that we have yet to evaluate.  Just build a simple
		      // apply of the lhs and rhs.
		      SimpleApply(op, arg)
	    }
    }
  }
}

/**
 * An ''operator apply''.  This is the common case of applying a known operator
 * to some argument list.
 * 
 * @param op			The operator.
 * @param arg			The argument list.
 * @param pabinds	The bindings from parameter name to argument.  Note that
 * 								if the operator is associative the parameters may be
 * 								synthetic!
 */
case class OpApply(override val op: OperatorRef, override val arg: AtomSeq,
    val pabinds: Bindings) extends Apply(op, arg) {
  /**
   * Compute the type from the type specified by the operator, and the bindings
   * provided during parameter matching.  This allows rewriting otherwise
   * abstract type information to get a proper type.
   */
  val theType = op.operator.typ.rewrite(pabinds)._1
  
  /**
   * Generate a parseable string.
   */
  def toParseString = toESymbol(op.name) + "(" + arg.toNakedString + ")"
  
  override def rewrite(binds: Bindings) = {
    // Rewrite the argument, but not the operator.  In reality, operators
    // should protect their arguments using De Bruijn indices, but that's
    // not implemented just yet.
    val pair = arg.rewrite(binds)
    if (pair._2) (Apply(op, pair._1), true) else (this, false)
  }
}

case class SimpleApply(override val op: BasicAtom, override val arg: BasicAtom)
extends Apply(op, arg) {
  /**
   * We take the type from the operator.  This may be an incomplete type, but
   * we cannot rewrite it yet because we don't know the full bindings.  This
   * might cause trouble with matching.
   */
  val theType = op.theType
  
  /**
   * Generate a parseable string.
   */
  def toParseString = "(" +
  	(if (op.isInstanceOf[IntegerLiteral])
  	  "(" + op.toParseString + ")" else op.toParseString) +
    "." + arg.toParseString + ")"
}
