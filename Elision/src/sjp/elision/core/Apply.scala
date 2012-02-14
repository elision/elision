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

/**
 * An apply represents applying an operator to an argument.
 * 
 * ==Structure and Syntax==
 * There are two forms of apply.
 * 
 * The usual form in which an operator is applied to a list of arguments that
 * match formal parameters, written with the operator identified by a symbol,
 * and the arguments in parentheses juxtaposed with the operator.
 *  - `max(5,9)`
 *  
 * The second form is the more general form, where some atom is treated as
 * an operator, and applied to another atom.  These atoms may be anything.
 * The application is written by interposing a dot (.) between the two atoms.
 *  - `max.%(5,9)`
 *  - `\$op.\$arg`
 * 
 * The second form is right associative.
 *  - `x.y.z` = `x.(y.x)`
 *  
 * ==Type==
 * The type is taken from the operator.
 *  
 * ==Equality and Matching==
 * Two applies are equal iff their operator and argument are equal.
 * 
 * Two applies match if the respective operators and arguments match.
 * 
 * @param op		The operator.
 * @param arg		The argument.
 */
class Apply private (val op: BasicAtom, val arg: BasicAtom)
extends BasicAtom {
  /** The type is taken from the operator. */
  val theType = op.theType
  
  /** An apply is constant iff both the operator and argument are constant. */
  val isConstant = op.isConstant && arg.isConstant
  
  /** The De Brujin index is just the maximum of the operator and body. */
  val deBrujinIndex = op.deBrujinIndex.max(arg.deBrujinIndex)

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    // Only applies match other applies.
    subject match {
      case Apply(oop, oarg) => {
        // Try to match the operators, and then the arguments.  If both match,
        // then this matches.  If not, then this does not match.
        op.tryMatch(oop, binds) match {
          case fail: Fail =>
            Fail("Operators do not match.", this, subject, Some(fail))
          case Match(newbinds) =>
            // The operators match.  Now try to match the arguments.
            arg.tryMatch(oarg, newbinds)
          case Many(matches) =>
            // The operators match in multiple ways.  This seems unlikely, but
            // we consider it here anyway.
            Many(new MatchIterator(arg.tryMatch(oarg, _), matches))
        }
      }
      case _ => Fail("Applications only match other applications.",
          this, subject)
    }

  def rewrite(binds: Bindings) = {
    val (newop, opchanged) = op.rewrite(binds)
    val (newarg, argchanged) = arg.rewrite(binds)
    if (opchanged || argchanged) (Apply(newop, newarg), true)
    else (this, false)
  }

  def toParseString =
    // If the operator is an actual operator instance, and if the argument is
    // an atom list, then we generate the "friendly" version of the apply.
    // Otherwise we generate the more general form of the apply.
    op match {
	    case actual:Operator =>
	      arg match {
	        case al:AtomList =>
	          toESymbol(actual.name) + "(" + al.toNakedString + ")"
	        case _ =>
	          op.toParseString + "." + arg.toParseString
	      }
	    case _ =>
	      op.toParseString + "." + arg.toParseString
	  }
  
  override def toString = "Apply(" + op.toString + ", " + arg.toString + ")"
  
  override lazy val hashCode = op.hashCode * 31 + arg.hashCode
  
  override def equals(other: Any) = other match {
    case Apply(oop, oarg) =>
      oop == op && oarg == arg
    case _ => false
  }
}

/**
 * Provide additional constructors for an apply.
 */
object Apply {
  /**
   * Construct an operator application, handling special cases.
   * @param op			The operator.
   * @param arg			The argument.
   */
  def apply(op: BasicAtom, arg: BasicAtom): BasicAtom = {
    //println("Building an apply:")
    //println("  op -> " + op)
    //println(" arg -> " + arg)
	  op match {
	    case Lambda(_, lvar, body) =>
	      // Curry the lambda body by binding the variable to the argument and then
	      // rewriting the body.
	      body.rewrite((new Bindings) + (lvar.name -> arg))._1
	    case rule:RewriteRule =>
	      // Try to apply the rewrite rule.  Whatever we get back is the result.
	      //println("Rewriting with rule.")
	      rule.tryRewrite(arg)._1
	    case _ =>
	      //println("Rewriting vanilla.")
	      new Apply(op, arg)
	  } 
  }
  
  /**
   * Unpack an operator application.
   * @param apply	The application.
   * @return	The pair of operator and argument.
   */
  def unapply(apply: Apply) = Some(apply.op, apply.arg)
}
 
object Op {
  /**
   * Pull an apply apart.  This only works in the case of an operator applied
   * to an atom list.
   * 
   * @param apply	The apply to dissect.
   * @return	The parts, as a sequence, starting with the operator name.
   */
  def unapplySeq(apply: Apply): Option[Seq[BasicAtom]] =
    apply match {
    case Apply(op:Operator, AtomList(atoms,_)) =>
      Some(Literal(STRING,op.name) +: atoms)
    case _ => None
  }
}
