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
 * An apply represents applying an operator to an argument of some kind.
 * @param op		The operator.
 * @param arg		The argument.
 * @param hack	Ignored.
 */
class Apply private (val op: BasicAtom, val arg: BasicAtom)
extends BasicAtom {
  val theType = op.theType
  
  // The De Brujin index is just the maximum of the operator and body.
  val deBrujinIndex = op.deBrujinIndex.max(arg.deBrujinIndex)

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    // Only applies match applies.
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
    if (opchanged || argchanged) (new Apply(newop, newarg), true)
    else (this, false)
  }

  override def toParseString = 
    op match {
	    case _:Operator => op.toParseString +
	    	"(" + arg.toParseString + ")"
	    case _ => op.toParseString + "." + arg.toParseString
	  }
}

object Apply {
  /**
   * Construct an operator application, handling special cases.
   * @param context	The context.
   * @param op			The operator.
   * @param arg			The argument.
   */
  def apply(context: Context, op: BasicAtom, arg: BasicAtom): BasicAtom = op match {
    case Lambda(_, lvar, body) =>
      body.rewrite((new Bindings) + (lvar.name -> arg))._1
    case Literal(_,SymVal(name)) =>
      // Get the real operator for the symbolic name.
      val realop = context.operatorLibrary(name.name)
      // Apply this operator to the given argument(s).
      new Apply(realop, arg)
    case _ => new Apply(op, arg)
  }
  
  /**
   * Construct an operator application.  No additional processing is
   * performed here.
   * @param op		The operator.
   * @param arg		The argument.
   * @return	The new application.
   */
  private[core] def apply(op: Operator, arg: BasicAtom) = new Apply(op, arg)
  
  /**
   * Unpack an operator application.
   * @param apply	The application.
   * @return	The pair of operator and argument.
   */
  def unapply(apply: Apply) = Some(apply.op, apply.arg)
}
