/* Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 */
package sjp.elision.core

/**
 * An apply represents applying an operator to an argument of some kind.
 * @param op		The operator.
 * @param arg		The argument.
 * @param hack	Ignored.
 */
case class Apply(op: BasicAtom, arg: BasicAtom, hack: Boolean)
extends BasicAtom {
  val theType = op.theType
  
  // The De Brujin index is just the maximum of the operator and body.
  val deBrujinIndex = op.deBrujinIndex.max(arg.deBrujinIndex)

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    // Only applies match applies.
    subject match {
      case Apply(oop, oarg, _) => {
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
    if (opchanged || argchanged) (Apply(newop, newarg, true), true)
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
      Apply(realop, arg, true)
    case _ => Apply(op, arg, true)
  }
}
