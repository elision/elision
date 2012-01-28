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
 */
case class Apply(op: BasicAtom, arg: BasicAtom, hack: Boolean) extends BasicAtom {
  // TODO This is simply wrong.  The type should be obtained from the operator.
  val theType = TypeUniverse

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
    if (opchanged || argchanged) (Apply(newop, newarg), true) else (this, false)
  }

  override def toString = 
    op match {
	    case Literal(_,SymVal(sval)) => sval.name + "(" + arg.toString + ")"
	    case _ => op.toString + "." + arg.toString
	  }
}

object Apply {
  /**
   * Capture the special case of applying a lambda.  We immediately bind the
   * lambda variable to the argument and rewrite the body.
   * @param lam		The lambda.
   * @param arg		The lambda argument to bind.
   */
  def apply(op: BasicAtom, arg: BasicAtom): BasicAtom = op match {
    case Lambda(lvar, body) =>
      body.rewrite((new Bindings) + (lvar.name -> arg))._1
    case _ => Apply(op, arg, true)
  }
}
