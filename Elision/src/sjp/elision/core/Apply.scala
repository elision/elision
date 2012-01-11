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
 */
case class Apply(op: BasicAtom, arg: BasicAtom) extends BasicAtom {
  // TODO This is simply wrong.  The type should be obtained from the operator.
  val theType = TypeUniverse

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
    }

  def rewrite(binds: Bindings) = {
    val (newop, opchanged) = op.rewrite(binds)
    val (newarg, argchanged) = arg.rewrite(binds)
    if (opchanged || argchanged) (Apply(newop, newarg), true) else (this, false)
  }

  override def toString = op.toString + "(" + arg.toString + ")"
}