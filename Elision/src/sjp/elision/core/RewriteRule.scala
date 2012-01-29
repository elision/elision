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
 * A type for all rules.
 */
object RULETYPE extends RootType {
  val theType = TypeUniverse
  
  def toParseString = "RULETYPE"
}

/**
 * Encapsulate a rewrite rule.
 */
case class RewriteRule(pattern: BasicAtom, rewrite: BasicAtom,
    guards: Seq[BasicAtom], rulesets: Set[String], cacheLevel: Int)
    extends BasicAtom {
  val theType = RULETYPE

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    // Rules match only other rules.
    subject match {
    case RewriteRule(opat, orew, ogua, orul, olev) =>
      Fail("Rule matching is not implemented.", this, subject)
    case _ => Fail("Rules cannot match non-rules.", this, subject)
  }

  def rewrite(binds: Bindings) = {
    // Rewrite the pattern and rewrite.
    val (newpat, patchanged) = pattern.rewrite(binds)
    val (newrew, rewchanged) = rewrite.rewrite(binds)
    // Rewrite all the guards.
    var changed = patchanged || rewchanged
    var newgua = guards.map(gua => {
      val (newgua, guachanged) = gua.rewrite(binds)
      changed |= guachanged
      newgua
    })
    if (changed)
      (RewriteRule(newpat, newrew, newgua, rulesets, cacheLevel), true)
    else (this, false)
  }
  
  def toParseString = "RULE { " + pattern.toParseString + " -> " +
  	rewrite.toParseString +
  	(if (!guards.isEmpty) guards.mkParseString(" if ", " if ", "") else "") +
  	(if (!rulesets.isEmpty) rulesets.map(toESymbol(_)).mkString(" rulesets ", ", ", "") else "") +
  	" level " + cacheLevel + " }"
}
