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
 * A type for all rules.
 */
object RULETYPE extends RootType {
  val theType = TypeUniverse
  val isConstant = true
  def toParseString = "RULETYPE"
  override lazy val hashCode = toParseString.hashCode
}

/**
 * Encapsulate a rewrite rule.
 * @param pattern			The pattern to match.
 * @param rewrite			The rewrite to apply on match.
 * @param guards			Guards that must be true to accept a match.
 * @param rulesets		The rulesets that contain this rule.
 * @param cachelLevel	Memoization cache level.
 */
case class RewriteRule(pattern: BasicAtom, rewrite: BasicAtom,
    guards: Seq[BasicAtom], rulesets: Set[String], cacheLevel: Int)
    extends BasicAtom {
  val theType = RULETYPE
  
  lazy val isConstant = pattern.isConstant && rewrite.isConstant &&
  	guards.foldLeft(true)(_ && _.isConstant)
  
  // The De Brujin index of a rewrite rule is equal to the maximum index of
  // the children of the rule.
  val deBrujinIndex = {
    import scala.math._
    val tmp = max(pattern.deBrujinIndex, rewrite.deBrujinIndex)
    max(tmp, guards.foldLeft(0)((x,y) => x.max(y.deBrujinIndex)))
  }

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    // Rules match only other rules.
    subject match {
	    case RewriteRule(opat, orew, ogua, orul, olev) =>
	      SequenceMatcher.tryMatch(pattern +: rewrite +: guards,
	          opat +: orew +: ogua)
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
    
    // If any part of the rewrite rule changed, generate a new rewrite rule,
    // preserving the ruleset membership and cache level.
    if (changed)
      (RewriteRule(newpat, newrew, newgua, rulesets, cacheLevel), true)
    else (this, false)
  }
  
  def toParseString = "RULE { " + pattern.toParseString + " -> " +
  	rewrite.toParseString +
  	(if (!guards.isEmpty) guards.mkParseString(" if ", " if ", "") else "") +
  	(if (!rulesets.isEmpty)
  	  rulesets.map(toESymbol(_)).mkString(" rulesets ", ", ", "") else "") +
  	" level " + cacheLevel + " }"
  	
  // To make a Scala parseable string we have to make the ruleset names into
  // parseable strings.
  override def toString = "RewriteRule(" +
  	pattern.toString + ", " +
  	rewrite.toString + ", " +
  	guards.toString + ", " +
  	rulesets.map(toEString(_)) + ", " +
  	cacheLevel + ")"
  	
  override lazy val hashCode = (pattern.hashCode * 31 + rewrite.hashCode) *
      31 + guards.hashCode
}
