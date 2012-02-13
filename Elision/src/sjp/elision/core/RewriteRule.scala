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
 * Encapsulate a rewrite rule.
 * 
 * ==Structure and Syntax==
 * 
 * ==Type==
 * 
 * ==Equality and Matching==
 * 
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
  
  /**
   * Attempt to apply this rule to a given atom.
   * 
   * To successfully apply a rule, the subject must match the pattern and
   * given the provided bindings (if any).  The complete set of bindings is
   * then used to rewrite every guard, and these are checked to see if they
   * are the literal true.  At present no further rewriting of guards is
   * performed.
   * 
   * If all guards are true, then the bindings are applied to the rewrite and
   * the result is returned.
   * 
   * @param subject	The subject to test.
   * @param binds		Bindings to honor.
   * @return	A pair consisting of an atom and a boolean.  The boolean is
   * 					true if the rewrite yielded a new atom, and is false otherwise.
   */
  def tryRewrite(subject: BasicAtom, binds: Bindings = new Bindings()):
  (BasicAtom, Boolean) = {
    // Local function to check the guards.
    def checkGuards(candidate: Bindings): Boolean = {
      for (guard <- guards) {
        val (newguard, _) = guard.rewrite(candidate)
        if (!newguard.isTrue) return false
      }
      true
    }
    
    // Local function to perform the rewrite if the rule fires.
    def doRewrite(candidate: Bindings) = rewrite.rewrite(candidate)
    
    // First we try to match the given atom against the pattern.
    pattern.tryMatch(subject, binds) match {
      case fail:Fail => (subject, false)
      case Match(newbinds) =>
        // We got a match.  Check the guards.
        if (checkGuards(newbinds)) doRewrite(newbinds)
        else (subject, false)
      case Many(iter) =>
        // We might have many matches.  We search through them until we find
        // one that satisfies the guards, or until we run out of candidates.
        for (newbinds <- iter) {
          if (checkGuards(newbinds)) doRewrite(newbinds)
        }
        (subject, false)
    }
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
      
  override def equals(rule: Any) = rule match {
    case orule:RewriteRule =>
      pattern == orule.pattern &&
      rewrite == orule.rewrite &&
      guards == orule.guards &&
      rulesets == orule.rulesets
    case _ => false
  }
}
