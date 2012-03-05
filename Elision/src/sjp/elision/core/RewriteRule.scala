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
 * This strategy applies the rules in a ruleset.  The first rule, in declared
 * order, to rewrite the atom wins.  At most one rewrite is performed.
 * 
 * The basic syntax is:
 * {{{
 * { rulesets NAME1, NAME2, ..., NAMEn }
 * }}}
 * This applies rules from the named rulesets.  The listing order of the
 * rulesets is //not// significant; only the declaration order of the rules.
 * 
 * @param context	The context supplying the rules.
 * @param names		The names of the rulesets to apply.
 */
case class RulesetStrategy(context: Context, names: List[String])
extends BasicAtom with Rewriter {
  val theType = STRATEGY
  val isConstant = true
  val constantPool = None
  val deBruijnIndex = 0
  val depth = 0
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) = subject match {
    case RulesetStrategy(_, onames) if names == onames => Match(binds)
    case _ => Fail("Ruleset strategies do not match.", this, subject)
  }
  def rewrite(binds: Bindings) = (this, false)
  def toParseString =
    names.map(toESymbol(_)).mkString("{ apply rulesets ", ", ", " }")
  override def toString = "RulesetStrategy(" + context + ", " +
  		names.map(toESymbol(_)).mkString(", ") + ")"
  		
  /**
   * Apply this strategy.  If any rule completes then the returned flag is
   * true.  Otherwise it is false.
   */
  def doRewrite(atom: BasicAtom): (BasicAtom, Boolean) = {
    // Get the rules.
    val rules = context.getRules(atom, names)
    // Now try every rule until one applies.
    for (rule <- rules) {
      val (newatom, applied) = rule.tryRewrite(atom)
      if (applied) return (newatom, applied)
    }
    return (atom, false)
  }
}

case class MapStrategy(include: Set[String], exclude: Set[String],
    lhs: BasicAtom) extends BasicAtom with Rewriter {
	val theType = STRATEGY
	
	val isConstant = lhs.isConstant
	val deBruijnIndex = lhs.deBruijnIndex
	val depth = lhs.depth + 1
	val constantPool =
	  Some(BasicAtom.buildConstantPool(theType.hashCode, lhs))
	  
	def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
  	subject match {
	  case MapStrategy(oin, oex, olhs) =>
	    if (include != oin || exclude != oex)
	      Fail("Labels do not match.", this, subject)
      else
        lhs.tryMatch(olhs, binds)
	  case _ => Fail("Subject is not a map strategy.", this, subject)
	}
	  
	def rewrite(binds: Bindings) = lhs.rewrite(binds) match {
	  case (newlhs, true) => (MapStrategy(include, exclude, newlhs), true)
	  case _ => (this, false)
	}
	
	def toParseString = "{ map " +
			include.map("@" + toESymbol(_)).mkString(" ") +
			exclude.map("-@" + toESymbol(_)).mkString(" ") +
			" " + lhs.toParseString + " }"
			
  override def toString = "MapStrategy(" +
  		include.map(toEString(_)).mkString("List(", ", ", ")") +
  		exclude.map(toEString(_)).mkString("List(", ", ", ")") +
  		", " + lhs.toString + ")"
  		
  override def hashCode = lhs.hashCode
	
	def doRewrite(atom: BasicAtom) = atom match {
	  // We only process two kinds of atoms here: atom lists and operator
	  // applications.  Figure out what we have.
	  case AtomList(atoms, props) =>
	    // All we can do is apply the lhs to each atom in the list.
	    (AtomList(atoms.map(Apply(lhs,_)), props), true)
	  case Apply(op, AtomList(atoms, props)) =>
      // We apply the lhs to each argument whose parameter meets the
      // label criteria.  This is modestly tricky.
      (Apply(op, AtomList(atoms.map(Apply(lhs,_)), props)), true)
	  case _ =>
	    // Do nothing in this case.
	    (atom, false)
	}
}

case class RMapStrategy(rhs: BasicAtom) extends BasicAtom with Rewriter {
	val theType = STRATEGY
	
	val isConstant = rhs.isConstant
	val deBruijnIndex = rhs.deBruijnIndex
	val depth = rhs.depth + 1
	val constantPool =
	  Some(BasicAtom.buildConstantPool(theType.hashCode, rhs))
	  
	def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
  	subject match {
	  case RMapStrategy(orhs) => rhs.tryMatch(orhs, binds)
	  case _ => Fail("Subject is not an rmap strategy.", this, subject)
	}
	  
	def rewrite(binds: Bindings) = rhs.rewrite(binds) match {
	  case (newrhs, true) => (RMapStrategy(newrhs), true)
	  case _ => (this, false)
	}
	
	def toParseString = "{ rmap " + rhs.toParseString + " }"
			
  override def toString = "RMapStrategy(" + rhs.toString + ")"
  		
  override def hashCode = rhs.hashCode
	
	def doRewrite(atom: BasicAtom) = atom match {
	  // We only process two kinds of atoms here: atom lists and operator
	  // applications.  Figure out what we have.
	  case AtomList(atoms, props) =>
	    // All we can do is apply the rhs to each atom in the list.
	    (AtomList(atoms.map(Apply(_,rhs)), props), true)
	  case Apply(op, AtomList(atoms, props)) =>
      // We apply the rhs to each argument whose parameter meets the
      // label criteria.  This is modestly tricky.
      (Apply(op, AtomList(atoms.map(Apply(_,rhs)), props)), true)
	  case _ =>
	    // Do nothing in this case.
	    (atom, false)
	}
}

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
    extends BasicAtom with Rewriter {
  val theType = STRATEGY
  
  lazy val isConstant = pattern.isConstant && rewrite.isConstant &&
  	guards.foldLeft(true)(_ && _.isConstant)
  	
  val constantPool =
    Some(BasicAtom.buildConstantPool(theType.hashCode,
        (pattern +: rewrite +: guards):_*))
  
  // The De Bruijn index of a rewrite rule is equal to the maximum index of
  // the children of the rule.
  val deBruijnIndex =
    guards.foldLeft(pattern.deBruijnIndex max rewrite.deBruijnIndex) {
    _ max _.deBruijnIndex
  }
  
  /**
   * The depth of a rewrite rule is equal to the maximum depth of the pattern,
   * guard, and rewrite rules, plus one.
   */
  val depth = guards.foldLeft(pattern.depth max rewrite.depth)(_ max _.depth) + 1

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
  
  def doRewrite(atom: BasicAtom) = tryRewrite(atom)
  
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
    
    // Local function to perform the rewrite if the rule fires.  We return
    // true in the pair no matter what, since the rule fired.
    def doRuleRewrite(candidate: Bindings) = (rewrite.rewrite(candidate)._1, true)
    
    // First we try to match the given atom against the pattern.
    pattern.tryMatch(subject, binds) match {
      case fail:Fail => return (subject, false)
      case Match(newbinds) =>
        // We got a match.  Check the guards.
        if (checkGuards(newbinds)) return doRuleRewrite(newbinds)
        else return (subject, false)
      case Many(iter) =>
        // We might have many matches.  We search through them until we find
        // one that satisfies the guards, or until we run out of candidates.
        for (newbinds <- iter) {
          if (checkGuards(newbinds)) return doRuleRewrite(newbinds)
        }
        return (subject, false)
    }
  }
  
  def toParseString = "{ RULE " + pattern.toParseString + " -> " +
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
  
  def doRewrite(atom: BasicAtom, binds: Bindings) = {
    // Try to apply the rewrite rule.  Whatever we get back is the result.
    //println("Rewriting with rule.")
    val result = tryRewrite(atom)
    (result._1, result._2)
  }
}
