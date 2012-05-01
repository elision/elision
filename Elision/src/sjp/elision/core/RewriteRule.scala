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
  val isTerm = true
  val constantPool = None
  val deBruijnIndex = 0
  val depth = 0
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) = subject match {
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

class MapStrategy(sfh: SpecialFormHolder, val lhs: BasicAtom,
    val include: AtomSeq, val exclude: AtomSeq)
extends SpecialForm(sfh.tag, sfh.content) with Rewriter {
  override val theType = STRATEGY
  
  // Make the include and exclude lists into sets.
  private lazy val _inSet =
    include.
    filter(_.isInstanceOf[SymbolLiteral]).
    asInstanceOf[IndexedSeq[SymbolLiteral]].
    map(_.value.name).
    toSet
  private lazy val _exSet =
    exclude.
    filter(_.isInstanceOf[SymbolLiteral]).
    asInstanceOf[IndexedSeq[SymbolLiteral]].
    map(_.value.name).
    toSet
	
	def doRewrite(atom: BasicAtom) = atom match {
	  // We only process two kinds of atoms here: atom lists and operator
	  // applications.  Figure out what we have.
	  case AtomSeq(props, atoms) =>
	    // All we can do is apply the lhs to each atom in the list.
	    (AtomSeq(props, atoms.map(Apply(lhs,_))), true)
	  case Apply(op, seq: AtomSeq) => op match {
	    case so: SymbolicOperator => _apply(so, seq)
	    case _ => (Apply(op, AtomSeq(seq.props, seq.atoms.map(Apply(lhs,_)))), true)
	  }
	  case _ =>
	    // Do nothing in this case.
	    (atom, false)
	}
  
  private def _apply(op: SymbolicOperator, args: AtomSeq): (BasicAtom, Boolean) = {
    val plen = op.params.length
  	def getP(index: Int) = op.params(if (index > plen) plen-1 else index)
  	// If there are no parameters, we can't do anything.
  	if (plen == 0) {
      return (Apply(op, AtomSeq(args.props, args.map(Apply(lhs,_)))), true)
  	}
    // We examine each argument and look at the labels (if any) on the
    // corresponding parameter.  If any label is in the exclude set, then
    // we exclude the argument.  Next if the include set is not empty and
    // no label is in the include set, we exclude the argument.  Otherwise
    // we include it.
    var newargs: OmitSeq[BasicAtom] = EmptySeq
    var changed = false
    for (index <- 0 until args.length) {
      // Get the argument.
      var arg = args(index)
      // Get the corresponding parameter.
      val param = getP(index).asInstanceOf[Variable]
      // Get the labels.
      val labels = param.labels
      // If any label is excluded, then the argument is excluded.
      if (_exSet.intersect(labels).isEmpty) {
        // The label was not excluded.  If the include set is nonempty, then
        // the label must be in the include set.
        if (_inSet.isEmpty || !_inSet.intersect(labels).isEmpty) {
          // The argument is admitted.  Rewrite it and continue.
          arg = Apply(lhs, arg)
          if (!changed && arg != args(index)) changed = true
        }
      }
      // Add the argument to the new argument list.
      newargs :+= arg
    } // Loop over arguments.
    // If anything changed, make a new apply and return it.
    if (changed) return (Apply(op, AtomSeq(args.props, newargs)), true)
    else return (this, false)
  }
}

object MapStrategy {
  val tag = Literal(Symbol("map"))
  
  def apply(sfh: SpecialFormHolder): MapStrategy = {
    val bh = sfh.requireBindings
    bh.check(Map(""->true, "include"->false, "exclude"->false))
    val lhs = bh.fetchAs[AtomSeq]("")
    val include = bh.fetchAs[AtomSeq]("include", Some(EmptySeq))
    val exclude = bh.fetchAs[AtomSeq]("exclude", Some(EmptySeq))
    if (lhs.length < 1) {
      throw new SpecialFormException("Map requires exactly one item " +
      		"(specified first) to apply to each matching child.  None " +
      		"was given.  Be sure it is given before any #include or #exclude.")
    }
    if (lhs.length > 1) {
      throw new SpecialFormException("Map requires exactly one item " +
      		"(specified first) to apply to each matching child.  Too many " +
      		"were given.  Did you forget a delimiter or an #include or #exclude?")
    }
    return new MapStrategy(sfh, lhs(0), include, exclude)
  }
  
  def apply(lhs: BasicAtom, include: AtomSeq, exclude: AtomSeq): MapStrategy = {
    val binds = Bindings() + (""->AtomSeq(NoProps, lhs)) +
    		("include"->include) + ("exclude"->exclude)
    val sfh = new SpecialFormHolder(tag, binds)
    return new MapStrategy(sfh, lhs, include, exclude)
  }
  
  def unapply(map: MapStrategy) = Some((map.lhs, map.include, map.exclude))
}

/**
 * Easier construction and pattern matching of rewrite rules.
 */
object RewriteRule {
  val tag = Literal(Symbol("rule"))
  
  /**
   * Create a rewrite rule.  The rule is not synthetic.
   * 
	 * @param pattern			The pattern to match.
	 * @param rewrite			The rewrite to apply on match.
	 * @param guards			Guards that must be true to accept a match.
	 * @param rulesets		The rulesets that contain this rule.
   */
  def apply(pattern: BasicAtom, rewrite: BasicAtom, guards: Seq[BasicAtom],
      rulesets: Set[String]) = {
    val sfh = _makeSpecialFormHolder(pattern, rewrite, guards, rulesets)
    new RewriteRule(sfh, pattern, rewrite, guards, rulesets, false)
  }
  
  /**
   * Create a rewrite rule.
   * 
	 * @param pattern			The pattern to match.
	 * @param rewrite			The rewrite to apply on match.
	 * @param guards			Guards that must be true to accept a match.
	 * @param rulesets		The rulesets that contain this rule.
	 * @param synthetic		If true, this is a synthetic rule.
   */
  def apply(pattern: BasicAtom, rewrite: BasicAtom, guards: Seq[BasicAtom],
      rulesets: Set[String], synthetic: Boolean) = {
    val sfh = _makeSpecialFormHolder(pattern, rewrite, guards, rulesets)
    new RewriteRule(sfh, pattern, rewrite, guards, rulesets, synthetic)
  }
  
  private def _makeSpecialFormHolder(pattern: BasicAtom, rewrite: BasicAtom,
      guards: Seq[BasicAtom], rulesets: Set[String]) = {
    // Make the map pair.
    val mappair = MapPair(pattern, rewrite)
    // Make the binding.
    val binds:Bindings = Bindings() +
    	(""->mappair) +
    	("if"->AtomSeq(Associative(true) and Commutative(true), guards.toIndexedSeq[BasicAtom])) +
    	("rulesets"->
    		AtomSeq(Associative(true) and Commutative(true), rulesets.map {
    			str => Literal(Symbol(str))
    		}.toIndexedSeq[BasicAtom]))
    new SpecialFormHolder(tag, BindingsAtom(binds))
  }

  /**
   * Break a rewrite rule into its parts.  The synthetic flag is not returned.
   * 
   * @param rule	The rewrite rule.
   * @return	The pattern, rewrite, guards, and rulesets.
   */
  def unapply(rule: RewriteRule) = Some((rule.pattern, rule.rewrite,
      rule.guards, rule.rulesets))
      
  def apply(sfh: SpecialFormHolder): RewriteRule = {
    // A rewrite rule must be given with a binding.
    val bh = sfh.requireBindings
    // Check the content.
    bh.check(Map(""->true, "if"->false, "ruleset"->false, "rulesets"->false))
    // Get the map pair.
    val mappair = bh.fetchAs[AtomSeq]("")
    if (mappair.length < 1)
      throw new SpecialFormException(
          "Rewrite rule does not contain a map pair.")
    if (mappair.length > 1)
      throw new SpecialFormException(
          "Too many items at the top level of a rewrite rule; did you forget a marker?")
    val pair = mappair(0) match {
      case mp:MapPair => mp
      case x =>
        throw new SpecialFormException(
            "Top-level item in rewrite rule is not a map pair: " + x.toParseString)
    }
    // Get the guards.
    val guards = bh.fetchAs[AtomSeq]("if", Some(EmptySeq))
    // Get the rulesets.
    val rulesets = bh.fetchAs[AtomSeq]("rulesets", Some(EmptySeq)) map {
      rs => rs match {
        case SymbolLiteral(_, name) => name.name
        case _ =>
          throw new SpecialFormException(
              "Ruleset specification is not a symbol: " + rs.toParseString)
      }
    }
    // Build the rule.
    new RewriteRule(sfh, pair.left, pair.right, guards.atoms, rulesets.toSet)
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
 * @param sfh					The special form holder.
 * @param pattern			The pattern to match.
 * @param rewrite			The rewrite to apply on match.
 * @param guards			Guards that must be true to accept a match.
 * @param rulesets		The rulesets that contain this rule.
 * @param synthetic		If true, this is a synthetic rule.
 */
class RewriteRule private (
    sfh: SpecialFormHolder,
    val pattern: BasicAtom, val rewrite: BasicAtom,
    val guards: Seq[BasicAtom], val rulesets: Set[String],
    val synthetic: Boolean = false)
    extends SpecialForm(sfh.tag, sfh.content) with Rewriter {
  override val theType = STRATEGY
  
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
  def tryRewrite(subject: BasicAtom, binds: Bindings = Bindings()):
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
  	
  // To make a Scala parseable string we have to make the ruleset names into
  // parseable strings.
  override def toString = "RewriteRule(" +
  	pattern.toString + ", " +
  	rewrite.toString + ", " +
  	guards.toString + ", " +
  	rulesets.map(toEString(_)) + ")"
  	
  def doRewrite(atom: BasicAtom) = doRewrite(atom, Bindings())
  
  def doRewrite(atom: BasicAtom, binds: Bindings) = {
    // Try to apply the rewrite rule.  Whatever we get back is the result.
    //println("Rewriting with rule.")
    val result = tryRewrite(atom)
    (result._1, result._2)
  }
}
