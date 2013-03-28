/*======================================================================
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 * The Elision Term Rewriter
 * 
 * Copyright (c) 2012 by UT-Battelle, LLC.
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
 * Collection of administrative costs for redistribution of the source code or
 * binary form is allowed. However, collection of a royalty or other fee in excess
 * of good faith amount for cost recovery for such redistribution is prohibited.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER, THE DOE, OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
======================================================================
* */
package ornl.elision.core

import ornl.elision.util.OmitSeq
import ornl.elision.util.Debugger

/**
 * The ruleset strategy.
 * 
 * == Purpose ==
 * This strategy is only applied if it is concrete; that is, the list of
 * ruleset names consists of just symbols.
 * 
 * This strategy applies the rules in the identified rulesets to the atom.
 * The first rule, in declared order, to rewrite the atom wins.  At most
 * one rewrite is performed.
 * 
 * The success flag is determined by the rule applied, or is `false` if no
 * rule is applied.
 */
class RulesetStrategy private (sfh: SpecialFormHolder,
    val context: Context, val names: AtomSeq)
extends SpecialForm(sfh.tag, sfh.content) with Rewriter {
  /** True if all ruleset names are concrete. */
  private var _conc = true
  /** The ruleset names that are concrete. */
  private val _namelist = (names.map {
    _ match {
      case SymbolLiteral(_, sym) => sym.name
      case StringLiteral(_, name) => name
      case _ =>
        _conc = false
        ""
    }
  }).toSet

  def doRewrite(atom: BasicAtom, hint: Option[Any]): (BasicAtom, Boolean) = {  
    // If this is not concrete, do not execute.
    if (!_conc) {
      return (SimpleApply(this, atom), false)
    } else {
      // Get the rules.
      val rules = context.ruleLibrary.getRules(atom, _namelist)
      // Now try every rule until one applies.
      for (rule <- rules) {
        val (newatom, applied) = rule.doRewrite(atom, hint)
        if (applied) {
          return (newatom, applied)
        }
      } // Try all rules.
      return (atom, false)
    }
  }
}

/**
 * Make and match ruleset strategy objects.
 */
object RulesetStrategy {
  /** The special form tag. */
  val tag = Literal('apply)
  
  /**
   * Make a ruleset strategy from the given special form data.
   * 
   * @param sfh	The special form data.
   * @return	The strategy object.
   */
  def apply(sfh: SpecialFormHolder, context: Context): RulesetStrategy = {
    val bh = sfh.requireBindings
    bh.check(Map(""->true))
    val names = bh.fetchAs[AtomSeq]("", Some(EmptySeq))
    new RulesetStrategy(sfh, context, names)
  }
  
  /**
   * Make a ruleset strategy from the components.
   * 
   * @param context	The context providing the rules.
   * @param names		The ruleset names; order is not significant.
   * @return	The strategy object.
   */
  def apply(context: Context, names: AtomSeq): RulesetStrategy = {
    val binds = Bindings() + (""->names)
    val sfh = new SpecialFormHolder(tag, binds)
    new RulesetStrategy(sfh, context, names)
  }
  
  /**
   * Make a ruleset strategy from the components.
   * 
   * @param context	The context providing the rules.
   * @param names		The ruleset names; order is not significant.
   * @return	The strategy object.
   */
  def apply(context: Context, names: String*): RulesetStrategy = {
    val nameseq =
      AtomSeq(NoProps, names.map(str => Literal(Symbol(str))).toIndexedSeq)
    val binds = Bindings() + (""->nameseq)
    val sfh = new SpecialFormHolder(tag, binds)
    new RulesetStrategy(sfh, context, nameseq)
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
  
	def doRewrite(atom: BasicAtom, hint: Option[Any]) = {
		atom match {
		  // We only process two kinds of atoms here: atom lists and operator
		  // applications.  Figure out what we have.
		  case AtomSeq(props, atoms) =>
  			// All we can do is apply the lhs to each atom in the list.
  			(AtomSeq(props, atoms.map(Apply(lhs,_))), true)
  			
		  case Apply(op, seq: AtomSeq) => op match {
    			case so: SymbolicOperator => 
    				_apply(so, seq)
    				
    			case _ => 
    				(Apply(op, AtomSeq(seq.props, seq.atoms.map(Apply(lhs,_)))), true)
  		}
		  
		  case _ =>
			  // Do nothing in this case.
        (atom, false)
		}
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
  val tag = Literal('map)
  
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
  val tag = Literal('rule)
  
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
    var binds:Bindings = Bindings() + (""->AtomSeq(NoProps, mappair))
    if (guards.length > 0) {
      binds += ("if" -> AtomSeq(NoProps, guards.toIndexedSeq[BasicAtom]))
    }
    if (rulesets.size > 0) {
      binds += ("rulesets" ->
          AtomSeq(Associative(true) and Commutative(true), rulesets.map {
            str => Literal(str)
          }.toIndexedSeq[BasicAtom]))
    }
    new SpecialFormHolder(tag, BindingsAtom(binds))
  }

  /**
   * Break a rewrite rule into its parts.  The synthetic flag is not returned.
   * 
   * @param rule	The rewrite rule.
   * @return	The pattern, rewrite, guards, rulesets, and whether the rule is
   *          synthetic.
   */
  def unapply(rule: RewriteRule) = Some((rule.pattern, rule.rewrite,
      rule.guards, rule.rulesets, rule.synthetic))
      
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
    // Get the rulesets.  We combine two different possibilities here.
    val rseq = bh.fetchAs[AtomSeq]("ruleset", Some(EmptySeq)) ++
      bh.fetchAs[AtomSeq]("rulesets", Some(EmptySeq))
    val rulesets = rseq map {
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
   * @param hint		An optional hint to pass along during matching.
   * @return	A pair consisting of an atom and a boolean.  The boolean is
   * 					true if the rewrite yielded a new atom, and is false otherwise.
   */
  private def _tryRewrite(subject: BasicAtom, binds: Bindings = Bindings(),
      hint: Option[Any] = None): (BasicAtom, Boolean) = {
    // Local function to check the guards.
    def checkGuards(candidate: Bindings): Boolean = {
      for (guard <- guards) {
        val (newguard, _) = guard.rewrite(candidate)
        val (newguard1, _) = knownExecutor.context.ruleLibrary.rewrite(newguard)
        if (!newguard1.isTrue) return false
      }
      true
    }
    
    // Local function to perform the rewrite if the rule fires.  We return
    // true in the pair no matter what, since the rule fired.
    def doRuleRewrite(candidate: Bindings) = {
      Debugger("rewrite", "Applied rule: " + this.toParseString +
          " to: " + candidate.toParseString + "")
      (rewrite.rewrite(candidate)._1, true)
    }
    
    // First we try to match the given atom against the pattern.
    pattern.tryMatch(subject, binds, hint) match {
      case fail:Fail => {
        return (subject, false)
      }
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
  
  
  /**
   * Checks to see if the RewriteRule will be applied to an atom 
   * without actually applying it.
   */
  private def _prodRewrite(subject: BasicAtom, binds: Bindings = Bindings(),
      hint: Option[Any] = None): Boolean = {
    // Local function checks if guards are rewritten.
    def checkGuards(candidate: Bindings): Boolean = {
      for (guard <- guards) {
        val (newguard, _) = guard.rewrite(candidate)
        val (newguard1, _) = knownExecutor.context.ruleLibrary.rewrite(newguard)
        if (!newguard1.isTrue) return false
      }
      true
    }
    
    // First we try to match the given atom against the pattern.
    pattern.tryMatch(subject, binds, hint) match {
      case fail:Fail => { 
        return false
      }
      case Match(newbinds) =>
        // We got a match.  Check the guards.
        if (checkGuards(newbinds)) return true
        else return false
      case Many(iter) =>
        // We might have many matches.  We search through them until we find
        // one that satisfies the guards, or until we run out of candidates.
        for (newbinds <- iter) {
          if (checkGuards(newbinds)) return true
        }
        return false
    }
  }
  
  	
  def doRewrite(atom: BasicAtom, hint: Option[Any]) =
    doRewrite(atom, Bindings(), hint)
  
  def doRewrite(atom: BasicAtom, binds: Bindings, hint: Option[Any]) = {
    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      (atom, true)
    } else {
      // Try to apply the rewrite rule.  Whatever we get back is the result.
      _tryRewrite(atom, binds, hint)
    }
  }
}
