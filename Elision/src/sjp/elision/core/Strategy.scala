/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package sjp.elision.core

/**
 * Encapsulate a strategy.  This is the common root type for all strategies.
 */
sealed abstract class Strategy extends BasicAtom {
  val theType = TypeUniverse
  
  /**
   * Apply this strategy to the given atom, honoring the supplied bindings.
   * The result of this is a pair consisting of a (possibly new) atom and
   * a flag indicating whether the strategy succeeded.
   * 
   * @param atom	The atom to rewrite.
   * @param binds	Bindings to honor.
   * @return	The pair of result and flag.
   */
  def apply(atom: BasicAtom, binds: Bindings): (BasicAtom, Boolean)
  
  /**
   * Rewrite the strategy by applying the bindings.  The resuls of rewriting
   * a strategy is //always// itself a strategy.
   * 
   * @param binds	The bindings to apply.
   * @return	A pair consisting of the potentially new strategy and true iff
   * 					any rewrites were performed.
   */
  override def rewrite(binds: Bindings): (Strategy, Boolean)
}

//======================================================================
// Strategy combinators.
//======================================================================

/**
 * This strategy applies a single rule.  The syntax is the same as for a
 * rule.
 * 
 * @param rule	The rule to apply.
 */
case class RuleStrategy(rule: RewriteRule) extends Strategy {
  val deBruijnIndex = rule.deBruijnIndex
  val isConstant = rule.isConstant
  
  /**
   * Apply this strategy.  If the rule completes a rewrite then the returned
   * flag is true.  Otherwise it is false.
   */
  def apply(atom: BasicAtom, binds: Bindings) = rule.tryRewrite(atom, binds)
  def toParseString = rule.toParseString
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    subject match {
    case RuleStrategy(orule) => rule.tryMatch(orule, binds)
    case _ => Fail("Strategies do not match.", this, subject)
  }
  def rewrite(binds: Bindings) = rule.rewrite(binds) match {
    case (newrule:RewriteRule, true) => (RuleStrategy(newrule), true)
    case _ => (this, false)
  }
}

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
extends Strategy {
  val deBruijnIndex = 0
  val isConstant = true
  
  /**
   * Apply this strategy.  If any rule completes then the returned flag is
   * true.  Otherwise it is false.
   */
  def apply(atom: BasicAtom, binds: Bindings): (BasicAtom, Boolean) = {
    // Get the rules.
    val rules = context.getRules(atom, names)
    // Now try every rule until one applies.
    for (rule <- rules) {
      val (newatom, applied) = rule.tryRewrite(atom, binds)
      if (applied) return (newatom, true)
    }
    return (atom, false)
  }
  def toParseString = names.map(toESymbol(_)).mkString("{ rulesets ",",","}")
  override def toString = "RulesetStrategy(" + context.toString +
		", List[String](" + names.map(toEString).mkString(",") + "))"
	def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
	  subject match {
    case RulesetStrategy(ocontext, onames)
    	if context == ocontext && names == onames => (Match(binds))
    case _ => Fail("Strategies do not match.", this, subject)
  }
  def rewrite(binds: Bindings) = (this, false)
}

/**
 * Repeatedly apply a strategy so long as the strategy returns the true
 * flag.
 * 
 * The syntax is:
 * {{{
 * { while S }
 * }}}
 * where `S` is a strategy.
 * 
 * @param child	The strategy to apply.
 */
case class WhileStrategy(child: Strategy) extends Strategy {
  val deBruijnIndex = child.deBruijnIndex
  val isConstant = child.isConstant
  
  /**
   * Apply this strategy.  The returned flag is always true.
   */
  def apply(atom: BasicAtom, binds: Bindings): (BasicAtom, Boolean) = {
    val (newatom, flag) = child.apply(atom, binds)
    if (!flag) (atom, true) else apply(newatom, binds)
  }
  def toParseString = "{ while " + child.toParseString + "}"
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    subject match {
    case WhileStrategy(ochild) => child.tryMatch(ochild)
    case _ => Fail("Strategies do not match.", this, subject)
  }
  def rewrite(binds: Bindings) = child.rewrite(binds) match {
    case (newchild: Strategy, true) => (WhileStrategy(newchild), true)
    case _ => (this, false)
  }
}

/**
 * Apply a sequence of strategies, unconditionally.
 * 
 * The syntax is:
 * {{{
 * { then S1, S2, ..., Sn }
 * }}}
 * where `S1`, `S2`, ..., `Sn` are the strategies to apply, in order.
 * 
 * @param children	The child strategies, in order.
 */
case class ThenStrategy(children: List[Strategy]) extends Strategy {
  val deBruijnIndex = children.foldLeft(0)(_ max _.deBruijnIndex)
  val isConstant = children.forall(_.isConstant)
  
  /**
   * Apply every strategy to the atom, in sequence.  The returned flag is the
   * inclusive or of the flags returned from the child strategies.
   */
  def apply(atom: BasicAtom, binds: Bindings) = {
    var success = false
    var isnow = atom
    for (child <- children) {
      val (newatom, flag) = child.apply(isnow, binds)
      success |= flag
      isnow = newatom
    }
    (isnow, success)
  }
  def toParseString = children.mkParseString("{ then ", ",", "}")
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    subject match {
    case ThenStrategy(ochildren) =>
      SequenceMatcher.tryMatch(children, ochildren, binds)
    case _ => Fail("Strategies do not match.", this, subject)
  }
  def rewrite(binds: Bindings) = {
    var changed = false
    def doit(atoms: List[Strategy]): List[Strategy] =
      if (atoms.isEmpty) List[Strategy]() else atoms.head.rewrite(binds) match {
        case (newstrategy: Strategy, true) =>
          changed = true
          newstrategy :: doit(atoms.tail)
        case _ =>
          atoms.head :: doit(atoms.tail)
      }
    val newchildren = doit(children)
    if (changed) (ThenStrategy(newchildren), true) else (this, false)
  }
}

/**
 * Apply a sequence of strategies, continuing as long as each strategy returns
 * a true flag, and stopping at the first false (if any).
 * 
 * The syntax is:
 * {{{
 * { andalso S1, S2, ..., Sn }
 * }}}
 * where `S1`, `S2`, ..., `Sn` are the strategies to apply, in order.
 * 
 * @param children	The child strategies, in order.
 */
case class AndAlsoStrategy(children: List[Strategy]) extends Strategy {
  val deBruijnIndex = children.foldLeft(0)(_ max _.deBruijnIndex)
  val isConstant = children.forall(_.isConstant)
  
  /**
   * Apply each strategy to the atom, in order, stopping as soon as a strategy
   * returns false or the list is exhausted.  The returned flag is true iff
   * every strategy is applied and returns true.
   */
  def apply(atom: BasicAtom, binds: Bindings) = {
    var isnow = atom
    val success = children.forall {
      child => {
      	val (newatom, flag) = child.apply(isnow, binds)
      	isnow = newatom
      	flag
      }
    }
    (isnow, success)
  }
  def toParseString = children.mkParseString("{ andalso ", ",", "}")
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    subject match {
    case AndAlsoStrategy(ochildren) =>
      SequenceMatcher.tryMatch(children, ochildren, binds)
    case _ => Fail("Strategies do not match.", this, subject)
  }
  def rewrite(binds: Bindings) = {
    var changed = false
    def doit(atoms: List[Strategy]): List[Strategy] =
      if (atoms.isEmpty) List[Strategy]() else atoms.head.rewrite(binds) match {
        case (newstrategy: Strategy, true) =>
          changed = true
          newstrategy :: doit(atoms.tail)
        case _ =>
          atoms.head :: doit(atoms.tail)
      }
    val newchildren = doit(children)
    if (changed) (AndAlsoStrategy(newchildren), true) else (this, false)
  }
}

/**
 * Apply a sequence of strategies, stopping as soon as a strategy returns a
 * true flag, and continuing otherwise.
 * 
 * The syntax is:
 * {{{
 * { orelse S1, S2, ..., Sn }
 * }}}
 * where `S1`, `S2`, ..., `Sn` are the strategies to apply, in order.
 * 
 * @param children	The child strategies, in order.
 */
case class OrElseStrategy(children: List[Strategy]) extends Strategy {
  val deBruijnIndex = children.foldLeft(0)(_ max _.deBruijnIndex)
  val isConstant = children.forall(_.isConstant)
  
  /**
   * Appy each strategy to the atom, in order, stopping as soon as a strategy
   * returns a true flag.  The returned flag is false iff no strategy returns
   * a true flag.
   */
  def apply(atom: BasicAtom, binds: Bindings) = {
    var isnow = atom
    val success = children.exists {
      child => {
      	val (newatom, flag) = child.apply(isnow, binds)
      	isnow = newatom
      	flag
      }
    }
    (isnow, success)
  }
  def toParseString = children.mkParseString("{ orelse ", ",", "}")
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    subject match {
    case OrElseStrategy(ochildren) =>
      SequenceMatcher.tryMatch(children, ochildren, binds)
    case _ => Fail("Strategies do not match.", this, subject)
  }
  def rewrite(binds: Bindings) = {
    var changed = false
    def doit(atoms: List[Strategy]): List[Strategy] =
      if (atoms.isEmpty) List[Strategy]() else atoms.head.rewrite(binds) match {
        case (newstrategy: Strategy, true) =>
          changed = true
          newstrategy :: doit(atoms.tail)
        case _ =>
          atoms.head :: doit(atoms.tail)
      }
    val newchildren = doit(children)
    if (changed) (OrElseStrategy(newchildren), true) else (this, false)
  }
}

/**
 * Apply a strategy and examine the returned flag to determine how to proceed.
 * If it is true, apply a second strategy (the "yes" strategy).  If the flag is
 * false, apply a third strategy (the "no" strategy).
 * 
 * The syntax is:
 * {{{
 * { if S1, S2, S3 }
 * }}}
 * where `S1`, `S2`, and `S3` are the test, yes, and no strategies, in order.
 * 
 * @param test	The test strategy.
 * @param yes		The strategy to apply when the test returns true.
 * @param no		The strategy to apply when the test returns false.
 */
case class IfThenElseStrategy(test: Strategy, yes: Strategy, no: Strategy)
extends Strategy {
  val deBruijnIndex =
    test.deBruijnIndex max yes.deBruijnIndex max no.deBruijnIndex
  val isConstant = test.isConstant && yes.isConstant && no.isConstant
  def apply(atom: BasicAtom, binds: Bindings) = {
    // Apply the first strategy and check the result.
    var (newatom, flag) = test.apply(atom, binds)
    if (flag) yes.apply(newatom, binds)
    else no.apply(newatom, binds)
  }
  def toParseString = "{ if " + test.toParseString + "," + yes.toParseString +
		"," + no.toParseString + "}"
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    subject match {
    case IfThenElseStrategy(otest, oyes, ono) =>
      SequenceMatcher.tryMatch(Seq(test, yes, no), Seq(otest, oyes, ono), binds)
    case _ => Fail("Strategies do not match.", this, subject)
  }
  def rewrite(binds: Bindings) = {
    val (newtest, ct) = test.rewrite(binds)
    val (newyes, cy) = yes.rewrite(binds)
    val (newno, cn) = no.rewrite(binds)
    if (ct || cy || cn)
      (IfThenElseStrategy(newtest, newyes, newno), true) else (this, false)
  }
}

/**
 * Apply a strategy, but only to atoms labeled with the specific key.
 * 
 * The syntax is:
 * {{{
 * { @K S }
 * }}}
 * where `K` is the key and `S` is the strategy to apply.
 * 
 * @param key		The key to match.
 * @param child	The strategy to apply.
 */
//case class KeyStrategy(key: String, child: Strategy) extends Strategy

/**
 * Apply a strategy, but only to an atom's children labeled with a specified
 * key.  If the key is omitted, then the strategy is applied to all children,
 * unconditionally.
 * 
 * The syntax is:
 * {{{
 * { @@K S }
 * { @@ S }
 * }}}
 * where `K` is the key and `S` is the strategy to apply.
 * 
 * @param key		The key to match.
 * @param child	The strategy to apply.
 */
//case class ChildKeyStrategy(key: Option[String], child: Strategy)
//extends Strategy
