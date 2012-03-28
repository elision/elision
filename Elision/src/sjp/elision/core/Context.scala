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

import scala.collection.mutable.{Map => MMap, BitSet, ListBuffer}
import sjp.elision.ElisionException

/**
 * Indicate an attempt to use an undeclared ruleset.
 * @param msg		A human readable message.
 */
class NoSuchRulesetException(msg: String) extends ElisionException(msg)

/**
 * Indicate an attempt to re-define a strategy.
 * @param msg		A human readable message.
 */
class StrategyRedefinitionException(msg: String) extends ElisionException(msg)

/**
 * A context provides access to operator libraries and rules, along with
 * the global set of bindings in force at any time.
 * 
 * @param allowUndeclared	Iff true, allow the use of undeclared rulesets.
 */
class Context(val allowUndeclared:Boolean = false) {
  
  //======================================================================
  // Global bindings management.
  //======================================================================
  
  /** The current bindings. */
  private var _binds: Bindings = Bindings()
  
  /**
   * Bind a variable in this context.
   * 
   * @param vname		The variable name to bind.
   * @param atom		The atom to bind to the variable.
   * @return	This context.
   */
  def bind(vname: String, atom: BasicAtom) = {
    _binds += (vname -> atom)
    this
  }
  
  /**
   * Unbind a variable in this context.
   * 
   * @param vname		The variable name.
   * @return	This context.
   */
  def unbind(vname: String) = {
    _binds -= vname
    this
  }
  
  /**
   * Get the current bindings for this context.
   * 
   * @return	The bindings for this context.
   */
  def binds = _binds
  
  //======================================================================
  // Operator library management.
  //======================================================================
  
  /** The current operator library. */
  private var _oplib: OperatorLibrary = _
  
  /**
   * Get the current operator library.  If none has explicitly been set, then
   * a default instance is created and returned.
   * @return	The current operator library.
   */
  def operatorLibrary = {
    if (_oplib == null) { _oplib = new OperatorLibrary() }
    _oplib
  }
    
  /**
   * Set the operator library to use.  Any prior value is lost.
   * @param lib	The new operator library.
   * @return	This context.
   */
  def operatorLibrary_=(lib: OperatorLibrary) = {
    require(lib != null)
    _oplib = lib
    this
  }
  
  //======================================================================
  // Controlling active rulesets.
  //======================================================================

  /** The active rulesets. */
  val _active = new BitSet()

  /**
   * Enable a ruleset.
   * @param name	The name of the ruleset to enable.
   * @return	This context.
   */
  def enableRuleset(name: String) = _active += getRulesetBit(name) ; this
  
  /**
   * Disable a ruleset.
   * @param name	The name of the ruleset to disable.
   * @return	This context.
   */
  def disableRuleset(name: String) = _active -= getRulesetBit(name) ; this
  
  //======================================================================
  // Rewriting.
  //======================================================================

  /** The rewrite limit. */
  private var _limit: BigInt = 100
  
  /**
   * Set the limit for the number of rewrites.
   * @param limit	The limit of the number of rewrites.
   * @return	This context.
   */
  def setLimit(limit: BigInt) = _limit = limit ; this
  
  /**
   * Rewrite the provided atom once, if possible.
   * @param atom	The atom to rewrite.
   * @return	The rewritten atom, and true iff any rules were successfully
   * 					applied.
   */
  def rewriteOnce(atom: BasicAtom): (BasicAtom, Boolean) = {
    // Get the rules.
    val rules = getRules(atom)
    // Now try every rule until one applies.
    for (rule <- rules) {
      val (newatom, applied) = rule.tryRewrite(atom)
      if (applied) return (newatom, applied)
    }
    return (atom, false)
  }
  
  /**
   * Rewrite the given atom, repeatedly applying the rules of the active
   * rulesets.  This is limited by the rewrite limit.
   * @param atom	The atom to rewrite.
   */
  def rewrite(atom: BasicAtom) = doRewrite(atom)

  /**
   * Rewrite the given atom, repeatedly applying the rules of the active
   * rulesets.  This is limited by the rewrite limit.
   * @param atom	The atom to rewrite.
   * @param bool	Flag used for tracking whether any rules have succeeded.
   * @param limit	The remaining rewrite limit.
   */
  private def doRewrite(atom: BasicAtom, bool: Boolean = false,
      limit: BigInt = _limit): (BasicAtom, Boolean) = {
    if (limit <= 0) return (atom, bool)
    else rewriteOnce(atom) match {
      case (newatom, false) => (newatom, bool)
      case (newatom, true) => doRewrite(newatom, true, limit-1)
    }
  }
  
  //======================================================================
  // Ruleset management.
  //======================================================================

  /**
   * A map from ruleset names to integers indicating the rulesets position
   * in the bitsets.
   */
  private val _rs2bit = MMap[String,Int]()
  
  /** Bit index of the next ruleset. */
  private var _nextrs = 1
  
  /** Local convenience method to get the next ruleset index. */
  private def bump() = { val tmp = _nextrs ; _nextrs += 1 ; tmp }
  
  /** Bit zero is reserved for the default ruleset. */
  _rs2bit += ("DEFAULT" -> 0)
  
  /** The default ruleset is on by default. */
  enableRuleset("DEFAULT")
  
  /**
   * Get the bit for a ruleset.
   * @param name	The ruleset name.
   * @return	The bit for the ruleset.
   * @throws	NoSuchRulesetException
   * 					The ruleset has not been declared, and undeclared rulesets are
   * 					not allowed.
   */
  private def getRulesetBit(name: String) =
    _rs2bit.getOrElseUpdate(name, (
        if (allowUndeclared) bump()
        else throw new NoSuchRulesetException(
            "The ruleset " + name + " has not been declared.")))
  
  /**
   * Declare the ruleset.
   * @param name	The name of the new ruleset.
   * @return	True if the ruleset was declared, and false if it was already
   * 					(previously) declared.
   */
  def declareRuleset(name: String) =
    _rs2bit.get(name) match {
      case None => _rs2bit += (name -> bump()) ; true
      case _ => false
    }
  
  /**
   * Map each kind of atom to a list of rules for rewriting that atom.  The
   * rules are ordered, and each has an associated bit set that tells which
   * rulesets the rule is in.
   */
  private val _kind2rules = MMap[Class[_],ListBuffer[(BitSet,RewriteRule)]]()
  
  /**
   * Map each operator to a list of rules for rewriting terms with that
   * operator at the root.  The rules are ordered, and each has an associated
   * bit set that tells which rulesets the rule is in.
   */
  private val _op2rules = MMap[String,ListBuffer[(BitSet,RewriteRule)]]()
  
  /**
   * Generate a newline-separated list of rules that can be parsed using the
   * atom parser to reconstruct the set of rules in this context.
   * 
   * @return	The parseable rule sets.
   */
  def toParseString = {
    val buf = new StringBuilder
    buf append "// START of context.\n"
    buf append "// Operator library.\n"
    buf append operatorLibrary.toParseString
    for ((kind,list) <- _kind2rules) {
      buf append ("// Rules for " + kind.toString + ".\n")
      buf append list.map(_._2).mkParseString("","\n","\n")
    }
    for ((name,list) <- _op2rules) {
      buf append ("// Rules for operator " + name + ".\n")
      buf append list.map(_._2).mkParseString("","\n","\n")
    }
    buf append "// END of context.\n"
    buf.toString()
  }
  
  /**
   * Generate a newline-separated list of rules that can be parsed by Scala
   * to reconstruct the set of rules in this context.
   * 
   * @return	The parseable rule sets.
   */
  override def toString = {
    val buf = new StringBuilder
    buf append operatorLibrary.toString
    for ((_,list) <- _kind2rules) {
      buf append list.map(_._2).mkString("","\n","\n")
    }
    for ((_,list) <- _op2rules) {
      buf append list.map(_._2).mkString("","\n","\n")
    }
    buf.toString()
  }
  
  /**
   * Add a rewrite rule to this context.
   * @param rule	The rewrite rule to add.
   * @throws	NoSuchRulesetException
   * 					At least one ruleset mentioned in the rule has not been declared,
   * 					and undeclared rulesets are not allowed.
   */
  def add(rule: RewriteRule) = {
    // Complete the rule.
    for (rule2 <- Completor.complete(rule)) doAdd(rule2)
    this
  }
  
  /**
   * Add a rewrite rule to this context.
   * @param rule	The rewrite rule to add.
   * @throws	NoSuchRulesetException
   * 					At least one ruleset mentioned in the rule has not been declared,
   * 					and undeclared rulesets are not allowed.
   */
  private def doAdd(rule: RewriteRule) = {
    // Figure out what rulesets this rule is in.  We build the bitset here.
    val bits = new BitSet()
    for (rs <- rule.rulesets) bits += getRulesetBit(rs)
    // Get (or create) the list for the kind of atom the rule's pattern uses.
    val list = getRuleList(rule.pattern)
    // Okay, now add the rule to the list.  We perform no checking to see if
    // the rule is already present.
    list += Pair(bits, rule)
    this
  }
  
  /**
   * Helper method to get all rules for a particular kind of atom.
   * @param atom	The atom.
   * @return	The list of rules for the given kind of atom.
   */
  private def getRuleList(atom: BasicAtom) = atom match {
    case Apply(op:Operator, _) =>
      _op2rules.getOrElseUpdate(op.name, ListBuffer[(BitSet, RewriteRule)]())
    case _ =>
	    _kind2rules.getOrElseUpdate(atom.getClass(),
	        ListBuffer[(BitSet, RewriteRule)]())
  }
        
  /**
   * Get the list of rules that apply to the given atom and which are in any
   * of the currently active rulesets.
   * @param atom	The atom to which the rule may apply.
   * @return	A list of rules.
   */
  def getRules(atom: BasicAtom) =
    for ((bits, rule) <- getRuleList(atom) ; if (!(bits & _active).isEmpty))
      yield rule
      
  /**
   * Get the list of rules that apply to the given atom and which are in any
   * of the specified rulesets.
   * @param atom	The atom to which to the rules may apply.
   * @param name	The ruleset names.
   * @return	A list of rules.
   */
  def getRules(atom: BasicAtom, names: List[String]) = {
    val rsbits = names.foldLeft(new BitSet())(_ += getRulesetBit(_))
    for ((bits, rule) <- getRuleList(atom); if (!(bits & rsbits).isEmpty))
      yield rule
  }
}

/**
 * Generate synthetic rules based on the provided rule, if necessary.
 * 
 * Synthetic rules are required when a rule pattern's root is an associative
 * operator.  There are two cases.
 * 
 * If the operator is both associative and commutative, then one synthetic rule
 * is constructed by adding an additional argument to the right-hand side of
 * the argument list in both the pattern and the rewrite.
 * 
 * Example:
 * {{{
 * { rule and($x, not($x)) -> false }
 * }}}
 * Synthetic Rule:
 * {{{
 * { rule and($x, not($x), $r) -> and(false, $r) }
 * }}}
 * (The rewrite in the above rule is of course reduced to `false`.)
 * 
 * If the operator is associative but not commutative, then we must add three
 * synthetic rules that add new arguments to either end of both the pattern
 * and rewrite.
 * 
 * Example:
 * {{{
 * { rule concat($x, inv($x)) -> "" }
 * }}}
 * Synthetic Rules:
 * {{{
 * { rule concat($l, $x, inv($x)) -> concat($l, "") }
 * { rule concat($x, inv($x), $r) -> concat("", $r) }
 * { rule concat($l, $x, inv($x), $r) -> concat($l, "", $r) }
 * }}}
 */
private object Completor {
  /**
   * Perform partial completion for the given rule by generating the necessary
   * synthetic rules.
   * 
   * @param rule	The provided rule.
   * @return	A list of rules, including the original rule and any synthetic
   * 					rules.
   */
  def complete(rule: RewriteRule): List[RewriteRule] = {
    // Make a new list to hold the rules, and add the original rule.
    var list = List[RewriteRule](rule)
    
    // Extract the pattern and rewrite, and then check the pattern to see if
    // it uses an operator.
    val pattern = rule.pattern
    val rewrite = rule.rewrite
    pattern match {
      case Apply(op:Operator, al:AtomList) => {
        // Extract the operator properties.
        val props = op.opdef match {
          case iod: ImmediateOperatorDefinition => NoProps
          case sod: SymbolicOperatorDefinition => sod.props
        }
        
        // If the operator is not associative, we don't need to do anything.
        if (!props.associative.getOrElse(false)) {
          return list
        }
        
        // The operator is associative.  We must at least add an argument on
        // the right-hand side.  Make and add the argument, and then add the
        // synthetic rule.
        var right = Variable(al(0).theType, "::R")
        var newpatternlist = al.atoms :+ right
        var newrewritelist = OmitSeq[BasicAtom](rewrite) :+ right
        list :+= RewriteRule(Apply(op, AtomList(props, newpatternlist)),
            Apply(op, AtomList(props, newrewritelist)),
            rule.guards, rule.rulesets, rule.cacheLevel, true)
        
        // If the operator is commutative, we are done.
        if (props.commutative.getOrElse(false)) {
          return list
        }
        
        // Repeat the above to add an argument on the left-hand side.
        var left = Variable(al(0).theType, "::L")
        newpatternlist = left +: al.atoms
        newrewritelist = left +: OmitSeq[BasicAtom](rewrite)
        list :+= RewriteRule(Apply(op, AtomList(props, newpatternlist)),
            Apply(op, AtomList(props, newrewritelist)),
            rule.guards, rule.rulesets, rule.cacheLevel, true)
            
        // And again add the argument on the right-hand side.
        newpatternlist = newpatternlist :+ right
        newrewritelist = newrewritelist :+ right
        list :+= RewriteRule(Apply(op, AtomList(props, newpatternlist)),
            Apply(op, AtomList(props, newrewritelist)),
            rule.guards, rule.rulesets, rule.cacheLevel, true)
            
        // Done.
        return list
      }
      case _ => return list
    }
  }
}
