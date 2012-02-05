/* Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 */
package sjp.elision.core

import scala.collection.mutable.{Map => MMap, BitSet, ListBuffer}

/**
 * Indicate an attempt to use an undeclared ruleset.
 * @param msg		A human readable message.
 */
case class NoSuchRulesetException(msg: String) extends Exception(msg)

/**
 * A context provides access to operator libraries and rules.
 * @param allowUndeclared	Iff true, allow the use of undeclared rulesets.
 */
class Context(val allowUndeclared:Boolean = false) {
  
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
  private val _active = new BitSet()

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
  _rs2bit += ("default" -> 0)
  
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
        else throw NoSuchRulesetException(
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
   * Add a rewrite rule to this context.
   * @param rule	The rewrite rule to add.
   * @throws	NoSuchRulesetException
   * 					At least one ruleset mentioned in the rule has not been declared,
   * 					and undeclared rulesets are not allowed.
   */
  def add(rule: RewriteRule) = {
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
  private def getRuleList(atom: BasicAtom) =
    _kind2rules.getOrElseUpdate(atom.getClass(),
        ListBuffer[(BitSet, RewriteRule)]())
        
  /**
   * Get the list of rules that apply to the given atom and which are in any
   * of the currently active rulesets.
   * @param atom	The atom to which the rule may apply.
   * @return	A list of rules.
   */
  def getRules(atom: BasicAtom) =
    for ((bits, rule) <- getRuleList(atom) ; if ((bits & _active) != 0))
      yield rule
}