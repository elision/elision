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
======================================================================*/
package ornl.elision.core

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap, BitSet, ListBuffer}
import ornl.elision.ElisionException
import ornl.elision.repl.ReplActor

/**
 * Indicate an attempt to use an undeclared ruleset.
 * 
 * @param msg		A human readable message.
 */
class NoSuchRulesetException(msg: String) extends ElisionException(msg)

/**
 * A ruleset reference.
 */
abstract class RulesetRef extends BasicAtom with Rewriter {
  val depth = 0
  val deBruijnIndex = 0
  val constantPool = None
  val isTerm = true
  val isConstant = true
  val theType = RSREF
  /** The name of the referenced ruleset. */
  val name: String
  
  def toParseString = toESymbol(name) + ":RSREF"
  
  /**
   * Ruleset references cannot be rewritten.
   */
  def rewrite(binds: Bindings) = (this, false)
  
  override def hashCode = 61*name.hashCode
  
  override def equals(other: Any) =
    (other match {
      case rr:RulesetRef => rr.name == name
      case _ => false
    })
}

/**
 * Creation and matching of ruleset references.
 */
object RulesetRef {
  /**
   * Extract the parts of a ruleset reference.
   * 
   * @param rr		The reference.
   * @return	The ruleset name.
   */
  def unapply(rr: RulesetRef) = Some((rr.name))
  
  /**
   * Make a new reference to the named ruleset in the rule library of the given
   * context.
   * 
   * @param context		The context.
   * @param name			The ruleset name.
   * @return	The new reference.
   */
  def apply(context: Context, name: String) =
    context.ruleLibrary.makeRulesetRef(name)
    
  /**
   * Make a new reference to the named ruleset in the given rule library.
   * 
   * @param library		The rule library.
   * @param name			The ruleset name.
   * @return	The new reference.
   */
  def apply(library: RuleLibrary, name: String) =
    library.makeRulesetRef(name)
}

/**
 * Encapsulate a rule library.
 * 
 * == Purpose ==
 * A rule library instance contains rules and methods for accessing relevant
 * rules and for performing "automated" rewriting.
 * 
 * Rules can be organized into rulesets.  A single rule may be in multiple
 * rulesets.  A rule can also be placed in no rulesets, though this is not
 * advisable, as it is difficult to access the rule later.  In fact, the
 * only way to see rules that are not part of any active ruleset is via the
 * `getRuleList` method.
 * 
 * The rule library also manages the rulesets.  These must be declared before
 * they can be used, and they can be enabled and disabled to better control
 * rewriting.
 * 
 * @param allowUndeclared	Iff true, allow the use of undeclared rulesets.
 */
class RuleLibrary(val allowUndeclared:Boolean = false)
extends Fickle with Mutable {
  
  //======================================================================
  // Controlling active rulesets.
  //======================================================================

  /** The active rulesets. */
  val _active = new BitSet()

  /**
   * Enable a ruleset.
   * 
   * @param name	The name of the ruleset to enable.
   * @return	This context.
   */
  def enableRuleset(name: String) = _active += getRulesetBit(name) ; this
  
  /**
   * Disable a ruleset.
   * 
   * @param name	The name of the ruleset to disable.
   * @return	This context.
   */
  def disableRuleset(name: String) = _active -= getRulesetBit(name) ; this
  
  //======================================================================
  // Controlling automatic rewriting.
  //======================================================================

  /** The rewrite limit.  Negative numbers mean *no* limit. */
  private var _limit: BigInt = 10000000
  
  /** Whether to recursively rewrite children. */
  private var _descend = true
  
  /**
   * Set the limit for the number of rewrites.  Use a negative number
   * to indicate no rewrites, and zero to turn off rewriting.
   * 
   * @param limit	The limit of the number of rewrites.
   * @return	This context.
   */
  def setLimit(limit: BigInt) = _limit = limit ; this
  
  /**
   * Set whether to rewrite children recursively.
   * 
   * @param descend	Whether to rewrite children recursively.
   * @return	This context.
   */
  def setDescend(descend: Boolean) = _descend = descend ; this
  
  //======================================================================
  // Rewriting.
  //======================================================================

  
  /*
  def rewriteOnce(atom: BasicAtom): (BasicAtom, Boolean) = {
    var (newtop, appliedtop) = rewriteTop(atom)
    if (_descend) {
	    var (newatom, applied) = rewriteChildren(newtop)
	    (newatom, appliedtop || applied)
    } else {
      (newtop, appliedtop)
    }
  }
  
  def rewriteTop(atom: BasicAtom): (BasicAtom, Boolean) = {
    // Get the rules.
    val rules = getRules(atom)
    // Now try every rule until one applies.
    for (rule <- rules) {
      val (newatom, applied) = rule.doRewrite(atom)
      if (applied) return (newatom, applied)
    }
    return (atom, false)
  }
  
  def rewriteChildren(atom: BasicAtom): (BasicAtom, Boolean) = atom match {
	  case AtomSeq(props, atoms) =>
	    var flag = false
	    (AtomSeq(props, atoms.map { atom =>
	      val (newatom, applied) = rewriteOnce(atom)
	      flag ||= applied
	      newatom
	    }), flag)
	  case Apply(op, AtomSeq(props, atoms)) =>
	    var flag = false
	    (Apply(op, AtomSeq(props, atoms.map { atom =>
	      val (newatom, applied) = rewriteOnce(atom)
	      flag ||= applied
	      newatom
	    })), flag)
	  case Apply(lhs, rhs) =>
	    val newlhs = rewriteOnce(lhs)
	    val newrhs = rewriteOnce(rhs)
	    (Apply(newlhs._1, newrhs._1), newlhs._2 || newrhs._2)
	  case _ =>
	    // Do nothing in this case.
	    (atom, false)
  }
  */
  

  
  /**
   * Rewrite the provided atom once, if possible.  Children may be rewritten,
   * depending on whether descent is enabled.
   * 
   * @param atom      The atom to rewrite.
   * @param rulesets  The rulesets to use, or `Set.empty` to use all enabled.
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  def rewriteOnce(atom: BasicAtom, rulesets: Set[String]): (BasicAtom, Boolean) = {
    var (newtop, appliedtop) = rewriteTop(atom, rulesets)
    if (_descend) {
      var (newatom, applied) = rewriteChildren(newtop, rulesets)
      (newatom, appliedtop || applied)
    } else {
      (newtop, appliedtop)
    }
  }
  
  /**
   * Rewrite the atom at the top level, once.
   * 
   * @param atom      The atom to rewrite.
   * @param rulesets  The rulesets to use, or `Set.empty` to use all enabled.
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  def rewriteTop(atom: BasicAtom, rulesets: Set[String]): (BasicAtom, Boolean) = {
    // Get the rules.
    val rules = if (rulesets.isEmpty) getRules(atom) else getRules(atom, rulesets)
    
    // Now try every rule until one applies.
    for (rule <- rules) {
      val (newatom, applied) = rule.doRewrite(atom)
      if (applied) {
        return (newatom, applied)
      }
    }
    return (atom, false)
  }
  
  /**
   * Recursively rewrite the atom and its children.  This method understands
   * atom collections and operators.
   * 
   * @param atom      The atom to rewrite.
   * @param rulesets  The rulesets to use, or `Set.empty` to use all enabled.
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  def rewriteChildren(atom: BasicAtom, rulesets: Set[String]): (BasicAtom, Boolean) = {
    atom match {
      case AtomSeq(props, atoms) =>
        var flag = false
        // Rewrite the properties.  The result must still be a property spec.
        // If not, we keep the same properties.
        val newProps = rewriteOnce(props, rulesets) match {
          case (ap: AlgProp, true) => flag = true; ap
          case _ => props
        }
        // Rewrite the atoms.
        val newAtoms = atoms.map {
          atom =>
            val (newatom, applied) = rewriteOnce(atom, rulesets)
            flag ||= applied
            newatom
        }
        // Return the result.
        if (flag) (AtomSeq(newProps, newAtoms), true)
        else (atom, false)
        
      case Apply(lhs, rhs) =>
        val newlhs = rewriteOnce(lhs, rulesets)
        val newrhs = rewriteOnce(rhs, rulesets)
        if (newlhs._2 || newrhs._2) (Apply(newlhs._1, newrhs._1), true)
        else (atom, false)
        
      case Lambda(param, body) =>
        val newparam = rewriteOnce(param, rulesets) match {
          case (v: Variable, true) => (v, true)
          case _ => (param, false)
        }
        val newbody = rewriteOnce(body, rulesets)
        if (newparam._2 || newbody._2) (Lambda(newparam._1, newbody._1), true)
        else (atom, false)

      case SpecialForm(tag, content) =>
        val newlhs = rewriteOnce(tag, rulesets)
        val newrhs = rewriteOnce(content, rulesets)
        if (newlhs._2 || newrhs._2) (SpecialForm(newlhs._1, newrhs._1), true)
        else (atom, false)

      case _ =>
        // Do nothing in this case.
        (atom, false)
    }
  }

  /**
   * Rewrite the given atom, repeatedly applying the rules of the active
   * rulesets.  This is limited by the rewrite limit.
   * 
   * @param atom      The atom to rewrite.
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
   
 // def rewrite(atom: BasicAtom) = doRewrite(atom, Set.empty)
    
    //////////////////// GUI changes
    
    def rewrite(atom: BasicAtom) = {
    ReplActor ! ("Eva","pushTable", "RuleLibrary rewrite")
    // top node of this subtree
    ReplActor ! ("Eva", "addToSubroot", ("rwNode", "RuleLibrary rewrite: ", atom)) // val rwNode = RWTree.addToCurrent("RuleLibrary rewrite: ", atom)
    ReplActor ! ("Eva", "setSubroot", "rwNode") // RWTree.current = rwNode
    
    val tempDisabled = ReplActor.disableGUIComs
    
    if(ReplActor.disableRuleLibraryVis) ReplActor.disableGUIComs = true
    val (newatom, flag) = doRewrite(atom, Set.empty)
    ReplActor.disableGUIComs = tempDisabled
    if(flag) ReplActor ! ("Eva", "addTo", ("rwNode", "", newatom)) // RWTree.addTo(rwNode,newatom)
    
    ReplActor ! ("Eva", "popTable", "RuleLibrary rewrite")
    (newatom, flag)
  }
    
    //////////////////// end GUI changes

    //////////////////// GUI changes
  /**
   * Rewrite the given atom, repeatedly applying the rules of the active
   * rulesets.  This is limited by the rewrite limit.
   * 
   * @param atom      The atom to rewrite.
   * @param rulesets  The rulesets to use, or `Set.empty` to use all enabled.
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  def rewrite(atom: BasicAtom, rulesets: Set[String]) = {
    ReplActor ! ("Eva","pushTable", "RuleLibrary rewrite")
    // top node of this subtree
    ReplActor ! ("Eva", "addToSubroot", ("rwNode", "RuleLibrary rewrite: ", atom)) // val rwNode = RWTree.addToCurrent("RuleLibrary rewrite: ", atom)
    ReplActor ! ("Eva", "setSubroot", "rwNode") // RWTree.current = rwNode
    
    val tempDisabled = ReplActor.disableGUIComs
    
    if(ReplActor.disableRuleLibraryVis) ReplActor.disableGUIComs = true
    val (newatom, flag) = doRewrite(atom, rulesets)
    ReplActor.disableGUIComs = tempDisabled
    ReplActor ! ("Eva", "addTo", ("rwNode", "", newatom)) // RWTree.addTo(rwNode,newatom)
    
    ReplActor ! ("Eva", "popTable", "RuleLibrary rewrite")
    (newatom, flag)
  }
  //////////////////// end GUI changes

  /**
   * Rewrite the given atom, repeatedly applying the rules of the active
   * rulesets.  This is limited by the rewrite limit.
   * 
   * @param atom      The atom to rewrite.
   * @param rulesets  The rulesets to use, or `Set.empty` to use all enabled.
   * @param bool      Flag used for tracking whether any rules have succeeded.
   * @param limit     The remaining rewrite limit.  Use a negative number for
   *                  no limit.
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  @tailrec
  private def doRewrite(atom: BasicAtom, rulesets: Set[String] = Set.empty,
      bool: Boolean = false, limit: BigInt = _limit): (BasicAtom, Boolean) = {
    if (limit == 0) return (atom, bool)
    else rewriteOnce(atom, rulesets) match {
      case (newatom, false) =>
        return (newatom, bool)
      case (newatom, true) =>
        return doRewrite(newatom, rulesets, true,
            if(limit > 0) limit-1 else limit)
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
   * 
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
   * 
   * @param name	The name of the new ruleset.
   * @return	True if the ruleset was declared, and false if it was already
   * 					(previously) declared.
   */
  def declareRuleset(name: String) =
    _rs2bit.get(name) match {
      case None => _rs2bit += (name -> bump()) ; true
      case _ => false
    }
  
  //======================================================================
  // Ruleset reference.
  //======================================================================
  
  /**
   * Make a ruleset reference.
   * 
   * @param name		The name of the ruleset.
   */
  def apply(name: String): RulesetRef = new _RulesetRef(name)
  
  /**
   * Make a ruleset reference.
   * 
   * @param name		The name of the ruleset.
   */
  def makeRulesetRef(name: String): RulesetRef = new _RulesetRef(name)
  
  /**
   * Implementation of ruleset references.
   * 
   * @param name		The name of the referenced ruleset.
   */
  private class _RulesetRef(val name: String) extends RulesetRef {
    /** The bit for the referenced ruleset. */
    val bit = getRulesetBit(name)
    
    /**
     * Apply this strategy. If any rule completes then the returned flag is
     * true. Otherwise it is false.
     */
    def doRewrite(atom: BasicAtom, hint: Option[Any]): (BasicAtom, Boolean) = {
	    // Get the rules.
	    val rules = getRules(atom, Set(name))
	    // Now try every rule until one applies.
	    for (rule <- rules) {
	      val (newatom, applied) = rule.doRewrite(atom, hint)
	      if (applied) return (newatom, applied)
	    }
	    return (atom, false)
	  }
    
//	//////////////////// GUI changes
//	  def doRewrite(atom: BasicAtom, hint: Option[Any]): (BasicAtom, Boolean) = {
//		// get the node representing this atom that is being rewritten
//		val rwNode = RWTree.current.addChild("_RulesetRef doRewrite: ")
//	    val atomNode = rwNode.addChild(atom)
//		// Get the rules.
//	    val rules = getRules(atom, List(name))
//		val rulesNode = atomNode.addChild("rules: ")
//		
//	    // Now try every rule until one applies.
//	    for (rule <- rules) {
//			val ruleNode = rulesNode.addChild(rule)
//			RWTree.current = ruleNode
//	      val (newatom, applied) = rule.doRewrite(atom, hint)
//		  ruleNode.addChild(newatom)
//		  
//	      if (applied) {
//				atomNode.addChild(newatom)
//				return (newatom, applied)
//			}
//	    }
//	    return (atom, false)
//	  }
//	  //////////////////// end GUI changes
    
	  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings, hints: Option[Any]) =
	    if (subject == this) Match(binds)
	    else subject match {
	      case rr:RulesetRef if (rr.name == name) => Match(binds)
	      case _ => Fail("Ruleset reference does not match subject.", this, subject)
	    }
  }
  
  //======================================================================
  // Rule management.
  //======================================================================
  
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
   * Add a rewrite rule to this context.
   * 
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
   * 
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
   * 
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
   * 
   * @param atom	The atom to which the rule may apply.
   * @return	A list of rules.
   */
  def getRules(atom: BasicAtom) =
    for ((bits, rule) <- getRuleList(atom) ; if (!(bits & _active).isEmpty))
      yield rule
      
  /**
   * Get the list of rules that apply to the given atom and which are in any
   * of the specified rulesets.
   * 
   * @param atom	The atom to which to the rules may apply.
   * @param name	The ruleset names.
   * @return	A list of rules.
   */
  def getRules(atom: BasicAtom, names: Set[String]) = {
    val rsbits = names.foldLeft(new BitSet())(_ += getRulesetBit(_))
    for ((bits, rule) <- getRuleList(atom); if (!(bits & rsbits).isEmpty))
      yield rule
  }
  
  //======================================================================
  // Strings.
  //======================================================================

  /**
   * Generate a newline-separated list of rules that can be parsed using the
   * atom parser to reconstruct the set of rules in this context.
   * 
   * @return	The parseable rule sets.
   */
  def toParseString = {
    val buf = new StringBuilder
    for ((kind,list) <- _kind2rules) {
      buf append ("// Rules for " + kind.toString + ".\n")
      buf append list.map(_._2).mkParseString("","\n","\n")
    }
    for ((name,list) <- _op2rules) {
      buf append ("// Rules for operator " + name + ".\n")
      buf append list.map(_._2).mkParseString("","\n","\n")
    }
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
    buf append "def _mkrulelib(_context: Context) {\n"
    buf append("  val rulelib = _context.ruleLibrary\n")
    for ((_,list) <- _kind2rules) {
      buf append list.map("  rulelib.add(" + _._2 + ")").mkString("","\n","\n")
    }
    for ((_,list) <- _op2rules) {
      buf append list.map("  rulelib.add(" + _._2 + ")").mkString("","\n","\n")
    }
    buf append "}\n"
    buf.toString()
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
      case Apply(op:Operator, as:AtomSeq) => {
        // Extract the operator properties.
        val props = op match {
          case po: SymbolicOperator => po.params.props
          case _ => NoProps
        }
        
        // If the operator is not associative, we don't need to do anything.
        if (!props.isA(false)) {
          return list
        }
        
        // The operator is associative.  We must at least add an argument on
        // the right-hand side.  Make and add the argument, and then add the
        // synthetic rule.
        var right = Variable(as(0).theType, "::R")
        var newpatternlist = as.atoms :+ right
        var newrewritelist = OmitSeq[BasicAtom](rewrite) :+ right
        list :+= RewriteRule(Apply(op, AtomSeq(props, newpatternlist)),
            Apply(op, AtomSeq(props, newrewritelist)),
            rule.guards, rule.rulesets, true)
        
        // If the operator is commutative, we are done.
        if (props.isC(false)) {
          return list
        }
        
        // Repeat the above to add an argument on the left-hand side.
        var left = Variable(as(0).theType, "::L")
        newpatternlist = left +: as.atoms
        newrewritelist = left +: OmitSeq[BasicAtom](rewrite)
        list :+= RewriteRule(Apply(op, AtomSeq(props, newpatternlist)),
            Apply(op, AtomSeq(props, newrewritelist)),
            rule.guards, rule.rulesets, true)
            
        // And again add the argument on the right-hand side.
        newpatternlist = newpatternlist :+ right
        newrewritelist = newrewritelist :+ right
        list :+= RewriteRule(Apply(op, AtomSeq(props, newpatternlist)),
            Apply(op, AtomSeq(props, newrewritelist)),
            rule.guards, rule.rulesets, true)
            
        // Done.
        return list
      }
      case _ => return list
    }
  }
}
