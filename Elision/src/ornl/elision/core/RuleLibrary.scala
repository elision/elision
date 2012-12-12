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

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap, BitSet, ListBuffer}
import ornl.elision.util.ElisionException
import ornl.elision.repl.ReplActor
import scala.collection.immutable.List
import scala.collection.immutable.HashSet
import ornl.elision.util.OmitSeq
import ornl.elision.util.other_hashify

/**
 * Indicate an attempt to use an undeclared ruleset.
 * 
 * @param msg		A human readable message.
 */
class NoSuchRulesetException(msg: String) extends ElisionException(msg)

/**
 * Indicate an attempt to add an identity rule.
 * 
 * @param msg   A human-readable message.
 */
class IdentityRuleException(msg: String) extends ElisionException(msg)

/**
 * Indicate an attempt to add a rule whose pattern is bindable (i.e., a
 * simple variable).
 * 
 * @param msg   A human-readable message.
 */
class BindablePatternException(msg: String) extends ElisionException(msg)

/**
 * Indicate an attempt to add a rule whose pattern is a literal, when such
 * are not allowed.
 * 
 * @param msg   A human-readable message.
 */
class LiteralPatternException(msg: String) extends ElisionException(msg)

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
  
  /**
   * Ruleset references cannot be rewritten.
   */
  def rewrite(binds: Bindings) = (this, false)
    
  override def hashCode = 61*name.hashCode
  lazy val otherHashCode = (name.toString).foldLeft(BigInt(0))(other_hashify)+1
  
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

  // needed for writing out the scala code to get the order of operations right
  // we prepend to the list for performance, so be sure to use a reverseIterator
  // to get the order. Contains rule additions, ruleset enabling, disabling, 
  // and declaring
  private var actionList = List[Action]()
  
  /**
   * Create a shallow clone of this rule library.  This returns a new rule
   * library instance; since the content of the library is immutable, this
   * new instance is technically independent of the original.
   * 
   * @return  The clone.
   */
  def cloneRuleLib : RuleLibrary = {
    val clone = new RuleLibrary(allowUndeclared)
    
    clone._kind2rules.clear
    for(mapping <- this._kind2rules) {
        clone._kind2rules += mapping
    }
    
    clone._op2rules.clear
    for(mapping <- this._op2rules) {
        clone._op2rules += mapping
    }
    
    clone._rs2bit.clear
    for(mapping <- this._rs2bit) {
        clone._rs2bit += mapping
    }
    
    clone._active.clear
    for(mapping <- this._active) {
        clone._active += mapping
    }

    for(mapping <- this._activeNames) {
        clone._activeNames += mapping
    }
    
    clone._nextrs = this._nextrs
    clone._descend = this._descend
    clone._limit = this._limit & this._limit
    
    clone
  }
  
  //======================================================================
  // Control what can be rewritten.
  //======================================================================
  
  /**
   * Should rules that rewrite literals be permitted.  If true, it is possible
   * to create rules that rewrite one literal to another.  In general this is
   * a bad idea.
   */
  var _allowLiteralRules = false
  
  //======================================================================
  // Controlling active rulesets.
  //======================================================================

  /** The active rulesets. */
  val _active = new BitSet()

  /** The active rulesets, by names. */
  var _activeNames : Set[String] = new HashSet[String]()

  /**
   * Enable a ruleset.
   * 
   * @param name	The name of the ruleset to enable.
   * @return	This context.
   */
  def enableRuleset(name: String) = {
    _active += getRulesetBit(name)
    _activeNames += name
    if (BasicAtom.traceRules) {
      println("Enable ruleset " + name + ". new rulesets = " + _activeNames)
    }
    this
  }
  
  /**
   * Disable a ruleset.
   * 
   * @param name	The name of the ruleset to disable.
  * @return	This context.
  */
  def disableRuleset(name: String) = {
    _active -= getRulesetBit(name)
    _activeNames -= name
    if (BasicAtom.traceRules) {
      println("Disable ruleset " + name + ". new rulesets = " + _activeNames)
    }
    this
  }
  
  //======================================================================
  // Controlling automatic rewriting.
  //======================================================================

  /** The rewrite limit.  Negative numbers mean *no* limit. */
  private var _limit: BigInt = 10000000
  
  /** Whether to recursively rewrite children. */
  private var _descend = true
  
  /**
   * The method to use to rewrite a child.  This is set by the
   * `setNormalizeChildren` method.
   */
  private var _rewritechild: (BasicAtom, Set[String]) => (BasicAtom, Boolean) =
    rewrite _
  
  /**
  * Set the limit for the number of rewrites.  Use a negative number
  * to indicate no rewrites, and zero to turn off rewriting.
  * 
  * @param limit	The limit of the number of rewrites.
  * @return	This rule library.
  */
  def setLimit(limit: BigInt) = { _limit = limit ; this }
  
  /**
  * Set whether to rewrite children recursively.
  * 
  * @param descend	Whether to rewrite children recursively.
  * @return	This rule library.
  */
  def setDescend(descend: Boolean) = { _descend = descend ; this }
  
  /**
   * Set whether to normalize children when rewriting.  The alternative is to
   * rewrite children once, recursively, and then return to the top level to
   * check it again.  The latter is a better strategy in general, but the
   * former results in the normalized form of children being stored in the
   * memoization cache.
   * 
   * Both should ultimately result in the same rewrite
   * if everything works correctly, but can result in different rewrites
   * under certain conditions.  Specifically, if an intermediate form of a
   * child causes a higher-level rewrite to occur, whereas the normalized form
   * of the child causes a different (or no) higher-level rewrite to occur,
   * then you will get different results.  This is really a problem with the
   * rules.
   * 
   * The default is to fully normalize children.
   * 
   * @param norm  Whether to normalize children.
   * @return  This rule library.
   */
  def setNormalizeChildren(norm: Boolean) = {
    if (norm) _rewritechild = rewrite _
    else _rewritechild = rewriteOnce _
    this
  }
  
  //======================================================================
  // Rewriting.
  //======================================================================
  
  /**
  * Rewrite the provided atom once, if possible.  Children may be rewritten,
  * depending on whether descent is enabled.
  * 
  * Do not memoize this method.
  * 
  * @param atom      The atom to rewrite.
  * @param rulesets  The rulesets to use, or `Set.empty` to use all enabled.
  * @return  The rewritten atom, and true iff any rules were successfully
  *          applied.
  */
  def rewriteOnce(atom: BasicAtom,
      rulesets: Set[String]): (BasicAtom, Boolean) = {

    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      return (atom, true)
    }

    var (newtop, appliedtop) = _rewriteTop(atom, rulesets)
    if (_descend) {
      var (newatom, applied) = _rewriteChildren(newtop, rulesets)     
      (newatom, appliedtop || applied)
    } else {
      (newtop, appliedtop)
    }
  }
  
  /**
  * Rewrite the atom at the top level, once.
  * 
  * Do not memoize this method.
  * 
  * @param atom      The atom to rewrite.
  * @param rulesets  The rulesets to use, or `Set.empty` to use all enabled.
  * @return  The rewritten atom, and true iff any rules were successfully
  *          applied.
  */
  private def _rewriteTop(atom: BasicAtom,
      rulesets: Set[String]): (BasicAtom, Boolean) = {

    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      return (atom, true)
    }

    // Get the rules.
    val rules = if (rulesets.isEmpty) getRules(atom) else getRules(atom, rulesets)

    if (BasicAtom.traceRules) {
      println("Rewriting atom '" + atom.toParseString + "'...")
    }
    // Now try every rule until one applies.
    for (rule <- rules) {
      if (BasicAtom.traceRules) {
        println("Trying rule '" + rule.toParseString + "'...")
      }
      val (newatom, applied) = rule.doRewrite(atom)
      if (applied) {

        // Return the rewrite result.
        return (newatom, applied)
      }
    } // Try all rules.

    return (atom, false)
  }
  
  /**
  * Recursively rewrite the atom and its children.  This method understands
  * atom collections and operators.
  * 
  * Do not memoize this method.
  * 
  * @param atom      The atom to rewrite.
  * @param rulesets  The rulesets to use, or `Set.empty` to use all enabled.
  * @return  The rewritten atom, and true iff any rules were successfully
  *          applied.
  */
  private def _rewriteChildren(atom: BasicAtom,
      rulesets: Set[String]): (BasicAtom, Boolean) = {

    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      return (atom, true)
    }

    // No timeout.
    else {
      atom match {
        case AtomSeq(props, atoms) =>
          var flag = false
          // Rewrite the properties.  The result must still be a property spec.
          // If not, we keep the same properties.
          val newProps = _rewritechild(props, rulesets) match {
            case (ap: AlgProp, true) => flag = true; ap
            case _ => props
          }
          // Rewrite the atoms.
          val newAtoms = atoms.map {
            atom =>
              val (newatom, applied) = _rewritechild(atom, rulesets)
              flag ||= applied
            newatom
          }
          // Return the result.
          if (flag) (AtomSeq(newProps, newAtoms), true) else (atom, false)
        
        case Apply(lhs, rhs) =>
          val newlhs = _rewritechild(lhs, rulesets)
          val newrhs = _rewritechild(rhs, rulesets)
          if (newlhs._2 || newrhs._2) {
            (Apply(newlhs._1, newrhs._1), true)
          } else {
            (atom, false)
          }
        
        case Lambda(param, body) =>
          val newparam = _rewritechild(param, rulesets) match {
            case (v: Variable, true) => (v, true)
              case _ => (param, false)
          }
          val newbody = _rewritechild(body, rulesets)
          if (newparam._2 || newbody._2) {
            (Lambda(newparam._1, newbody._1), true)
          } else {
            (atom, false)
          }

        case SpecialForm(tag, content) =>
          val newlhs = _rewritechild(tag, rulesets)
          val newrhs = _rewritechild(content, rulesets)
          if (newlhs._2 || newrhs._2) {
            (SpecialForm(newlhs._1, newrhs._1), true)
          } else {
            (atom, false)
          }

        case _ =>
          // Do nothing in this case.
          (atom, false)
      }
    }
  }

  /**
   * Rewrite the given atom, repeatedly applying the rules of the active
   * rulesets.  This is limited by the rewrite limit.
   * 
   * This method uses the memoization cache, if enabled.
   * 
   * @param atom      The atom to rewrite.
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  def rewrite(atom: BasicAtom) = {

    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      (atom, true)
    }

    // No timeout.
    else if (atom.isInstanceOf[Literal[_]] && !_allowLiteralRules) (atom, false)
    else if (atom.isInstanceOf[Variable]) (atom, false)
    else {
      ReplActor ! ("Eva","pushTable", "RuleLibrary rewrite")
      ReplActor ! ("Eva", "addToSubroot", ("rwNode", "RuleLibrary rewrite: ", atom))
      ReplActor ! ("Eva", "setSubroot", "rwNode")
      val tempDisabled = ReplActor.disableGUIComs
      if (ReplActor.disableRuleLibraryVis) ReplActor.disableGUIComs = true

      // Check the cache.
      var timedOut = false
      val (newatom, flag) = Memo.get(atom, _active) match {
        case None => {

          // Set the time at which rewriting the atom will time out, if we
          // are timing rewrites out.
          var pair: (ornl.elision.core.BasicAtom, Boolean) = null      
          if (BasicAtom.timingOut) {
            BasicAtom.timeoutTime.withValue(BasicAtom.computeTimeout) {
              pair = _doRewrite(atom, Set.empty)
              timedOut = BasicAtom.rewriteTimedOut
            }
          }
          else {
            pair = _doRewrite(atom, Set.empty)
            timedOut = false
          }
          Memo.put(atom, _active, pair._1, 0)
          Memo.put(pair._1, _active, pair._1, 0)
          pair
        }
        case Some(pair) => {
          if (BasicAtom.traceRules) {
            println("Got cached rewrite '" +
                    atom.toParseString + "' -> '" + pair._1.toParseString +
                    "' w. rulesets " + _activeNames)
          }
          pair
        }
      }

      ReplActor.disableGUIComs = tempDisabled
      if(flag) ReplActor ! ("Eva", "addTo", ("rwNode", "", newatom))
      ReplActor ! ("Eva", "popTable", "RuleLibrary rewrite")
      
      // Did rewriting this atom time out?
      if (timedOut) {
        
        // Return the original atom, with the rewrite flag set to true
        // to indicate a timeout.
        if (BasicAtom.traceRules) {
          println("Rewriting of atom '" + atom.toParseString + "' timed out.")
        }
        (atom, true)
      }
      
      // No timeout.
      else {
          
        // Return the rewrite results.
        (newatom, flag)
      }
    }
  }

  /**
   * Rewrite the given atom, repeatedly applying the rules of the specified
   * rulesets.  This is limited by the rewrite limit.
   * 
   * Perform rewriting of an atom given a collection of rulesets.
   * 
   * This method uses the memoization cache, if enabled.
   * 
   * @param atom      The atom to rewrite.
   * @param rulesets  The rulesets to use, or `Set.empty` to use all enabled.
   * @return  The rewritten atom, and true iff any rules were successfully
   *          applied.
   */
  def rewrite(atom: BasicAtom, rulesets: Set[String]) = {

    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      (atom, true)
    }

    // No timeout.
    else if (atom.isInstanceOf[Literal[_]] && !_allowLiteralRules) (atom, false)
    else if (atom.isInstanceOf[Variable]) (atom, false)
    else {
      ReplActor ! ("Eva","pushTable", "RuleLibrary rewrite")
      ReplActor ! ("Eva", "addToSubroot", ("rwNode", "RuleLibrary rewrite: ", atom))
      ReplActor ! ("Eva", "setSubroot", "rwNode")
      val tempDisabled = ReplActor.disableGUIComs

      if (ReplActor.disableRuleLibraryVis) ReplActor.disableGUIComs = true

      // Check the cache.
      val usedRulesetsBits = 
        if (rulesets.isEmpty) _active 
        else {
          var r = new BitSet()
          for (rs <- rulesets) r += getRulesetBit(rs)
          r
        }
      val usedRulesets = if (rulesets.isEmpty) _activeNames else rulesets
      var timedOut = false
      val (newatom, flag) = Memo.get(atom, usedRulesetsBits) match {
        case None =>

          // Set the time at which rewriting the atom will time out, if we
          // are timing rewrites out.
          var pair: (ornl.elision.core.BasicAtom, Boolean) = null
          if (BasicAtom.timingOut) {
            BasicAtom.timeoutTime.withValue(BasicAtom.computeTimeout) {
              pair = _doRewrite(atom, usedRulesets)
              timedOut = BasicAtom.rewriteTimedOut
            }
          }
          else {
            pair = _doRewrite(atom, usedRulesets)
            timedOut = false
          }
          Memo.put(atom, usedRulesetsBits, pair._1, 0)
          Memo.put(pair._1, usedRulesetsBits, pair._1, 0)
          pair
        case Some(pair) => {
            if (BasicAtom.traceRules) {
              println("Got cached rewrite '" +
                      atom.toParseString + "' -> '" + pair._1.toParseString +
                      "' w. rulesets " + usedRulesets)
            }
            pair
        }
      }
        
      ReplActor.disableGUIComs = tempDisabled
      ReplActor ! ("Eva", "addTo", ("rwNode", "", newatom))
      ReplActor ! ("Eva", "popTable", "RuleLibrary rewrite")
      
      // Did rewriting this atom time out?
      if (timedOut) {
        
        // Return the original atom, with the rewrite flag set to true
        // to indicate a timeout.
        if (BasicAtom.traceRules) {
          println("Rewriting of atom '" + atom.toParseString + "' timed out.")
        }
        (atom, true)
      }
      
      // No timeout.
      else {
        
        // Return the rewrite results.
        (newatom, flag)
      }
    }
  }

  /**
   * Rewrite the given atom, repeatedly applying the rules of the active
   * rulesets.  This is limited by the rewrite limit.
   * 
   * This is the main method responsible for implementing systematic rewrite
   * using rulesets and tracking the rewrite limit.  Do not memoize this
   * method!
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
  private def _doRewrite(atom: BasicAtom,
                        rulesets: Set[String] = Set.empty,
                        bool: Boolean = false,
                        limit: BigInt = _limit): (BasicAtom, Boolean) = {

    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      return (atom, true)
    }

    if (limit == 0) return (atom, bool)
    else rewriteOnce(atom, rulesets) match {
      case (newatom, false) =>
        return (newatom, bool)
      case (newatom, true) => {
        if (BasicAtom.traceRules) {
          println("Rewrote atom to '" + newatom.toParseString + "'")
        }
        if (atom == newatom) {
          return (newatom, true)
        }
        return _doRewrite(newatom, rulesets, true,
                         if(limit > 0) limit-1 else limit)
      }
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
  def declareRuleset(name: String) = {
    actionList = DeclareRS(name) :: actionList
    
    _rs2bit.get(name) match {
      case None => _rs2bit += (name -> bump()) ; true
      case _ => false
    }
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

      // Check the cache.
      Memo.get(atom, _active) match {
        case None => {
          
          // We do not have a cached value for the current atom. We will
          // need to do the rewrites.
        }
        case Some(pair) => {
          
          // We have a cached value for this atom. Use it.
          if (BasicAtom.traceRules) {
            println("Got cached rewrite '" +
                    atom.toParseString + "' -> '" + pair._1.toParseString +
                    "' w. rulesets " + _activeNames)
          }
          return pair
        }
      }

      // Get the rules.
      val rules = getRules(atom, Set(name))
      // Now try every rule until one applies.
      for (rule <- rules) {
      	val (newatom, applied) = rule.doRewrite(atom, hint)
      	if (applied) {

          // Return the rewrite result.
          return (newatom, applied)
        }
      } // Try every rule until one applies.

      return (atom, false)
    }
    
    
    def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings, hints: Option[Any]) =
      if (subject == this) Match(binds) else subject match {
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
    // add to action list
    actionList = AddRule(rule) :: actionList

    // A rule whose left-hand side is either bindable is not allowed.
    if (rule.pattern.isBindable) {
      throw new BindablePatternException("The rule " + rule.toParseString +
          " has a bindable pattern.  It cannot be added to the system.")
    }
    
    // Rules that rewrite literals might not be allowed.
    if (rule.pattern.isInstanceOf[Literal[_]] && !_allowLiteralRules) {
      throw new LiteralPatternException("The rule " + rule.toParseString +
          " has a literal pattern.  It cannot be added to the system.")
    }
    
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
    // Make sure the rule is not (or does not appear to be) an identity.
    if (rule.pattern == rule.rewrite) {
      throw new IdentityRuleException("The rule " + rule.toParseString +
          " appears to be an identity.  It cannot be added to the system.")
    }
    
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
    case Apply(OperatorRef(op), _) =>
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
    } // Add all the rules stored by kind.
    for ((name,list) <- _op2rules) {
      buf append ("// Rules for operator " + name + ".\n")
      buf append list.map(_._2).mkParseString("","\n","\n")
    } // Add all the rules stored by name.
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

    // make the master Rules
    buf append "object Rules {\n  import ornl.elision.core.Context\n"

    // add apply
    buf append "  def apply(_context: Context):Unit = {\n"
    for(k <- 0 until actionList.length/5000+1) buf append "    Rules"+k+"(_context)\n"
    buf append "  }\n}\n\n"
    
    // add ruleLibrary actions to Rule* classes
    var start = 0
    var end = 0
    var runNum = 0
    var needBrace = false
    actionList.reverseIterator.zipWithIndex.foreach { case (v,i) =>  {
      // update end so we can get the range right for the apply()
      end = i
      
      if(i%5000==0 && needBrace) {
        buf append "\n  def apply(_context: Context):Unit = {\n"
        for(j <- start to end)
          buf append "    rule"+j+"(_context)\n"
        buf append "  }\n}\n"
        needBrace = false
      }
      if(i%5000==0) {
        start = i
        buf append "object Rules"+runNum+" {\n  import ornl.elision.core._\n"
        runNum = runNum + 1 
        needBrace = true
      }      
      
      buf append "  def rule"+i+"(_context: Context):Unit = " +
      (v match {
        case AddRule(rule) => "_context.ruleLibrary.add("+ rule +")\n"
        case DeclareRS(ruleSet) => "_context.ruleLibrary.declareRuleset(\""+ ruleSet +"\")\n"
      })
      
    }}
    if(needBrace) {
        buf append "  def apply(_context: Context):Unit = {\n"
        for(j <- start to end)
          buf append "    rule"+j+"(_context)\n"
        for (rsname <- _activeNames) {
          buf append "    _context.ruleLibrary.enableRuleset(\""+ rsname +"\")\n"
        } // Enable rulesets.
        buf append "  }\n}\n"      
    }
    
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
      case Apply(op:OperatorRef, as:AtomSeq) => {
        // Extract the operator properties.
        val props = op.operator match {
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
        val newRule = RewriteRule(Apply(op, AtomSeq(props, newpatternlist)),
            Apply(op, AtomSeq(props, newrewritelist)),
            rule.guards, rule.rulesets, true)
        list :+= newRule
        
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

abstract class Action
case class AddRule(rule: RewriteRule) extends Action {
//  def apply(rl: RuleLibrary, action: Action): String = "rl.add(rule)"
}
case class EnableRS(ruleSet: String) extends Action {
//  def apply(rl: RuleLibrary) = rl.enableRuleset(ruleSet)
}
case class DisableRS(ruleSet: String) extends Action {
//  def apply(rl: RuleLibrary) = rl.disableRuleset(ruleSet)
}
case class DeclareRS(ruleSet: String) extends Action {
//  def apply(rl: RuleLibrary) = rl.declareRuleset(ruleSet)
}
case class NopAction() extends Action {
//  def apply(rl: RuleLibrary) = ()
}
