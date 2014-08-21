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
package ornl.elision.context

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap, BitSet, ListBuffer}
import scala.collection.immutable.List
import scala.collection.immutable.HashSet
import scala.math.BigInt.int2bigInt
import ornl.elision.core.AlgProp
import ornl.elision.core.Apply
import ornl.elision.core.AtomSeq
import ornl.elision.core.BasicAtom
import ornl.elision.core.Bindings
import ornl.elision.core.Fickle
import ornl.elision.core.Lambda
import ornl.elision.core.Literal
import ornl.elision.core.Memo
import ornl.elision.core.Match
import ornl.elision.core.Fail
import ornl.elision.core.Mutable
import ornl.elision.core.NoProps
import ornl.elision.core.Operator
import ornl.elision.core.OperatorRef
import ornl.elision.core.RulesetRef
import ornl.elision.core.RSREF
import ornl.elision.core.RewriteRule
import ornl.elision.core.Rewriter
import ornl.elision.core.SpecialForm
import ornl.elision.core.SymbolicOperator
import ornl.elision.core.Variable
import ornl.elision.core.giveMkParseString
import ornl.elision.util.ElisionException
import ornl.elision.util.OmitSeq
import ornl.elision.util.other_hashify
import ornl.elision.util.Debugger
import ornl.elision.util.Loc
import scala.language.reflectiveCalls
import scala.collection.mutable.Queue

/**
 * Indicate an attempt to use an undeclared ruleset.
 * 
 * @param loc   Location of the atom containing the bad reference, or of the
 *              bad reference.
 * @param msg		A human readable message.
 */
class NoSuchRulesetException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * Indicate an attempt to add an identity rule.
 * 
 * @param loc   Location of the bad rule.
 * @param msg   A human-readable message.
 */
class IdentityRuleException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * Indicate an attempt to add a rule whose pattern is bindable (i.e., a
 * simple variable).
 * 
 * @param loc   Location of the bad rule.
 * @param msg   A human-readable message.
 */
class BindablePatternException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * Indicate an attempt to add a rule whose pattern is a literal, when such
 * are not allowed.
 * 
 * @param loc   Location of the bad rule.
 * @param msg   A human-readable message.
 */
class LiteralPatternException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * Encapsulate a rule library.
 * 
 * == Purpose ==
 * A rule library instance contains rules and methods for accessing relevant
 * rules and for performing "automated" rewriting.
 * 
 * Rules can be organized into rulesets.  A single rule may be in multiple
 * rulesets.  A rule can also be placed in no rulesets, though this is not
 *  advisable, as it is difficult to access the rule later.  In fact, the
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

  // This is needed to correctly order operations when the Scala code for a
  // rule library is being created.  Note that we prepend to the list for
  // performance, so be sure to use a reverseIterator to get the items in
  // order.  Contains rule additions and ruleset operations such as enabling,
  // disabling, and declaration.
  private var actionList = List[Action]()
  
  /**
   * Create a shallow clone of this rule library.  This returns a new rule
   * library instance; since the content of the library is immutable, this
   * new instance is technically independent of the original.
   * 
   * @return  The clone.
   */
  override def clone: RuleLibrary = {
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
    Debugger("rule.rulesets", "Enabled ruleset: " + name)
    Debugger("rule.rulesets", "Active rulesets: " +
        _activeNames.mkString(", "))
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
    Debugger("rule.rulesets", "Disabled ruleset: " + name)
    Debugger("rule.rulesets", "Active rulesets: " +
        _activeNames.mkString(", "))
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
  private var _rewritechild: (TrackedAtom, Set[String]) => (TrackedAtom, Boolean) =
    _rewrite _
  
  /**
  * Set the limit for the number of rewrites.  Use a negative number
  * to indicate no rewrites, and zero to turn off rewriting.
  * 
  * @param limit	The limit of the number of rewrites.
  * @return	This rule library.
  */
  def setLimit(limit: BigInt) = {
    Debugger("rule.library", "Set limit: " + limit)
    _limit = limit
    this
  }
  
  /**
  * Set whether to rewrite children recursively.
  * 
  * @param descend	Whether to rewrite children recursively.
  * @return	This rule library.
  */
  def setDescend(descend: Boolean) = {
    Debugger("rule.library", "Descent: " +
        (if (descend) "enabled" else "disabled"))
    _descend = descend
    this
  }
  
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
    Debugger("rule.library", "Normalize Children: " +
        (if (norm) "enabled" else "disabled"))
    if (norm) _rewritechild = _rewrite _
    else _rewritechild = _rewriteOnce _
    this
  }
  
  //======================================================================
  // Rewriting.
  //======================================================================
  private def _rewriteOnce(tatom: TrackedAtom,
    rulesets: Set[String]): (TrackedAtom, Boolean) = {
    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      Debugger("rewrite", "Rewriting timed out: " + tatom.atom.toParseString)
      throw new TimedOut(tatom.atom.loc, "Rewriting timed out")
    }

    var (newtop, appliedtop) = _rewriteTop(tatom, rulesets)
    if (_descend) {
      var (newatom, applied) = _rewriteChildren(newtop, rulesets)     
      (newatom, appliedtop || applied)
    } else {
      (newtop, appliedtop)
    } 
  }
  
  
  
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
    val pair = _rewriteOnce(new TrackedAtom(atom), rulesets)
    (pair._1.atom, pair._2)
  }

  
  private def _ruleSearch(tatom : TrackedAtom, rules : ListBuffer[RewriteRule]) =
  {
    //Debugger("rewrite", "Rewriting atom: " + atom.toParseString)
    var _newatom = tatom.atom
    var _applied = false
    var i = 0
    Debugger("rewrite", "Starting rule search...")
    while (!BasicAtom.rewriteTimedOut && i < rules.length && !_applied) {
      Debugger("rewrite", "Current rule attempt: " + rules(i).toParseString)
      val (newatom, applied) = rules(i).doRewrite(tatom.atom)
      if (applied) {
        Debugger("rewrite", "Rule applied: " + rules(i).toString)
        //Debugger("rewrite", "Rewrote to: " + newatom.toParseString.substring(0, 1024))        
        _newatom = newatom
        _applied = applied
      }
      i += 1
    }
    if(!_applied) (tatom, _applied)
    else (new TrackedAtom(_newatom, Some(tatom)), _applied)
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
  private def _rewriteTop(tatom: TrackedAtom,
                          rulesets: Set[String]): (TrackedAtom, Boolean) = {

    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      Debugger("rewrite", "Rewriting timed out: " + tatom.atom.toParseString)
      throw new TimedOut(tatom.atom.loc, "Rewriting timed out")
    }

    // Get the rules.
    val rules = if (rulesets.isEmpty) getRules(tatom.atom)
    else getRules(tatom.atom, rulesets)

    // Now try every rule until one applies.
    //Debugger("rewrite", "Rewriting atom: " + atom.toParseString)
    val (newatom, applied) = _ruleSearch(tatom, rules)
        
    
    if (!applied) Debugger("rewrite", "No rules applied.")
    //if (!_applied) Debugger("rewrite", "No rules applied to: " + atom.toParseString.substring(0, 1024))
    return (newatom, applied)
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
  private def _rewriteChildren(tatom: TrackedAtom,
      rulesets: Set[String]): (TrackedAtom, Boolean) = {
    if (BasicAtom.rewriteTimedOut) {
      Debugger("rewrite", "Rewriting timed out: " + tatom.atom.toParseString)
      throw new TimedOut(tatom.atom.loc, "Rewriting timed out")
    } else {
      val retval : (TrackedAtom, Boolean) = tatom.atom match {
        case AtomSeq(props, atoms) =>
          var flag = false
          // Rewrite the properties.  The result must still be a property spec.
          // If not, we keep the same properties.
          val newProps = _rewritechild(new TrackedAtom(props), rulesets) match {
            case (TrackedAtom(ap: AlgProp, _), true) => flag = true; ap
            case _ => props
          }
          // Rewrite the atoms.
          val newtAtoms = atoms.map {
            atom =>
              val (newatom, applied) = _rewritechild(new TrackedAtom(atom), rulesets)
              flag ||= applied
            newatom
          }
          // Return the result.
          val newAtoms = newtAtoms.map {
            ta =>
              ta.atom
          }
          if (flag) (new TrackedAtom(AtomSeq(newProps, newAtoms), Some(tatom)), true) 
          else (tatom, false)
        
        case Apply(lhs, rhs) =>
          val newlhs = _rewritechild(new TrackedAtom(lhs), rulesets)
          val newrhs = _rewritechild(new TrackedAtom(rhs), rulesets)
          if (newlhs._2 || newrhs._2) {
            (new TrackedAtom(Apply(newlhs._1.atom, newrhs._1.atom), Some(tatom)), true)
          } else {
            (tatom, false)
          }
        
        case Lambda(param, body) =>
          val newparam = _rewritechild(new TrackedAtom(param), rulesets) match {
            case (TrackedAtom(v: Variable, _), true) => (v, true)
              case _ => (param, false)
          }
          val newbody = _rewritechild(new TrackedAtom(body), rulesets)
          if (newparam._2 || newbody._2) {
            (new TrackedAtom(Lambda(newparam._1, newbody._1.atom), Some(tatom)), true)
          } else {
            (tatom, false)
          }

        case SpecialForm(tag, content) =>
          val newlhs = _rewritechild(new TrackedAtom(tag), rulesets)
          val newrhs = _rewritechild(new TrackedAtom(content), rulesets)
          if (newlhs._2 || newrhs._2) {
            (new TrackedAtom(SpecialForm(tatom.atom.loc, newlhs._1.atom, newrhs._1.atom),Some(tatom)), true)
          } else {
            (tatom, false)
          }

        case _ =>
          // Do nothing in this case.
          (tatom, false)
      }
      retval
    }
  }

  private def _rewrite(tatom: TrackedAtom, rulesets: Set[String]): (TrackedAtom, Boolean) = {
    // Get a bitset for the given set of rulesets.
    val usedRulesetsBits =
      if (rulesets.isEmpty) _active
      else {
        var r = new BitSet()
        for (rs <- rulesets) r += getRulesetBit(rs)
        r
      }

    // Has this atom already been rewritten using (at least) the
    // current rulesets?
    tatom.atom.cleanRulesets match {

      // Yes it has been rewritten with the current rulesets (and
      // possibly other rulesets..
      case Some(priorRulesets) if (usedRulesetsBits.subsetOf(priorRulesets)) => {
        (tatom, false)
      }

      // It has never been rewritten.
      case _ => {
        // Has rewriting timed out?
        if (BasicAtom.rewriteTimedOut) {
          Debugger("rewrite", "Rewriting timed out: " +
            tatom.atom.toParseString)
          throw new TimedOut(tatom.atom.loc, "Rewriting timed out")
        } else if (tatom.atom.isInstanceOf[Literal[_]] && !_allowLiteralRules) {
          (tatom, false)
        } else if (tatom.atom.isInstanceOf[Variable]) {
          (tatom, false)
        } else {
          // Check the cache.
          val usedRulesets = if (rulesets.isEmpty) _activeNames else rulesets
          var timedOut = false
          val (newatom, flag) = Memo.get(tatom.atom, usedRulesetsBits) match {
            case None => {

              // Set the time at which rewriting the atom will time out, if we
              // are timing rewrites out.
              var pair: (TrackedAtom, Boolean) = null
              if (BasicAtom.timingOut) {
                BasicAtom.timeoutTime.withValue(BasicAtom.computeTimeout) {
                  pair = _doRewrite(tatom, usedRulesets)
                  timedOut = BasicAtom.rewriteTimedOut
                }
              } else {
                pair = _doRewrite(tatom, usedRulesets)
                timedOut = false
              }
              tatom.atom.cleanRulesets match {
                // The atom has now been rewritten with both the
                // rulesets it was previosuly rewritten with and the new
                // rulesets. Save that.
                case Some(prior) => pair._1.atom.cleanRulesets = Some(prior.union(usedRulesetsBits.clone))
                // The atom was not rewritten previosuly. Save the
                // current rulesets used to rewrite the atom.
                case _           => pair._1.atom.cleanRulesets = Some(usedRulesetsBits.clone)
              }
              Memo.put(tatom.atom, usedRulesetsBits, pair._1.atom, 0)
              Memo.put(pair._1.atom, usedRulesetsBits, pair._1.atom, 0)
              pair
            }
            
            case Some(pair) => {
              Debugger("rewrite", "rewrite() Got cached rewrite: " +
                tatom.atom.toParseString + " to: " + pair._1.toParseString +
                " with rulesets: " + usedRulesets.mkString(", "))
              (new TrackedAtom(pair._1), pair._2)
            }
          }

          // Did rewriting this atom time out?
          if (timedOut) {
            // Return the original atom, with the rewrite flag set to true
            // to indicate a timeout.
            Debugger("rewrite", "Rewriting timed out: " +
              tatom.atom.toParseString)
            throw new TimedOut(tatom.atom.loc, "Rewriting timed out")
          } // No timeout.
          else {

            // Return the rewrite results.
            (newatom, flag)
          }
        }
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
  def rewrite(atom: BasicAtom, rulesets: Set[String] = Set.empty): (BasicAtom, Boolean) = {
    val pair = _rewrite(new TrackedAtom(atom), rulesets)
    (pair._1.atom, pair._2)
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
  private def _doRewrite(tatom: TrackedAtom,
                        rulesets: Set[String] = Set.empty,
                        bool: Boolean = false,
                        limit: BigInt = _limit): (TrackedAtom, Boolean) = {

    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      Debugger("rewrite", "Rewriting timed out: " + tatom.atom.toParseString)
      throw new TimedOut(tatom.atom.loc, "Rewriting timed out")
    }
    
    if (limit == 0) return (tatom, bool)
    else _rewriteOnce(tatom, rulesets) match {
      case (newatom, false) =>
        return (newatom, bool)
      case (newatom, true) => {
        Debugger("rewrite", "Rewrote to: " + newatom.atom.toParseString)
        if (tatom.atom == newatom) {
          return (newatom, true)
        }
        if(tatom.contains(newatom.atom)){
          Debugger("rewrite", "WARNING: Rewrite cycle detected:")
          Debugger("rewrite", "Atom history:")
          tatom.toSeq().foreach( h => {
            Debugger("rewrite", h.toParseString)
          })
            
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
   * Get all ruleset names.  Note that this includes the `DEFAULT` ruleset.
   * 
   * return An iterable list of ruleset names.
   */
  def getAllRulesets() = {
    _rs2bit map { _._1 }
  }
  
  /**
   * Get the set of active ruleset names.  This may include the `DEFAULT`
   * ruleset.
   * 
   * @return The set of active ruleset names.
   */
  def getActiveRulesets() = {
    _activeNames
  }
  
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
      else throw new NoSuchRulesetException(Loc.internal,
        "The ruleset " + name + " has not been declared.")))
  
  /**
   * Get the bit set for a collection of rulesets.
   * 
   * @param names   The name of the rulesets to include.
   * @return  The bits set for those rulesets.
   */
  def getRulesetBits(names: Set[String]) = {
    names.foldLeft(new BitSet())(_ += getRulesetBit(_))
  }
  
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
  
  
  //private case class TrackedAtom(val atom:BasicAtom, val history:Queue[BasicAtom] = Queue.empty)
  private case class TrackedAtom(val atom:BasicAtom, val history:Option[TrackedAtom] = None){
    @tailrec
    final def contains(thing:BasicAtom) : Boolean =
    {
      if(thing == atom) return true
      history match {
        case None => false
        case Some(ta) => ta.contains(thing)
      }
    }
    
    final def hasloop() : Boolean = {
      this.contains(atom)
    }
    
    @tailrec
    final def toString(stringsofar : String = "") : String = {
      history match {
        case None => stringsofar + ", " + atom.toString
        case Some(ta) => ta.toString(stringsofar + ", " + atom.toString) 
      }
    }
    
    @tailrec
    final def toParseString(stringsofar : String = "") : String = {
      history match {
        case None => stringsofar + ", " + atom.toParseString
        case Some(ta) => ta.toParseString(stringsofar + ", " + atom.toParseString) 
      }
    }
    
    @tailrec
    final def toSeq(atoms: Seq[BasicAtom] = Seq.empty) : Seq[BasicAtom] = {
      history match {
        case None => atom +: atoms 
        case Some(ta) => ta.toSeq(atom +: atoms) 
      }
    }
  }
  
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
      val (newatom, applied) = Memo.get(atom, _active) match {
        case None => {
          // We do not have a cached value for the current atom. We will
          // need to do the rewrites.
                // Get the rules.
          val rules = getRules(atom, Set(name))
          // Now try every rule until one applies.
          val rwr =_ruleSearch(new TrackedAtom(atom), rules)
          (rwr._1.atom, rwr._2)
        }
        case Some(pair) => {
          // We have a cached value for this atom. Use it.
          Debugger("rewrite", "doRewrite() Got cached rewrite: " +
              atom.toParseString + " to: " + pair._1.toParseString +
              " with rulesets: " + _activeNames.mkString(", "))
          (pair._1, pair._2)
        }
      }
      
      //If a rewrite rule wasn't applied
      if (!applied) Debugger("rewrite", "No rules applied.")
      (newatom, applied)
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
   * Map rule names to the rules that were added with that name.  Rules do not
   * require names, but if a rule has a name it must be unique among all added
   * rules.  This requires special enforcement during addition, and also means
   * that it is sometimes necessary to remove rules.
   */
  private val _name2rules = MMap[String, List[RewriteRule]]()

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
      throw new BindablePatternException(rule.loc,
          "The rule " + rule.toParseString +
          " has a bindable pattern.  It cannot be added to the system.")
    }
    
    // Rules that rewrite literals might not be allowed.
    if (rule.pattern.isInstanceOf[Literal[_]] && !_allowLiteralRules) {
      throw new LiteralPatternException(rule.loc,
          "The rule " + rule.toParseString +
          " has a literal pattern.  It cannot be added to the system.")
    }
    
    // Complete the rule.
    val rules = Completor.complete(rule)
    
    // Rules can have names, and adding a rule with the same name as a
    // previously-added rule replaces the prior rule.  This is a novel
    // case, since typically rules added first have precedence.
    // For this reason, if the rule has a name first remove any prior rules
    // with the same name.  Then record the rules under the name.
    rule.name match {
      case None =>
      case Some(value) =>
        // The rule has a name.  See if we need to remove prior rules that
        // were added with that name.
        _name2rules.get(value) match {
          case None =>
          case Some(priors) =>
            for (prior <- priors) doRemove(prior)
        }
        _name2rules(value) = rules
    }
    
    // Add the rules.
    for (rule2 <- rules) doAdd(rule2)
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
      throw new IdentityRuleException(rule.loc,
          "The rule " + rule.toParseString +
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
   * Remove a rewrite rule from this context.
   * 
   * @param rule  The rewrite rule to remove.
   * @throws  NoSuchRulesetException
   *          At least one ruleset mentioned in the rule has not been declared,
   *          and undeclared rulesets are not allowed.
   */
  private def doRemove(rule: RewriteRule) = {
    // Get the list for the kind of atom the rule's pattern uses.
    val list = getRuleList(rule.pattern)
    // Get the bitset for the rule's ruleset membership.
    val bits = new BitSet()
    for (rs <- rule.rulesets) bits += getRulesetBit(rs)
    // Now remove every instance of the rule from the list.
    for (idx <- list.indices.reverse)
      if (list(idx) == (bits, rule)) list.remove(idx)
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

  /**
   * Get all rules contained in the system, in the order they would be
   * considered during rewrite.
   * 
   * @return  The list of rules.
   */
  def getAllRules() = {
    var all = List[RewriteRule]()
    for ((_, list) <- _kind2rules ++ _op2rules; (_, rule) <- list) all :+= rule
    all
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
    buf append "  def apply(context: Context):Unit = {\n"
    for(k <- 0 until actionList.length/5000+1) {
      buf append "    Rules"+k+"(context)\n"
    }
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
        buf append "\n  def apply(context: Context):Unit = {\n"
        for(j <- start to end)
          buf append "    rule"+j+"(context)\n"
        buf append "  }\n}\n"
        needBrace = false
      }
      if(i%5000==0) {
        start = i
        buf append "object Rules"+runNum+" {\n  import ornl.elision.core._\n"
        runNum = runNum + 1 
        needBrace = true
      }      
      
      buf append "  def rule"+i+"(context: Context):Unit = " +
      (v match {
        case AddRule(rule) => "context.ruleLibrary.add("+ rule +")\n"
        case DeclareRS(ruleSet) => "context.ruleLibrary.declareRuleset(\""+ ruleSet +"\")\n"
      })
      
    }}
    if(needBrace) {
        buf append "  def apply(context: Context):Unit = {\n"
        for(j <- start to end)
          buf append "    rule"+j+"(context)\n"
        for (rsname <- _activeNames) {
          buf append "    context.ruleLibrary.enableRuleset(\""+ rsname +"\")\n"
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
   * @param rule  The provided rule.
   * @return  A list of rules, including the original rule and any synthetic
   *          rules.
   */
  private def _complete(rule: RewriteRule, op: Operator, as: AtomSeq):
      List[RewriteRule] = {
    // Make the list
    var list = List[RewriteRule](rule)
    
    // Extract the operator properties.
    val props = op match {
      case po: SymbolicOperator => po.params.props
      case _ => NoProps
    }
    
    // If the operator is not associative, we don't need to do anything.
    if (!props.isA(false)) {
      return list
    }
    
    // Extract the pattern and rewrite.
    val pattern = rule.pattern
    val rewrite = rule.rewrite

    // The operator is associative.  We must at least add an argument on
    // the right-hand side.  Make and add the argument, and then add the
    // synthetic rule.
    var right = Variable(as(0).theType, "::R")
    var newpatternlist = as.atoms :+ right
    var newrewritelist = OmitSeq[BasicAtom](rewrite) :+ right
    val newRule = RewriteRule(rule.loc,
        Apply(op, AtomSeq(props, newpatternlist)),
        Apply(op, AtomSeq(props, newrewritelist)),
        rule.guards,
        rule.rulesets,
        true)
    list :+= newRule
    
    // If the operator is commutative, we are done.
    if (props.isC(false)) {
      for (rule <- list) {
        Debugger("rule.completion", "Generated synthetic rule: " +
            newRule.toParseString)
      } // Show the rules.
      return list
    }
    
    // Repeat the above to add an argument on the left-hand side.
    var left = Variable(as(0).theType, "::L")
    newpatternlist = left +: as.atoms
    newrewritelist = left +: OmitSeq[BasicAtom](rewrite)
    list :+= RewriteRule(rule.loc, Apply(op, AtomSeq(props, newpatternlist)),
        Apply(op, AtomSeq(props, newrewritelist)),
        rule.guards, rule.rulesets, true)
        
    // And again add the argument on the right-hand side.
    newpatternlist = newpatternlist :+ right
    newrewritelist = newrewritelist :+ right
    list :+= RewriteRule(rule.loc, Apply(op, AtomSeq(props, newpatternlist)),
        Apply(op, AtomSeq(props, newrewritelist)),
        rule.guards, rule.rulesets, true)
        
    // Done.
    for (rule <- list) {
      Debugger("rule.completion", "Generated synthetic rule: " +
          newRule.toParseString)
    } // Show the rules.
    return list
  }
  
  /**
   * Perform partial completion for the given rule by generating the necessary
   * synthetic rules.
   * 
   * @param rule	The provided rule.
   * @return	A list of rules, including the original rule and any synthetic
   * 					rules.
   */
  def complete(rule: RewriteRule): List[RewriteRule] = {
    rule.pattern match {
      case Apply(op: OperatorRef, as: AtomSeq) =>
        _complete(rule, op.operator, as)
      case Apply(op: Operator, as: AtomSeq) =>
        _complete(rule, op, as)
      case _ =>
        List[RewriteRule](rule)
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
