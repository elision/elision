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

import scala.collection.immutable.HashSet
import scala.collection.mutable.{HashSet => MutableHashSet, BitSet}
import scala.compat.Platform
import scala.util.DynamicVariable
import ornl.elision.util.PropertyManager
import ornl.elision.util.HasOtherHash
import ornl.elision.generators.ElisionGenerator
import ornl.elision.generators.ScalaGenerator
import ornl.elision.util.Debugger
import ornl.elision.util.Loc

/**
 * This marker trait is used to frighten developers and strike fear into
 * anyone trying to parallelize something in the library.  If you see it,
 * '''BEWARE'''!  The associated class contains some form of mutable data!
 */
trait Mutable

/**
 * This marker trait is used to frighten developers and strike fear into
 * anyone trying to get some work done.  If you see it, '''BEWARE'''!  The
 * associated class is going to change dramatically, disappear, or explode
 * violently.
 */
trait Fickle

/**
 * A ''rewriter'' is an atom that can be applied to some other atom to generate
 * a potentially new atom.  It also needs to report its ''success'' via a flag.
 */
trait Rewriter {
  /**
   * Apply this rewriter to the given atom, yielding a potentially new atom.
   * The rewriter must also provide a flag indicating whether it "succeeded"
   * in some appropriate sense.
   * 
   * The specific sense of "success" is dependent on the rewriter, and does
   * not necessarily mean that the rewritten atom is different from the
   * original atom.
   * 
   * Atoms that have this trait can be placed on the left-hand side of the
   * applicative dot.  This method will get invoked when that happens, with
   * the right-hand side passed as the atom.
   * 
   * @param atom	The atom to rewrite.
   * @param hint	An optional hint to pass along during matching.
   * @return	A pair consisting of a potentially new atom and a flag indicating
   * 					success or failure.
   */
  def doRewrite(atom: BasicAtom, hint: Option[Any] = None): (BasicAtom, Boolean)
}

/**
 * An ''applicable'' is an atom that can be applied to some other atom to
 * generate a potentially new atom.
 */
trait Applicable {
  /**
   * Apply this object, whatever it is, to the given atom with the provided
   * bindings.
   * 
   * Atoms that have this trait can be placed on the left-hand side of the
   * applicative dot.  This method will get invoked when that happens, with
   * the right-hand side passed as the atom.
   * 
   * An applicable such as an operator might have a native handler registered.
   * To control when these are applied, a `bypass` flag is provided.  If 
   * `true` then no native handler should be invoked.  By default the flag
   * is `false`.
   * 
   * @param rhs			The atom to apply this to.
   * @param bypass	Whether to bypass native handlers.
   * @return	A potentially new atom.
   */
  def doApply(rhs: BasicAtom, bypass: Boolean = false): BasicAtom
}

/**
 * The root of all atoms manipulated by the rewriter.
 *
 * To use this, extend the class as a new `case class`.  Then implement the
 * abstract methods and fields, and override any methods or fields you need to.
 * 
 * The following list is a short guide to some of the things you should do when
 * you implement `BasicAtom`.
 * 
 *  - Specify the type of the object.  To do this add `val theType = `(a basic
 *    atom).
 *    
 *  - Implement `tryMatchWithoutTypes`.  You can safely assume this method is
 *    not invoked until ''after'' the types have been matched successfully.  You
 *    can also assume this may be invoked multiple times if there are many
 *    potential matches for the types.
 *    
 *  - Implement `rewrite`.
 *  
 *  - Visit [[ornl.elision.core.ElisionGenerator]] and add code to create a
 *    string from the new atom.  This must return a string that is parseable by
 *    [[ornl.elision.core.AtomParser]] to re-create the atom.
 *    
 *  - Visit [[ornl.elision.core.ScalaGenerator]] and add code to create a
 *    string from the new atom.  This must return a string that is parseable by
 *    Scala to re-create the atom.  In many cases making the class into a
 *    `case class` will be sufficient, but if there are arguments that are
 *    primitive types, such as strings, whose toString method does not produce
 *    a parseable result, this must be adjusted.  Be sure to see the implicitly
 *    added `mkParseString` method found in the package object
 *    [[ornl.elision.core.package]], as this can help.
 *    
 *  - Write code to specify the De Bruijn index of the instance and add
 *    `val deBruijnIndex =` to set the index.  This can be computed as follows.
 *    - Instances with no children have De Bruijn index of zero.
 *    - Instances other than lambdas with children have index equal to the
 *      maximum index of their children.
 *    - Lambdas have index one greater than the index of their body, and the
 *      body must also be rewritten to replace the variable with a De Bruijn
 *      index variable.  The implementation of this is left to classes that
 *      implement lambdas.
 *    {{{
 *    // Common implementation with children.
 *    lazy val deBruijnIndex = children.foldLeft(0)(_ max _.deBruijnIndex)
 *    }}}
 *      
 *  - Write code to compute the depth of the instance.  This can be computed
 *    as follows.
 *    - Instances that do not have children have depth of zero.
 *    - Instances with other atoms as children have depth equal to the maximum
 *      depth of their children, plus one.
 *    {{{
 *    // Common implementation with children.
 *    lazy val depth = children.foldLeft(0)(_ max _.depth) + 1
 *    }}}
 *      
 *  - Write code to determine if the instance is a constant.  A constant can be
 *    arbitrarily complex, but cannot contain variables.  This can be computed
 *    as follows.
 *    - Variables are not constants.
 *    - Literals are constants.
 *    - Instances with children are constant iff all their children are
 *      constant.
 *    {{{
 *    // Common implementation with children.
 *    lazy val isConstant = children.forall(_.isConstant)
 *    }}}
 *    
 *  - Specify whether this atom represents a term, or a metaterm.  If a term,
 *    then set `isTerm` to `true`.  Otherwise, set it to `false`.  An atom is
 *    a metaterm if it simply ''is'', or if it contains a metaterm.  In general
 *    the following will work.
 *    {{{
 *    // Common implementation of isTerm with children.
 *    lazy val isTerm = children.forall(_.isTerm)
 *    }}}
 * 
 * @param loc The location where this atom originated.
 */
abstract class BasicAtom(val loc: Loc = Loc.internal) extends HasOtherHash {
  import scala.collection.mutable.{Map => MMap}

  /**
   * The rulesets with respect to which this atom is clean. If this is
   * not None the atom has already been rewritten with some set of
   * rulesets. If None, the atom has never been rewritten.
   */
  var cleanRulesets: Option[BitSet] = None

  /** The type for the atom. */
  val theType: BasicAtom
  
  /** The De Bruijn index. */
  val deBruijnIndex: Int

  /**
   * An alternate hash code for a BasicAtom. An
   * alternate hash code is used in some cases to provide 2 different
   * hash codes for an Elision object. These 2 hash codes are used to
   * lower the chances of a hash collision (both different hash codes
   * will need to collide for a hash collision to occur).
   */
  val otherHashCode: BigInt

  /**
   * If true then this atom can be bound.  Only variables should be bound, so
   * override this for variables; it is `false` by default.
   */
  val isBindable: Boolean = false

  /**
   * If true, this atom represents false.  Override this for an atom that
   * represents false.
   */
  val isFalse: Boolean = false

  /**
   * If true, this atom represents true.  Override this for an atom that
   * represents true.
   */
  val isTrue: Boolean = false

  /**
   * Iff true, this is a De Bruijn index.  A De Bruijn index is a special kind
   * of variable that is used during alpha conversion of lambdas.  If you are
   * not writing code for a lambda, you can ignore this.  Otherwise you need to
   * convert variables bound by the lambda into De Bruijn indices.  This flag
   * is used to protect De Bruijn indices against further rewriting.
   */
  val isDeBruijnIndex = false
  
  /** If true then this atom denotes a constant (it contains no variables). */
  val isConstant: Boolean
  
  /** If true then this atom denotes a term.  If false, a metaterm. */
  val isTerm: Boolean
  
  /**
   * The depth of the atom.  An atom's depth is equal to the maximum depth
   * of its children, plus one.  An atom with no children has depth zero.
   */
  val depth: Int
  
  /**
   * Whether this atom should be applied, even on meta-terms.  Do not mess
   * with this; unless you are modifying the Elision language you want to
   * leave this alone.  Only exceptional operators that must operate
   * handlers to terms that will contain meta-terms need to change this.
   * For nearly every operator the default handling of meta-terms is
   * appropriate.
   */
  val evenMeta = false
  
  /**
   * Construct and cache the spouse of this object.
   */
  lazy val spouse = BasicAtom.buildSpouse(this)

  /**
   * Get all the variables referenced in an atom. Override this if the
   * atom can actually contain variables.
   * 
   * The set uses hash codes to distinguish elements, and variables and
   * metavariables have distinct hash codes, so `$``x` and `$``$``x` will
   * both be included - if present - in the result.
   * 
   * @return A set of the variables referenced in the atom, if it has
   * any. 
   */
  def getVariables() : Option[MutableHashSet[BasicAtom]] = {
    return None
  }

  /**
   * Get all the named operators referenced in an atom. Override this if the
   * atom can actually contain operator expressions.
   *
   * @param opNames The names of the operators to look for.
   * @return A set of the operators referenced in the atom, if it has
   * any. 
   */
  def getOperators(opNames : MutableHashSet[String]) : Option[MutableHashSet[BasicAtom]] = {
    return None
  }
  
  /**
   * Attempt to match this atom, as a pattern, against the subject atom,
   * observing the bindings, if any.  The type is checked prior to trying
   * any matching.
   * 
   * @param subject	The subject atom to match.
   * @param binds		Any bindings that must be observed.  This is optional.
   * @param hints		Optional hints.
   * @return	The matching outcome.
   */
  def tryMatch(subject: BasicAtom, binds: Bindings = Bindings(),
      hints: Option[Any] = None) = {
    var what = 0L
    Debugger("matching") {
      // We compute a hash code for this match.  This is used in the output to
      // associate lines referring to the match, since matches can be nested and
      // (potentially) interleaved.
      what = this.hashCode * 31 + subject.hashCode
    
      // The match attempt is starting.  Write out information about the
      // attempted match.
      Debugger.debugf("matching",
          "TRYING  (%x) in %s:\n", what, this.getClass.toString)
      Debugger("matching", "  pattern: " + this.toParseString + "\n  subject: " +
          subject.toParseString + "\n  with: " + binds.toParseString)
    }
        
    // Perform the match.
    val outcome = doMatch(subject, binds, hints)
    
    Debugger("matching") {
      // Write out information about the result of the match attempt.
      outcome match {
        case fail:Fail =>
        	Debugger.debugf("matching", "FAILURE (%x): ", what)
          Debugger("matching", fail)
        case Match(bnd) =>
      		Debugger.debugf("matching", "SUCCESS (%x): ", what)
          Debugger("matching", bnd.toParseString)
        case many:Many =>
        	Debugger.debugf("matching", "SUCCESS (%x): ", what)
          Debugger("matching", "  Many Matches")
      }
    }
    
    // The value is the outcome of the match.
    outcome
  }

  /**
   * Attempt to match this atom, as a pattern, against the subject atom,
   * observing the bindings, if any.  The type is checked prior to trying
   * any matching.
   * 
   * @param subject	The subject atom to match.
   * @param binds		Any bindings that must be observed.
   * @param hints		Optional hints.
   * @return	The matching outcome.
   */
  private def doMatch(subject: BasicAtom, binds: Bindings, hints: Option[Any]) =

    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      Fail("Timed out.", this, subject)
    }
  
    else if (subject == ANY && !this.isBindable)
      // Any pattern is allowed to match the subject ANY.  In the matching
      // implementation for ANY, any subject is allowed to match ANY.
      // Thus ANY is a kind of wild card.  Note that no bindings are
      // applied - anything can match ANY.
      //
      // Of course, if this atom is bindable, we might want to bind to ANY,
      // so we exempt that case.
      Match(binds)
    else if (depth > subject.depth)
    	// If this pattern has greater depth than the subject, reject immediately.
      Fail("Depth of pattern greater than depth of subject.", this, subject)
    else if (isConstant && this == subject)
	    // Don't bother to try to match equal atoms that are constant.  The
	    // constancy check is required; otherwise we might "match" $x against
	    // $x, but not bind.  This leaves us free to bind $x to something
      // different later, invalidating the original "match".  Matching is
      // tricky.
      Match(binds)
    else
      // We didn't find a fast way to match, so we need to actually perform
      // the match.  First we try to match the types.  If this succeeds, then
      // we invoke the implementation of tryMatchWithoutTypes.
      matchTypes(subject, binds, hints) match {
	      case fail: Fail => {
                fail
              }
	      case mat: Match => tryMatchWithoutTypes(subject, mat.binds, hints)
	      case Many(submatches) =>
	        Many(MatchIterator(tryMatchWithoutTypes(subject, _, hints),
	          submatches))
	    }

  /**
   * Try to match this atom, as a pattern, against the given subject.  Do not
   * do type matching for this atom, but use [[BasicAtom.tryMatch]] for any
   * children, so their types are correctly matched.
   *
   * @param subject	The subject atom to match.
   * @param binds		Any bindings that must be observed.
   * @param hints		Optional hints.
   * @return	The matching outcome.
   */
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]): Outcome

  /**
   * Rewrite this atom with the specified bindings.  If types are involved, it
   * is expected that overriding types will handle rewriting those, as well.
   * 
   * @param binds		The bindings.
   * @return	The result of rewriting this atom, and whether or not anything
   * 					changed.
   */
  def rewrite(binds: Bindings): (BasicAtom, Boolean)
  
  /**
   * Systematically replace one basic atom with another basic atom.
   * 
   * Note that you can use this to search for a subterm.  The idea is to map
   * the subterm to itself (so no real "change" is made) and then look at the
   * flag on return.
   * 
   * @param map   A mapping from old atom to replacement atom.
   * @return  The result of the replacement (a potentially new atom), and a
   *          flag indicating whether or not any replacement was performed.
   */
  def replace(map: Map[BasicAtom, BasicAtom]): (BasicAtom, Boolean)

  /**
   * Generate a parseable string from this atom.  The returned string should
   * be able to "round trip," that is, [[ornl.elision.parse.AtomParser]] must
   * be able to parse it and return an atom equal to this one.
   * 
   * @return	The string.
   */
  def toParseString = ElisionGenerator.apply(this).toString
  
  /**
   * Generate a parseable string from this atom.  The string is immediately
   * written ("streamed") to the given appendable.
   * 
   * @param app   The appendable to get the string.
   * @param limit A limit on the depth of the returned string.  By default this
   *              is negative one, for no limit.  Note that if the limit is
   *              set, and is exceeded, the string will not be parseable.
   * @return  The appendable.
   */
  def toParseString(app: Appendable, limit: Int = -1) =
    ElisionGenerator.apply(this, app, limit)
    
  /**
   * Generate a parseable string from this atom.
   * 
   * @param limit A limit on the depth of the returned string.  By default this
   *              is negative one, for no limit.  Note that if the limit is
   *              set, and is exceeded, the string will not be parseable.
   * @return  The string.
   */
  def toParseString(limit: Int) =
    Generator("elision", this, new StringBuffer, limit).toString
  
  /**
   * Make a string that can be used to re-generate this atom.
   * 
   * @return  The string.
   */
  override def toString = Generator("scala", this).toString
  
  /**
   * Recursively match the types.  This is unbounded recursion; it is expected
   * that a class (a type universe) will override this method to create a basis
   * case.
   *
   * NOTE: When strategies are finally implemented, this is where the selection
   * of type matching strategies may be done.
   *
   * @param subject	The atom to match.
   * @param binds		The bindings to observe.
   * @param hints		Optional hints.
   * @return	The outcome of the match.
   */
  protected def matchTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]): Outcome =
    this.theType.tryMatch(subject.theType, binds, hints) match {
      case mat: Match => mat
      case many: Many => many
      case fail: Fail => Fail("Types do not match.", this, subject, Some(fail))
    }
}

/**
 * Mutable controls affecting all atoms and matching go here.
 * 
 * In addition you can find the helper method `buildConstantPool` to
 * compute the constant pool for an atom.
 */
object BasicAtom {

  /** The clock time at which the current rewrite will time out.*/
  var timeoutTime: DynamicVariable[Long] = new DynamicVariable[Long](-1L)

  /**
   * The maximum time allowed (in seconds) to rewrite an atom. Set to
   * 0 to allow unlimited time.
   */
  var _maxRewriteTime: BigInt = 0;

  /** Declare the Elision property for setting the max rewrite time. */
  knownExecutor.declareProperty("rewrite_timeout",
      "The maximum time to try rewriting an atom. In seconds.",
      _maxRewriteTime,
      (pm: PropertyManager) => {
        _maxRewriteTime =
          pm.getProperty[BigInt]("rewrite_timeout").asInstanceOf[BigInt]
      })

  /**
   * Compute the wall clock time at which the current rewrite will time
   * out.
   */
  def computeTimeout = {
    
    // Are we timing rewrites out? Also, are we already trying to time
    // out a rewrite?
    //
    // NOTE: We have to explicitly go out to the current executor to
    // get the timeout value rather than using _maxRewriteTime since
    // the closure used in the above declaration for the
    // rewrite_timeout property can wind up referencing a different
    // executor than what Elision is actually using.
    val rewrite_timeout = knownExecutor.getProperty[BigInt]("rewrite_timeout").asInstanceOf[BigInt]
    if ((rewrite_timeout > 0) && (timeoutTime.value <= -1)) {
      // The time at which things time out is the current time plus
      // the maximum amount of time to rewrite things.
      Platform.currentTime + (rewrite_timeout.longValue * 1000)
    } else if (timeoutTime.value > -1) {
      // Return the previously computed timeout value.
      timeoutTime.value
    } else {
      -1L
    }
  }

  /**
   * Check to see if we are timing out rewrites.
   * @return  True if rewrite timeout is enabled (positive) or disabled
   *          (non-positive).
   */
  def timingOut = {
    knownExecutor.getProperty[BigInt]("rewrite_timeout").asInstanceOf[BigInt] > 0
  }

  /**
   * Check to see if the current rewrite has timed out.
   * @return  True iff the current rewrite should time out.
   */
  def rewriteTimedOut = {
    if (timingOut && (timeoutTime.value > 0)) {
      (Platform.currentTime >= timeoutTime.value)
    } else {
      false
    }
  }

  /** Whether or not to provide type information in toParseString(). */
  var printTypeInfo = false

  /**
   * Every basic atom may have a "spouse" that is a different object.
   * This field specifies a closure to create the spouse object.  The
   * basic atom constructor calls this closure, passing the basic atom
   * itself, and caching the returned object.
   */
  var buildSpouse: (BasicAtom) => T forSome {type T <: AnyRef} =
    ((obj: BasicAtom) => obj)
}
