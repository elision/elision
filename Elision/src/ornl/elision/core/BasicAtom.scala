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
import scala.collection.immutable.HashMap

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
 *  - Implement `toParseString`.  This must return a string that is parseable by
 *    [[ornl.elision.core.AtomParser]] to re-create the atom.
 *    
 *  - If necessary, override `toString`.  This must return a string that is
 *    parseable by Scala to re-create the atom.  In many cases making the class
 *    into a `case class` will be sufficient, but if there are arguments that
 *    are primitive types, such as strings, whose toString method does not
 *    produce a parseable result, this must be adjusted.  Be sure to see the
 *    implicitly added `mkParseString` method found in the package object
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
 *    val deBruijnIndex = children.foldLeft(0)(_ max _.deBruijnIndex)
 *    }}}
 *      
 *  - Write code to compute the depth of the instance.  This can be computed
 *    as follows.
 *    - Instances that do not have children have depth of zero.
 *    - Instances with other atoms as children have depth equal to the maximum
 *      depth of their children, plus one.
 *    {{{
 *    // Common implementation with children.
 *    val depth = children.foldLeft(0)(_ max _.depth) + 1
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
 *    val isConstant = children.forall(_.isConstant)
 *    }}}
 *    
 *  - Write code to compute the `constantPool`.  This is somewhat complex;
 *    see the documentation for the field for how to do this without going
 *    insane.
 *    
 *  - Specify whether this atom represents a term, or a metaterm.  If a term,
 *    then set `isTerm` to `true`.  Otherwise, set it to `false`.
 */
abstract class BasicAtom {
  import scala.collection.mutable.{Map => MMap}
  
  /** The type for the atom. */
  val theType: BasicAtom
  
  /** The De Bruijn index. */
  val deBruijnIndex: Int

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
   * A mapping from known constant descendants to their child index.
   * 
   * This works as follows.  For constants, and for atoms that do not have any
   * atom children, the constant pool is empty.  In fact, to conserve space the
   * constant pool can be omitted by setting it to `None`.
   * 
   * For nodes that have atom children each child should be queried to determine
   * if it is a constant.  If so, get its hash code, merge it with the atom's
   * own top level code (the name of an operator or some other relevant item),
   * and store it in the pool.
   * 
   * If a child is not a constant, get its constant pool and iterate over it,
   * adding the atom's own top level code to each hash code, and putting the
   * result in the pool.
   * 
   * See the function `buildConstantPool` in the companion object for help with
   * all this.  That function should be sufficient for nearly all atoms.
   * 
   * Suppose you have a complicated case, such as two distinguished children
   * alice and bob, and then a litter of others.  Your constructor might be
   * {{{
   * class Klaus(alice: BasicAtom, bob: BasicAtom, litter: BasicAtom*)
   * }}}
   * Computing the constant pool for this using the `buildConstantPool` works
   * like this.
   * {{{
   * val constantPool =
   *     Some(BasicAtom.buildConstantPool("Klaus".hashCode,
   *         (pattern +: rewrite +: guards):_*))
   * }}}
   * Everything else should be significantly easier!
   */
  val constantPool: Option[Map[Int, Int]]
  
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
    // Determine whether tracing of the match is requested.
    if (BasicAtom.traceMatching) {
      if (BasicAtom.traceVerbose(this))
        traceVerbose(subject, binds, hints)
      else if (BasicAtom.traceTerse(this))
        traceTerse(subject, binds, hints)
      else doMatch(subject, binds, hints)    
    }
    else doMatch(subject, binds, hints)    
  }
  
  /**
   * Attempt to match this atom, as a pattern, against the subject atom,
   * observing the bindings, if any.  The type is checked prior to trying
   * any matching.
   * 
   * Tracing information is printed as matching is performed.
   * 
   * @param subject	The subject atom to match.
   * @param binds		Any bindings that must be observed.
   * @param hints		Optional hints.
   * @return	The matching outcome.
   */
  private def traceVerbose(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) = {
    // We compute a hash code for this match.  This is used in the output to
    // associate lines referring to the match, since matches can be nested and
    // (potentially) interleaved.
    val what = this.hashCode * 31 + subject.hashCode
    
    // If terse matching is requested, write the short messages.
    
    // The match attempt is starting.  Write out information about the
    // attempted match.
    printf("TRYING  (%x) in %s:\n", what, this.getClass.toString)
    println("  pattern: " + this.toParseString + "\n  subject: " +
        subject.toParseString + "\n  with: " + binds.toParseString)
        
    // Perform the match.
    val outcome = doMatch(subject, binds, hints)
    
    // Write out information about the result of the match attempt.
    outcome match {
      case fail:Fail =>
      	printf("FAILURE (%x): ", what)
        println(fail)
      case Match(bnd) =>
    		printf("SUCCESS (%x): ", what)
        println(bnd.toParseString)
      case many:Many =>
      	printf("SUCCESS (%x): ", what)
        println("  Many Matches")
    }
    
    // The value is the outcome of the match.
    outcome
  }
  
  /**
   * Attempt to match this atom, as a pattern, against the subject atom,
   * observing the bindings, if any.  The type is checked prior to trying
   * any matching.
   * 
   * Tracing information is printed as matching is performed.
   * 
   * @param subject The subject atom to match.
   * @param binds   Any bindings that must be observed.
   * @param hints   Optional hints.
   * @return  The matching outcome.
   */
  private def traceTerse(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) = {
    // We compute a hash code for this match.  This is used in the output to
    // associate lines referring to the match, since matches can be nested and
    // (potentially) interleaved.
    val what = this.hashCode * 31 + subject.hashCode
    
    // If terse matching is requested, write the short messages.
    
    // The match attempt is starting.  Write out information about the
    // attempted match.
    printf("TRYING  (%x) in %s: ", what, this.getClass.getName())
    println(this.toParseString + " ~> " +
        subject.toParseString + " " + binds.toParseString)
        
    // Perform the match.
    val outcome = doMatch(subject, binds, hints)
    
    // Write out information about the result of the match attempt.
    outcome match {
      case fail:Fail =>
        printf("FAILURE (%x)\n", what)
      case Match(bnd) =>
        printf("SUCCESS (%x)\n", what)
      case many:Many =>
        printf("SUCCESS (%x) many\n", what)
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
    if (subject == ANY && !this.isBindable)
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
	      case fail: Fail => fail
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
   * Generate a parseable string from this atom.  The returned string should
   * be able to "round trip," that is, [[ornl.elision.parse.AtomParser]] must
   * be able to parse it and return an atom equal to this one.
   * 
   * @return	The string.
   */
  def toParseString: String

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
  /** Enable (if true) or disable (if false) match tracing. */
  var traceMatching = false
  
  /**
   * Specify verbose tracing for certain objects during matching.
   * 
   * This is a closure that selects the objects to trace.  The closure must
   * take an object (it will be the `this` object at runtime) and return a
   * Boolean indicating whether matching should be traced for that object.
   * This will only occur, of course, if `traceMatching` is set to true.  By
   * default this value is set to `(obj: Any) => false)`, which traces
   * nothing.
   * 
   * Note that full tracing takes precedence over terse tracing.
   */
  var traceVerbose: (Any) => Boolean = ((obj: Any) => false)
  
  /**
   * Specify terse tracing for certain objects during matching.
   * 
   * This is a closure that selects the objects to trace.  The closure must
   * take an object (it will be the `this` object at runtime) and return a
   * Boolean indicating whether matching should be traced for that object.
   * This will only occur, of course, if `traceMatching` is set to true.  By
   * default this value is set to `(obj: Any) => true)`, which traces
   * everything.
   * 
   * Note that full tracing takes precedence over terse tracing.
   */
  var traceTerse: (Any) => Boolean = ((obj: Any) => true)

  /**
   * Provide a convenience method to compute the constant pool map for an atom.
   * Invoke this with your top-level hash (probably your operator name or some
   * similar simple thing) and then provide the children as arguments, or just
   * provide the list of children, expanded for the vararg method.
   * 
   * @param myhash		Your top-level hash.  The constant hash for constants.
   * @param children	The children.
   * @return	The constant pool map.
   */
  def buildConstantPool(myhash: Int, children: BasicAtom*): Map[Int,Int] = {
    // Provide a specialized method to deal with the logic for children.  This
    // could be folded into the expression later, but is broken out here for
    // slightly improved readability.
    def handleChild(child: BasicAtom, index: Int) =
      if (child.isConstant)
        List(hashify(myhash, child) -> index)
      else
        child.constantPool.getOrElse(Map.empty).keySet.map {
        	(hashify(myhash,_) -> index)
        }
    
    // Compute the map.
    children.zip(0 until children.length).map {
      pair => handleChild(pair._1, pair._2)
    }.flatten.toMap
  }
}
