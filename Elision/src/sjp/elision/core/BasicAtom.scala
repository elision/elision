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
 * The root of all atoms manipulated by the rewriter.
 *
 * To use this, extend the class as a new `case class`.  Then do the following.
 *  - Specify the type of the object.  To do this add `val theType = `(a basic
 *    atom).
 *  - Implement `tryMatchWithoutTypes`.  You can safely assume this method is
 *    not invoked until ''after'' the types have been matched successfully.  You
 *    can also assume this may be invoked multiple times if there are many
 *    potential matches for the types.
 *  - Implement `rewrite`.
 *  - Implement `toParseString`.  This must return a string that is parseable by
 *    [[sjp.elision.core.AtomParser]] to re-create the atom.
 *  - Implement `toString`.  This must return a string that is parseable by
 *    Scala to re-create the atom.  In many cases making the class into a
 *    `case class` will be sufficient, but if there are arguments that are
 *    primitive types, such as strings, whose toString method does not produce
 *    a parseable result, this must be adjusted.
 *  - Write code to specify the De Brujin index of the instance.  This can be
 *    computed as follows.
 *    - Instances with no children have De Brujin index of zero.
 *    - Instances other than lambdas with children have index equal to the
 *      maximum index of their children.
 *    - Lambdas have index one greater than the index of their body, and the
 *      body must also be rewritten to replace the variable with a De Brujin
 *      index variable.  The implementation of this is left to the lambda
 *      class.
 */
abstract class BasicAtom {
  /** The type for the atom. */
  val theType: BasicAtom
  
  /** The De Brujin index. */
  val deBrujinIndex: Int

  /** If true then this atom can be bound. */
  val isBindable: Boolean = false

  /** If true, this atom represents false. */
  val isFalse: Boolean = false

  /** If true, this atom represents true. */
  val isTrue: Boolean = false

  /** Iff true, this is a De Brujin index. */
  val isDeBrujinIndex = false

  /**
   * Attempt to match this atom, as a pattern, against the subject atom,
   * observing the bindings, if any.  The type is checked prior to trying
   * any matching.
   * @param subject	The subject atom to match.
   * @param binds		Any bindings that must be observed.  This is optional.
   * @return	The matching outcome.
   */
  def tryMatch(subject: BasicAtom, binds: Bindings = new Bindings) =
    // Don't bother to try to match equal atoms.
    if (this eq subject) Match(binds)
    else matchTypes(subject, binds) match {
      case fail: Fail => fail
      case mat: Match => tryMatchWithoutTypes(subject, binds)
      case Many(submatches) =>
        Many(new MatchIterator(tryMatchWithoutTypes(subject, _),
          submatches))
    }

  /**
   * Try to match this atom, as a pattern, against the given subject.  Do not
   * do type matching for this atom, but use [[BasicAtom.tryMatch]] for any
   * children, so their types are correctly matched.
   *
   * @param subject	The subject atom to match.
   * @param binds		Any bindings that must be observed.  This is optional.
   * @return	The matching outcome.
   */
  def tryMatchWithoutTypes(subject: BasicAtom,
    binds: Bindings = new Bindings): Outcome

  /**
   * Rewrite this atom with the specified bindings.  If types are involved, it
   * is expected that overriding types will handle rewriting those, as well.
   * @param binds		The bindings.
   * @return	The result of rewriting this atom, and whether or not anything
   * 					changed.
   */
  def rewrite(binds: Bindings): (BasicAtom, Boolean)
  
  /**
   * Generate a parseable string from this atom.
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
   * @return	The outcome of the match.
   */
  protected def matchTypes(subject: BasicAtom, binds: Bindings): Outcome =
    this.theType.matchTypes(subject.theType, binds) match {
      case mat: Match => mat
      case many: Many => many
      case fail: Fail => Fail("Types do not match.", this, subject, Some(fail))
    }
}

/**
 * A "root type" is a simple type that is well-known and used globally in the
 * system.  It cannot be rewritten, and matches only itself.
 */
abstract class RootType extends BasicAtom {
	// The index of a symbol or root type is zero.
  val deBrujinIndex = 0
  
  /**
   * Try to match this type against the provided atom.
   * @param subject	The subject to match.
   * @param binds		The bindings to honor.
   * @return	The outcome of the match.
   */
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    // A root type matches only itself.
    if (this == subject) Match(binds)
    else Fail("This type matches only itself.", this, subject)

  /**
   * The root types cannot be rewritten.
   * @param binds	The bindings.
   * @return	This type.
   */
  def rewrite(binds: Bindings) = (this, false)
}

/**
 * Trivial root type with a specified name and the type universe as its type.
 * @param name	The name.
 */
case class NamedRootType(name: String) extends RootType {
  // All named root types are in the type universe.
  val theType = TypeUniverse
  // The parse string is the name, as a symbol.
  def toParseString = toESymbol(name)
  // We have to override the Scala to protect the string.
  override def toString = "NamedRootType(" + toEString(name) + ")"
}
