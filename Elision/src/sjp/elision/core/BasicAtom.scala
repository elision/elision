/* Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 */
package sjp.elision.core

/**
 * The root of all atoms manipulated by the rewriter.
 *
 * Use this by extending it and implementing the abstract methods.  Be sure
 * to specify the type.
 */
abstract class BasicAtom {
  /** The type for the atom. */
  val theType: BasicAtom

  /** If true then this atom can be bound. */
  val isBindable: Boolean = false

  /** If true, this atom represents false. */
  val isFalse: Boolean = false

  /** If true, this atom represents true. */
  val isTrue: Boolean = false

  /**
   * Recursively match the types.  This is unbounded recursion; it is expected
   * that a class (a type universe) will override this method to create a basis
   * case.
   *
   * NOTE: When strategies are finally implemented, this is where the selection
   * of type matching strategies should be done.
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
}

/**
 * A "root type" is a simple type that is well-known and used globally in the
 * system.  It cannot be rewritten, and matches only itself.
 */
abstract class RootType extends BasicAtom {
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
  val theType = TypeUniverse
  def toParseString = name
  override def toString = "NamedRootType(" + toEString(name) + ")"
}
