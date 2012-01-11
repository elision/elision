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
 * Define the unique type universe.
 */
object TypeUniverse extends BasicAtom {
  /** The type of the type universe is itself. */
  val theType = this

  /**
   * The type universe is represented by ^TYPE.
   * @return The string representation of the type universe.
   */
  override def toString = "^TYPE"

  /**
   * Type universe matching is a special case, and the basis case, for matching
   * types.
   * @param subject	The subject to match.
   * @param binds		The bindings to honor.
   * @return	The outcome of the match.
   */
  override protected def matchTypes(subject: BasicAtom, binds: Bindings) =
    tryMatchWithoutTypes(subject, binds)

  /**
   * Type universe matching is a special case, and the basis case, for matching
   * types.
   * @param subject	The subject to match.
   * @param binds		The bindings to honor.
   * @return	The outcome of the match.
   */
  override def tryMatch(subject: BasicAtom, binds: Bindings) =
    tryMatchWithoutTypes(subject, binds)

  /**
   * Try to match this type universe against the provided atom.
   * @param subject	The subject to match.
   * @param binds		The bindings to honor.
   * @return	The outcome of the match.
   */
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    // A type universe matches only itself.
    if (this == subject) Match(binds)
    else Fail("Type universes match only themselves.", this, subject)

  /**
   * The type universe cannot be rewritten.
   * @param binds	The bindings.
   * @return	This type universe.
   */
  def rewrite(binds: Bindings) = (this, false)
}
