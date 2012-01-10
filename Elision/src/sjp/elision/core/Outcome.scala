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
 * An outcome reports the result of attempting a match.
 */
sealed abstract class Outcome

/**
 * An attempted match succeeded with the resulting bindings.  No alternative
 * matches are possible.
 * @param binds	The bindings that make the match succeed.
 */
case class Match(binds: Bindings) extends Outcome

/**
 * An attempted match succeeded.  Multiple alternative matches may be possible,
 * and can be obtained from the provided iterator.
 * @param matches	An iterator over all possible matches.
 */
case class Many(matches: MatchIterator) extends Outcome

/**
 * An attempted match failed for the specified reason.
 * @param reason	The reason the match failed, as a closure.
 */
case class Fail(reason: () => String) extends Outcome {
  /**
   * Evaluate the reason closure to obtain the actual reason.  This is done
   * in a lazy fashion and stored to prevent it from being evaluated before
   * it is needed, and to prevent it from being evaluated more than once,
   * respectively.
   */
  lazy val theReason = reason()

  /**
   * Get the reason for the matching failure.
   */
  override def toString = theReason
}

/**
 * Define additional methods for Fail.
 */
object Fail {
  /**
   * Construct a new Fail using a simple string instead of a closure.
   * @param reason	The reason the match failed.
   */
  def apply(reason: String) = new Fail(() => reason)

  /**
   * Construct a new Fail using the given reason string, showing the pattern
   * and subject.  The result is packaged as a closure, so the final string is
   * not constructed unless it is needed.
   * @param reason	The reason matching failed.
   * @param pattern	The pattern.
   * @param subject	The subject.
   * @param prior		The optional subordinate matching failure that caused this
   * 								match to fail.  This allows reporting a chain of matching
   * 								failures.  This value is optional.
   */
  def apply(reason: String, pattern: BasicAtom, subject: BasicAtom,
    prior: Option[Fail] = None) =
    new Fail(() => reason + "\nPattern: " +
      pattern.toString + "\nSubject: " + subject.toString +
      (prior match {
        case None => ""
        case Some(fail: Fail) => "\n\nCaused by: " + prior.toString
      }))
}
