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
case class Many(matches: Matches) extends Outcome

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

/**
 * Provide a systematic way to return potentially multiple matches.
 */
abstract class Matches extends Iterator[Bindings] {
  /** Is this iterator exhausted?  By default it is not. */
  private var exhausted = false
  
  /**
   * If there is a current known match, this is it.  This is the next match
   * to be returned by [[Matches.next]].  It is allowed to be null to
   * indicate that none is known.  We avoid the object creation of Option.
   */
  private var current: Bindings = null
  
  /**
   * Determine if there is a next match.
   * 
   * Since we have to know if there is a next match, we actually generate the
   * match here if necessary.
   * 
   * @return	True if there is another match, and false if not.
   */
  def hasNext =
    if (exhausted) false
    else if (current != null) true
    else {
      current = getNextMatch
      if (current == null) exhausted = true
      current != null
    }
  
  /**
   * Get the next match.
   * 
   * This may be called without first calling [[Matches.hasNext]].  If so, then
   * we call [[Matches.hasNext]] here to get the next match.  Note that this
   * method is allowed to return null.  This is done instead of making an
   * exception, which is costly.
   * 
   * @return	The next match, or null if there is none.
   */
  def next =
    if (current != null || hasNext) {
      val ret = current
      current = null
      ret
    } else null
    
  /**
   * The method to generate the next match.  This must be supplied by any
   * overriding class.
   * @return	The next match(es).
   */
  protected def getNextMatch: Bindings
}
