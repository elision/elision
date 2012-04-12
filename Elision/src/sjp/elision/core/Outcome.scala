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
 * An outcome reports the result of attempting a match.
 */
sealed abstract class Outcome

/**
 * Simplified construction.
 */
object Outcome {
  /**
   * Convert a match iterator to an outcome.  If the match iterator is empty,
   * then we return some fail.
   * 
   * @param iter	The match iterator.
   * @param fail	Failure, evaluated only when returned.
   * @return	The outcome.
   */
  def convert(iter: MatchIterator, fail: => Fail) =
    if (iter.hasNext) Many(iter) else fail
}

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
 * @param index		A failure index, in cases where that has meaning, such as
 * 								when matching a sequence of atoms against another sequence.
 */
case class Fail(reason: () => String, index: Int) extends Outcome {
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
  def apply(reason: String) = new Fail(() => reason, 0)

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
   * @param index		An index.  This is useful when we are matching a sequence.
   */
  def apply(reason: String, pattern: BasicAtom, subject: BasicAtom,
    prior: Option[Fail] = None, index: Int = 0): Fail =
    new Fail(() => reason + "\n  pattern: " +
      pattern.toParseString + "\n  subject: " + subject.toParseString +
      (prior match {
        case None => ""
        case Some(_) => "\n\nCaused by:\n" + prior.get.toString
      }), index)
}
