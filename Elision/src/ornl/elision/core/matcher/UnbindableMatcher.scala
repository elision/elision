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
package ornl.elision.core.matcher

import ornl.elision.core.BasicAtom
import ornl.elision.core.Bindings
import ornl.elision.core.Fail
import ornl.elision.core.Many
import ornl.elision.core.Match
import ornl.elision.core.MatchIterator
import ornl.elision.util.OmitSeq
import ornl.elision.util.OmitSeq.fromIndexedSeq

/**
 * Match unbindable atoms in a sequence of patterns to the unbindable atoms
 * in a sequence of subjects.  This match is done commutatively.
 * 
 * == Purpose ==
 * When performing commutative matching it is best to fail as rapidly as
 * possible.  The best place to look for a failure is in an "unbindable" atom
 * that appears in the patterns.  An unbindable atom is an atom whose
 * `isBindable` method returns false; it cannot be bound.  Examples are
 * constants (which are actually eliminated elsewhere, and should not need to
 * be handled here) and operator applications.
 * 
 * This forces the matching system to attempt to match structurally first, and
 * then try various potential bindings second, leading to much faster matching.
 * It also requires that the atoms matched by this method be removed before
 * further matching continue.
 * 
 * To determine which atoms are left after this matcher completes, a specialized
 * version of Bindings is returned.
 * 
 * == Example ==
 * Consider the patterns `$``x,$``y,foo(4,$``y),$``z` and the subject lists:
 * - `foo(4,yar),9,21,yar` (which matches)
 * - `foo(4,yar),9,21,har` (which does not)
 * 
 * It could take a long time to determine that the second item does not match
 * the pattern, since the system will exhaustively try all orderings and
 * groupings before giving up.  The unbindable matcher will first scan the
 * patterns for unbindables, seeing `foo(4,$``y)`.  It then tries to match this
 * against each of the subjects in the list.  For the first example above
 * it immediately matches against `foo(4,yar)` with the binding `$``y -> yar`.
 * This results in an almost immediate match.  In the second case no match
 * is possible, and the unbindable matcher immediately fails and stops any
 * further attempts.
 * 
 * Note also that this example shows the benefit for commutative, but not
 * associative, matching.
 * 
 * @param patterns	The patterns to match.
 * @param subjects	The subjects to match.
 * @param binds			Bindings to honor in any match.
 */
class UnbindableMatcher(patterns: OmitSeq[BasicAtom],
    subjects: OmitSeq[BasicAtom], binds: Bindings) extends MatchIterator {
  /**
   * Next index to start looking for a subject to match.
   */
  private var _nextsubindex = 0


  def get_shallowest_unbindable(patterns :OmitSeq[BasicAtom]) : Int = {
    var cur_index = patterns.indexWhere(!_.isBindable, 0)
    var min_index = cur_index
    var min_depth = -1

    while (cur_index >= 0) {
      val cur_depth = patterns(cur_index).depth
      if (min_depth == -1 || cur_depth < min_depth) {
	min_index = cur_index
	min_depth = cur_depth
      }

      cur_index = patterns.indexWhere(!_.isBindable,cur_index+1)
    }

    return min_index
  }
  
  // Locate the first unbindable pattern and save its index.  This is the only
  // pattern we try to match in this instance.
  private val _patindex = get_shallowest_unbindable(patterns)

  if (_patindex < 0) {
    // There were no patterns.  Return the binding we got as the only "match."
    // This will be returned, and then findNext will be invoked which will mark
    // the iterator as exhausted.
    _current = binds
  }
  
  /**
   * Return the match, but also cache the patterns and subjects at this point.
   */
  override def next = super.next match {
    case null => null
    case binds1:Bindings =>
      // Make sure to honor the original bindings we were given from the
      // complete match (not the ones we have cached locally).
      (binds1 ++ binds).set(binds1.patterns.getOrElse(patterns),
                            binds1.subjects.getOrElse(subjects))
  }
  
  /* Find the first unbindable pattern and then search the subjects to find a
   * matching subject.  If we find one, save the iterator.  Then make a matcher
   * for the unbindable patterns that remain, and join these with the match
   * iterator combinator to yield a complete iterator.
   * 
   * If we cannot find a match, the iterator is exhausted.
   */
  
  def findNext {
//    println("unbindable matcher findNext")
    // If we had either a current match or a local iterator, then the matching
    // infrastructure would use it up before calling this method.  Since we
    // have arrived here, we do not have either.

    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      _exhausted = true
      return
    }
    
    // If there are no more patterns, this matcher is exhausted.  This can
    // happen if there were no suitable patterns to start with.
    if (_patindex < 0) {
      _exhausted = true
      return
    }

    // Find the next unbindable subject.  We bump the _nextsubindex value so
    // we look past this next time.
    val subindex = subjects.indexWhere(!_.isBindable, _nextsubindex)
    _nextsubindex = subindex + 1
    
    // If we ran off the end of the subjects, we have exhausted this match
    // iterator.
    if (subindex < 0) {
      _exhausted = true
      return
    }
//    println(patterns(_patindex).toParseString)
//    println(subjects(subindex).toParseString)
    // Try to match the subject and the pattern.
    val iterator = patterns(_patindex).tryMatch(subjects(subindex), binds) match {
      case fail:Fail =>
        // The match failed, but we can try to keep searching.  The thing to
        // do is look at the next subject index.
        findNext
        return
      case Match(binds1) =>
        // The pattern and subject match.  This binding is the basis for the
        // continued match. Make sure to honor the original bindings we were given.
        MatchIterator((binds1 ++ binds).set(binds1.patterns.getOrElse(patterns),
                                            binds1.subjects.getOrElse(subjects)))
      case Many(iter) =>
        // The pattern and subject match in many ways.  We use this to build
        // a new iterator for the next pattern.
        iter
    }
//    println("foo")
    
    // If we arrive here, we were able to match the subject and pattern, and
    // we have an iterator over the matches.  Now we must combine this with
    // the subsequent unbindable matches (if any).
    _local = iterator ~ (bindings => 

      // Have we timed out since the unbindable match iterator was
      // created?
      if (bindings == null) {

        // This set of bindings can never match. Return an empty iterator.
        new MatchIterator {
          _current = null
          _local = null
          _exhausted = true
          def findNext = {
//	    println("unbindable empty findNext")
            _exhausted = true
          }
        }
      }

      else {
        new UnbindableMatcher(
          patterns.omit(_patindex), 
          subjects.omit(subindex), 
          bindings ++ binds)
      })
  }
}
