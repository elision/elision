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
package ornl.elision.core.matcher
import ornl.elision.core._

/**
 * Match unbindable atoms in a sequence of patterns to the unbindable atoms
 * in a sequence of subjects.  This match is done commutatively.
 * 
 * To determine which atoms are left after this matcher completes, a specialized
 * version of Bindings is returned.
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
  
  // Locate the first unbindable pattern and save its index.  This is the only
  // pattern we try to match in this instance.
  private val _patindex = patterns.indexWhere(!_.isBindable)
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
    case binds:Bindings => binds.set(patterns, subjects)
  }
  
  /* Find the first unbindable pattern and then search the subjects to find a
   * matching subject.  If we find one, save the iterator.  Then make a matcher
   * for the unbindable patterns that remain, and join these with the match
   * iterator combinator to yield a complete iterator.
   * 
   * If we cannot find a match, the iterator is exhausted.
   */
  
  def findNext {
    // If we had either a current match or a local iterator, then the matching
    // infrastructure would use it up before calling this method.  Since we
    // have arrived here, we do not have either.
    
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
    
    // Try to match the subject and the pattern.
    val iterator = patterns(_patindex).tryMatch(subjects(subindex)) match {
      case fail:Fail =>
        // The match failed, but we can try to keep searching.  The thing to
        // do is look at the next subject index.
        findNext
        return
      case Match(binds) =>
        // The pattern and subject match.  This binding is the basis for the
        // continued match.
        MatchIterator(binds)
      case Many(iter) =>
        // The pattern and subject match in many ways.  We use this to build
        // a new iterator for the next pattern.
        iter
    }
    
    // If we arrive here, we were able to match the subject and pattern, and
    // we have an iterator over the matches.  Now we must combine this with
    // the subsequent unbindable matches (if any).
    _local = iterator ~ (bindings => new UnbindableMatcher(
        patterns.omit(_patindex), subjects.omit(subindex), bindings))
  }
}
