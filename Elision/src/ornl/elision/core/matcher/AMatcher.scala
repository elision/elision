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
import ornl.elision.core._
import ornl.elision.util.OmitSeq
import ornl.elision.util.Debugger._
import ornl.elision.util.Debugger

/**
 * Match two sequences, where the elements of the second sequence can be
 * grouped, but not re-ordered.
 */
object AMatcher {

  /**
   * Attempt to match two lists.  The second list can be re-grouped, but every
   * group must be non-empty.  This restriction is easy to overcome, but
   * provides better performance and results in more obvious (and expected)
   * matching in many cases.
   * 
   * @param plist	The pattern list.
   * @param slist	The subject list.
   * @param binds	Bindings that must be honored in any match.
   * @param op		An optional operator to apply to sublists.
   * @return	The match outcome.
   */
  def tryMatch(plist: AtomSeq, slist: AtomSeq, binds: Bindings,
               op: Option[OperatorRef]): Outcome = {

    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      return Fail("Timed out", plist, slist)
    }

    // Check the length.
    if (plist.atoms.length > slist.atoms.length)
      return Fail("More patterns than subjects, so no match is possible.",
          plist, slist)
          
    // Watch for the case in which there are no patterns.
    if (plist.atoms.length == 0) {
      if (slist.atoms.length == 0) {
        // Match.
        return Match(binds)
      } else {
        // No match is possible.
        return Fail("No patterns to bind to subjects.  No match is possible.",
            plist, slist)
      }
    }
      
    // If there are the same number, then this is a simple case of matching.
    if (plist.atoms.length == slist.atoms.length)
      return SequenceMatcher.tryMatch(plist.atoms, slist.atoms, binds)
      
    // If there is exactly one pattern then match it immediately.
    if (plist.atoms.length == 1) {
      // If there is an operator, apply it to the subjects, then try to match
      // the single pattern against the result.
      return plist.atoms(0).tryMatch(op match {
        case Some(opref) =>
          Apply(opref, slist)
        case None =>
          slist
      }, binds)
    }
      
    // We need to group the atoms so there is the same number of patterns and
    // subjects.  If there are N subjects and M patterns (with M < N per the
    // above checks) then we essentially insert M-1 markers between elements
    // of the N subjects.  Get it?  We use a special iterator for that.
    val iter = new AMatchIterator(plist, slist, binds, op)
    if (iter.hasNext) return Many(iter)
    else Fail("The lists do not match.", plist, slist)
  }
  
  /* How associative matching works.
   * 
   * There must be at least as many subjects as patterns.  If there is an
   * equal number, then we use the sequence matcher to perform the match.
   * Otherwise we need to group the items to perform the match.
   * 
   * We do not allow less subjects than patterns.  Technically we could,
   * allowing patterns to match empty groups of subjects.  In practice, this
   * just adds things to check and can result in non-obvious results from
   * matching.  It is better (from an experience, performance, and expectation
   * point of view) to require groups be non-empty.
   * 
   * Because we cannot re-order the atoms, we need a completely different
   * form of matching for associativity.
   * 
   * Naive associative matching works as follows.  If we have P patterns and
   * S subjects (S>P) then we need to break the S subjects up into P groups.
   * We can do this by inserting P-1 "markers" between subjects, and we can
   * do this in (S-1) C (P-1) ways.
   * 
   * If a marker comes between items n and n+1, we say the marker is after
   * position n.  We then have an array of P-1 markers, initialized to hold
   * 0, 1, 2, ..., P-2.  That is, position i initially holds the value i.
   * 
   * We advance to the next position by incrementing the highest position
   * marker (position P-2).  If that exceeds the limit for that marker, then
   * we increment the prior marker, and reset the current marker to the first
   * legal position.  We continue in this fashion until the array holds the
   * stop position, , ..., S-2
   * 
   * We can illustrate this below for four patterns, and seven markers.
   * 
   * subjects (markers are |)        array
   * x|x|x|x x x x                   0,1,2
   * x|x|x x|x x x                   0,1,3
   * x|x|x x x|x x                   0,1,4
   * x|x|x x x x|x                   0,1,5
   * x|x x|x|x x x                   0,2,3
   * x|x x|x x|x x                   0,2,4
   * x|x x|x x x|x                   0,2,5
   * x|x x x|x|x x                   0,3,4
   * x|x x x|x x|x                   0,3,5
   * x|x x x x|x|x                   0,4,5
   * x x|x|x|x x x                   1,2,3
   * x x|x|x x|x x                   1,2,4
   * x x|x|x x x|x                   1,2,5
   * x x|x x|x|x x                   1,3,4
   * x x|x x|x x|x                   1,3,5
   * x x|x x x|x|x                   1,4,5
   * x x x|x|x|x x                   2,3,4
   * x x x|x|x x|x                   2,3,5
   * x x x|x x|x|x                   2,4,5
   * x x x x|x|x|x                   3,4,5
   * 
   * Note that there are 20 groupings.
   * (7-1) C (4-1) = 6 C 3 = 6! / 3! / 3! = 6*5*4 / 6 = 5*4 = 20.
   * 
   * The last subject index is 7-1 = 6.  We are placing 4-1 = 3 markers,
   * so the last position for the first (i=0) marker is 6 - 3 = 3.  The last
   * position for the ith marker is 6 - 3 + i.
   * 
   * Note that all that would be necessary to allow empty group matching is
   * to always reset markers equal to the prior marker.  Then the marker
   * array would run 0,0,0 - 0,0,1 - 0,0,2 - 0,0,3 - 0,0,4 - 0,0,5 - 0,1,1 -
   * and so forth to 5,5,5.  For 0,0,0, the first and second groups are
   * empty and the third is everything.  For 5,5,5, the first group has
   * everything and the second and third are empty.
   */
  
  /**
   * Perform associative matching on the subjects and patterns, honoring any
   * bindings we are given and optionally applying the given operator to any
   * subgroups.
   * 
   * @param patterns	The patterns.
   * @param subjects	The subjects.
   * @param binds			Bindings to honor.
   */
  private class AMatchIterator(patterns: AtomSeq, subjects: AtomSeq,
      binds: Bindings, op: Option[OperatorRef]) extends MatchIterator {
    
    /** An iterator over all groupings of the subjects. */
    private val _groups = new GroupingIterator(patterns, subjects, op)
    
    /**
     * Find the next match.  At the end of running this method either we
     * have `_current` set to the next match or we have exhausted the
     * iterator.
     */
    @scala.annotation.tailrec
    final protected def findNext {
      Debugger("matching", "A Searching... ")

      // Has rewriting timed out?
      if (BasicAtom.rewriteTimedOut) {
        _exhausted = true
        return
      }

      _current = null
      if (_local != null && _local.hasNext) {
        _current = _local.next
      } else {
        _local = null
        if (_groups.hasNext) {
          SequenceMatcher.tryMatch(patterns.atoms, _groups.next, binds) match {
            case fail:Fail =>
              // We ignore this case.  We only fail if we exhaust all attempts.
              Debugger("matching", fail.toString)
              findNext
            case Match(binds1) =>
              // This case we care about.  Save the bindings as the current match.
              _current = (binds ++ binds1).set(binds1.patterns.getOrElse(patterns),
                  binds1.subjects.getOrElse(subjects))
              Debugger("matching", "A Found.")
            case Many(iter) =>
              // We've potentially found many matches.  We save this as a local
              // iterator and then use it in the future.
              _local = iter
              findNext
          }
        } else {
          // We have exhausted the permutations.  We have exhausted this
          // iterator.
          _exhausted = true
          Debugger("matching", "A Exhausted.")
        }
      }
    } // findNext method
  } // AMatchIterator class
}
