/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package sjp.elision.core.matcher
import sjp.elision.core._

/**
 * Match two sequences, where the elements of the second sequence can be
 * grouped.
 */
object AMatcher {

  /**
   * Attempt to match two lists.  The second list can be re-grouped.
   * 
   * @param plist	The pattern list.
   * @param slist	The subject list.
   * @param binds	Bindings that must be honored in any match.
   * @return	The match outcome.
   */
  def tryMatch(plist: AtomList, slist: AtomList, binds: Bindings): Outcome = {
    if (plist.atoms.length > slist.atoms.length)
      return Fail("More patterns than subjects, so no match is possible.",
          plist, slist)
      
    // If there are the same number, then this is a simple case of matching.
    if (plist.atoms.length == slist.atoms.length)
      return SequenceMatcher.tryMatch(plist.atoms, slist.atoms, binds)
      
    // We need to group the atoms so there is the same number of patterns and
    // subjects.  If there are N subjects and M patterns (with M < N per the
    // above checks) then we essentially insert M-1 markers between elements
    // of the N subjects.  Get it?  We use a special iterator for that.
    val iter = new AMatchIterator(plist.atoms, slist.atoms, binds)
    if (iter.hasNext) return Many(iter)
    else Fail("The lists do not match.", plist, slist)
  }
  
  /* How associative matching works.
   * 
   * There must be at least as many subjects as patterns.  If there is an
   * equal number, then we use the sequence matcher to perform the match.
   * Otherwise we need to group the items to perform the match.
   *
   * During phase one of matching, we try to match and eliminate any atoms
   * that are not bindable.  This is done in an iterative fashion; each
   * non-bindable pattern is matched against each subject and if a match is
   * successful we continue to the next non-bindable pattern until we match
   * them all.  If this matching ever fails, we have exhausted the matcher.
   * 
   * The trick is that, since the lists are not commutative, if pattern P
   * matches at position i, then the next pattern Q is only allowed to match
   * at position j>=i (equality is possible since we discard the matching
   * elements).  This restriction is not needed in full associative and
   * commutative matching.
   * 
   * Once we have a complete match for all non-bindable patterns (which get
   * removed from the pattern and sequence) we then enter phase two.  During
   * phase two, we first check to see if we have a single variable pattern.
   * If so, we immediately bind it to the residual subjects and we are done.
   * If not, then we generate all parenthesizations of the subjects and try
   * to match each one.
   * 
   * If we have N subjects and we have P patterns (with P<N), then we can
   * think of the groupings as distributing (P-1) markers among (N-1) slots.
   * We can do this in (N-1) C (P-1) ways.
   */
  
  /**
   * Perform commutative matching on two lists.  The subjects can be
   * arbitrarily re-ordered.
   * 
   * This implements phase one of the matching.  The idea is to match any
   * non-bindable patterns.  Since this can happen in many ways, we use an
   * iterator.
   * 
   * Phase two performs groupings of the remaining stuff against the bindables.
   * 
   * @param patterns	The patterns.
   * @param subjects	The subjects.
   * @param binds			Bindings to honor.
   */
  private class AMatchIterator(patterns: OmitSeq[BasicAtom],
      subjects: OmitSeq[BasicAtom], binds: Bindings) extends MatchIterator {
    def findNext {
      
    }
  }
}