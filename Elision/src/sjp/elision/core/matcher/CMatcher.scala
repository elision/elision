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
 * Match two sequences whose elements can be re-ordered.  That is, the lists are
 * commutative.
 */
object CMatcher {

  /**
   * Attempt to match two lists.  The second list can be re-ordered arbitrariliy.
   * 
   * @param plist	The pattern list.
   * @param slist	The subject list.
   * @param binds	Bindings that must be honored in any match.
   * @return	The match outcome.
   */
  def tryMatch(plist: AtomList, slist: AtomList, binds: Bindings): Outcome = {
    if (plist.atoms.length != slist.atoms.length)
      return Fail("Lists are different sizes, so no match is possible.",
          plist, slist)
      
    // Step one is to perform constant elimination.  For each constant
    // pattern, find and remove the same constant pattern from the subjects.
    // If we cannot, we do not match.
    val (patterns, subjects, fail) = MatchHelper.eliminateConstants(plist, slist)
    if (fail.isDefined) return fail.get
    
    // Step two is to re-order the subjects and match until we succeed, or we
    // exhaust the search space.  We have to do this with a match iterator, but
    // we also have to check for a single match first.
    val iter = new CMatchIterator(patterns, subjects, binds)
    if (iter.hasNext) return Many(iter)
    else Fail("The lists do not match.", plist, slist)
  }
  
  /* How commutative matching works.
   * 
   * The lists must be the same length.  We immediately match and remove any
   * constants from both lists.  Next we need to try to match the remainder.
   * We generate all permutations of the subject list, and for each one, we
   * try to match it against the pattern list using the sequence matcher.  If
   * the match succeeds, we yield the match.  If it does not, we continue
   * to look for a match until we exhaust all permutations.
   */
  
  /**
   * Perform commutative matching on two lists.  The subjects can be
   * arbitrarily re-ordered.
   * 
   * @param patterns	The patterns.
   * @param subjects	The subjects.
   * @param binds			Bindings to honor.
   */
  private class CMatchIterator(patterns: OmitSeq[BasicAtom],
      subjects: OmitSeq[BasicAtom], binds: Bindings) extends MatchIterator {
    /** An iterator over all permutations of the subjects. */
    private val _perms = subjects.permutations
    
    /** A local iterator returning matches. */
    private var _local: MatchIterator = null
    
    /**
     * Find the next match.  At the end of running this method either we
     * have _current set to the next match, or not.
     */
    protected def findNext {
      print("Searching... ")
      _current = null
      if (_local != null && _local.hasNext) _current = _local.next
      else {
        _local = null
	      if (_perms.hasNext)
	        SequenceMatcher.tryMatch(patterns, _perms.next, binds) match {
	        case Fail(_,_) =>
	          // We ignore this case.  We only fail if we exhaust all attempts.
	          findNext
	        case Match(binds) =>
	          // This case we care about.  Save the bindings as the current match.
	          _current = binds
	          println("Found.")
	        case Many(iter) =>
	          // We've potentially found many matches.  We save this as a local
	          // iterator and then use it in the future.
	          _local = iter
	          findNext
	      } else {
	        // We have exhausted the permutations.  We have exhausted this
	        // iterator.
	        _exhausted = true
	        println("Exhausted.")
	      }
      }
    }
  }
}