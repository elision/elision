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

/**
 * Match two sequences whose elements can be re-ordered.  That is, the lists are
 * commutative.
 */
object CMatcher {

  /**
   * Attempt to match two lists.  The second list can be re-ordered arbitrarily.
   * 
   * @param plist	The pattern list.
   * @param slist	The subject list.
   * @param binds	Bindings that must be honored in any match.
   * @return	The match outcome.
   */
  def tryMatch(plist: AtomSeq, slist: AtomSeq, binds: Bindings): Outcome = {

    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      return Fail("Timed out", plist, slist)
    }

    // Check the length.
    if (plist.length != slist.length)
      return Fail("Lists are different sizes, so no match is possible.",
          plist, slist)
          
    // If there are no patterns, there is nothing to do (since there are also
    // no subjects).
    if (plist.length == 0) return Match(binds)
      
    // Step one is to perform constant elimination.  For each constant
    // pattern, find and remove the same constant pattern from the subjects.
    // If we cannot, we do not match.
    var (patterns, subjects, fail) =
      MatchHelper.eliminateConstants(plist, slist)
    if (fail.isDefined) return fail.get
    
    // Step two is to match and eliminate any unbindable atoms.  These are
    // atoms that are not variables, and so their matching is much more
    // restrictive.  We obtain an iterator over these, and then combine it
    // with the iterator for the reorderings to get the entire match iterator.
    val um = new UnbindableMatcher(patterns, subjects, binds)
    
    // Step three is to re-order the subjects and match until we succeed, or we
    // exhaust the search space.  We have to do this with a match iterator, but
    // we also have to check for a single match first since we need to determine
    // precisely what to return from this method.
    val iter = um ~ (bindings => {

      // Have we timed out since the unbindable match iterator was
      // created?
      if (bindings == null) {

        // This set of bindings can never match. Return an empty iterator.
        new MatchIterator {
          _current = null
          _local = null
          _exhausted = true
          def findNext = {
            _exhausted = true
          }
        }
      }

      else {

        // Get the patterns and subjects that remain.
        val pats = bindings.patterns.getOrElse(patterns)
        val subs = bindings.subjects.getOrElse(subjects)
        
        // If there are no patterns, there is nothing to do.  We have matched.
        // Just to be safe, we set the patterns and subjects (that remain from
        // the unbindable matcher) so they are carried forward.
        if (pats.length == 0) {
          MatchIterator((bindings ++ binds).set(pats, subs))
        } else {
          new CMatchIterator(pats, 
                             subs, 
                             (bindings ++ binds).set(pats, subs))
        }
      }
    })
    
    // If there is a next match, return the iterator.  If not, return a failure.
    if (iter.hasNext) return Many(iter)
    else Fail("The lists do not match.", plist, slist)
  }
  
  /* How commutative matching works.
   * 
   * The lists must be the same length.
   * 
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
    
    /**
     * Find the next match.  At the end of running this method either we
     * have `_current` set to the next match or we have exhausted the
     * iterator.
     */
    import scala.annotation.tailrec
    @tailrec
    final protected def findNext {
      if (BasicAtom.traceMatching) print("C Searching... ")

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
        if (_perms.hasNext) {
          SequenceMatcher.tryMatch(patterns, _perms.next, binds) match {
            case fail:Fail =>
              // We ignore this case.  We only fail if we exhaust all attempts.
              if (BasicAtom.traceMatching) println(fail)
              findNext
            case Match(binds1) =>
              // This case we care about.  Save the bindings as the current match.
              _current = (binds ++ binds1).set(binds1.patterns.getOrElse(patterns),
                                               binds1.subjects.getOrElse(subjects))
              if (BasicAtom.traceMatching) println("C Found.")
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
          if (BasicAtom.traceMatching) println("C Exhausted.")
        }
      }
    }
  }
}
