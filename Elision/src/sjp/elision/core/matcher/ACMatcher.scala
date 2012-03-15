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
 * Match two sequences whose elements can be re-ordered or re-grouped.  That is,
 * the lists are associative and commutative.
 */
object ACMatcher {

  /**
   * Attempt to match two lists.  The second list can be re-ordered and
   * re-grouped arbitrarily.
   * 
   * @param plist	The pattern list.
   * @param slist	The subject list.
   * @param binds	Bindings that must be honored in any match.
   * @return	The match outcome.
   */
  def tryMatch(plist: AtomList, slist: AtomList, binds: Bindings, op: Option[Operator]): Outcome = {
    if (plist.atoms.length > slist.atoms.length)
      return Fail("More patterns than subjects, so no match is possible.",
          plist, slist)
          
    // If there are the same number, then this is a simple case of commutative
    // matching.
    if (plist.atoms.length == slist.atoms.length)
      return CMatcher.tryMatch(plist, slist, binds)
      
    // Step one is to perform constant elimination.  Any constants must match
    // exactly, and we match and remove them.
    var (patterns, subjects, fail) = MatchHelper.eliminateConstants(plist, slist)
    if (fail.isDefined) return fail.get
    
    // Step two is to match and eliminate any unbindable atoms.  These are
    // atoms that are not variables, and so their matching is much more
    // restrictive.  We obtain an iterator over these, and then combine it
    // with the iterator for "everything else."
    val um = new UnbindableMatcher(patterns, subjects, binds)

    // Discard unbindables from both lists.
    patterns = patterns.filter(_.isBindable)
    subjects = subjects.filter(_.isBindable)
    println("Removing Unbindables: Patterns: " + patterns.mkParseString("",",",""))
    println("                      Subjects: " + patterns.mkParseString("",",",""))
    
    // Now, check the number of patterns remaining.  If there are none, we are
    // finished, and the unbindable matcher is the result.
    if (patterns.length == 0) return Many(um)
    
    // If there is just one pattern remaining, then it must get all the
    // remaining subjects.
    if (patterns.length == 1) {
      // What we do depends on the number of subjects.
      if (subjects.length == 1) {
        // Bind the pattern to the subject and return the result.
        val iter = um ~> (bnd => patterns(0).tryMatch(subjects(0), bnd))
        if (iter.hasNext) return Many(iter)
        else Fail("The lists do not match.", plist, slist)
      }
    }
    
    // This is not so simple.  We need to perform the match.
    Fail("")
  }
  
  /* How associative and commutative matching works.
   * 
   * The subject list must be at least as long as the pattern list, or no
   * match is possible.  
   */
}