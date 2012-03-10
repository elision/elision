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
  def tryMatch(plist: AtomList, slist: AtomList, binds: Bindings): Outcome = {
    if (plist.atoms.length > slist.atoms.length)
      return Fail("More patterns than subjects, so no match is possible.",
          plist, slist)
          
    // If there are the same number, then this is a simple case of commutative
    // matching.
    if (plist.atoms.length == slist.atoms.length)
      return CMatcher.tryMatch(plist, slist, binds)
      
    // In any case we need to remove any constants in the two lists.
    val (patterns, subjects, fail) = MatchHelper.eliminateConstants(plist, slist)
    if (fail.isDefined) return fail.get
      
    // This is not so simple.  We need to perform the match.
    Fail("")
  }
  
  /* How associative and commutative matching works.
   * 
   */
}