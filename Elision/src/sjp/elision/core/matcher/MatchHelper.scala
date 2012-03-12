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
 * Provide some support methods for matching.
 */
object MatchHelper {

  /**
   * Given two lists of atoms, identify and remove any constants from the two
   * lists, returning the resulting lists.
   * 
   * @param plist	The pattern list.
   * @param slist	The subject list.
   */
  def eliminateConstants(plist: AtomList, slist: AtomList):
  (OmitSeq[BasicAtom], OmitSeq[BasicAtom], Option[Fail] ) = {
    var patterns = plist.atoms
    var subjects = slist.atoms
    for ((pat, pindex) <- plist.constantMap) {
      slist.constantMap.get(pat) match {
        case None =>
          return (patterns, subjects, Some(Fail("Element " + pindex +
              " not found in subject list.", plist, slist)))
        case Some(sindex) =>
          println("Omitting " + pat.toParseString)
          patterns = patterns.omit(pindex)
          subjects = subjects.omit(sindex)
      }
    } // Omit constants from the lists.
    println("Now: Patterns: " + patterns.mkParseString("",",",""))
    println("     Subjects: " + subjects.mkParseString("",",",""))
    (patterns, subjects, None)
  }
  
  /**
   * Match and remove unbindables from two list.
   * 
   * @param plist				The pattern list.
   * @param slist				The subject list.
   * @param binds				Bindings to honor.
   * @param commutative	If true, matching is commutative.
   */
  private class UnbindableMatcher(plist: AtomList, slist: AtomList,
      binds: Bindings, commutative: Boolean) extends MatchIterator {
    var patterns = plist.atoms
    var subjects = slist.atoms
    
    /** Bindable pattern indices. */
    var bpi = List[Int]()
    
    /** Bindable subject indices. */
    var bsi = List[Int]()
    
    /** The matchers for each pattern. */
    var snm: Array[SeekNextMatch] = null
    
    /* How this works.
     * 
     * We traverse the list of patterns and, for each pattern that is not
     * bindable, we create a specialized matcher (the SeekNextMatch matcher).
     *
     * We obtain a match from the first item, and pass that to the next
     * matcher in the sequence.  We get its match and pass it along, and so
     * forth, until we have "primed" them all.  Then we run all the matches
     * from the last one.  When we exhaust the last one, we return to the
     * prior matcher and get its next match.  We continue in this fashion
     * until we exhaust the first matcher, at which point we are done.
     */
    private def init {
      // Find all the non-bindable patterns.  If we don't find any, then
      // there is nothing more to do.
      for (position <- 0 until patterns.length) {
        if (!patterns(position).isBindable)
          bpi = position :: bpi
      } // Find all bindable patterns.
      
      // Find all the non-bindable subjects.
      for (position <- 0 until subjects.length) {
        if (!subjects(position).isBindable)
          bsi = position :: bsi
      }
      
      // If there are fewer non-bindable subjects than there are patterns,
      // then no match is possible and we are immediately exhausted.
      if (bsi.length < bpi.length) _exhausted = true
      
      // Build the matchers.
      snm = new Array(bpi.length)
      for (index <- 0 until bpi.length) {
        snm(index) = new SeekNextMatch(patterns(bpi(index)))
      } 
    }
    init
    
    def findNext {
      
    }
  }
  
  /**
   * Iterate over the ways the given pattern can match a non-bindable subject
   * in the list of subjects.  We try all matches of pattern and subject.
   * Before you use this, you must invoke `init` and pass the bindings to use
   * and the list of subjects; otherwise you will get a `NullPointerException`.
   * 
   * @param pattern	The pattern to match.
   */
  private class SeekNextMatch(pattern: BasicAtom) extends MatchIterator {
    /** The index of the subject to check.  We increment before we check. */
    private var _sindex = -1
    
    /** Bindings to honor in any match. */
    private var _binds: Bindings = null
    
    /** The subjects to try to match. */
    private var _subjects: OmitSeq[BasicAtom] = null
    
    /**
     * Initialize the iterator.
     * 
     *  @param binds		The bindings to honor.
     *  @param subjects	The subjects we can match.
     */
    def init(binds: Bindings, subjects: OmitSeq[BasicAtom]) {
      _binds = binds
      _subjects = subjects
    }
    
    /**
     * Find the next match.  This sets the _current, _local, and _exhausted
     * fields.
     */
    def findNext {
      // Use the local matches until we have used them up.
      if (_local != null && _local.hasNext) {
        _current = _local.next
      }
      _local = null
      
      // Traverse the list of potential subjects and find one that works.  We
      // allow throwing a null pointer exception here if the class has not been
      // initialized.  It is private, after all.
      _sindex += 1
      if (_sindex >= _subjects.length) {
        _exhausted = true
        return
      } 
      val subject = _subjects(_sindex)
      if (!subject.isBindable) pattern.tryMatch(subject, _binds) match {
        case Fail(_,_) =>
          // We do not care; we only care about the case that we exhaust
          // all potential matches.  We fall through and go around the loop
          // again.
          findNext
          return
        case Match(binds) =>
          // We found a match.  Save this as the local current match.
          _current = binds
          return
        case Many(iter) =>
          // We found many matches.  Save this as the local iterator and
          // try again.
          _local = iter
          findNext
          return
      }
      
      // The subject was not bindable.  Move to the next one.
      findNext
      return
    }
  }
}