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

abstract class MatchIterator extends Iterator[Bindings] {
  /** The current match, if any. */
  protected var _current: Bindings = null
  
  /** Whether this matcher is exhausted. */
  protected var _exhausted = false
  
  /**
   * Return the next match, or `null` if no more matches remain.
   * 
   * @return	The next match or `null`.
   */
  def next =
    if (hasNext) {
      val nextMatch = _current
      _current = null
      nextMatch
    } else {
      null
    }
  
  /**
   * Determine if there is a next match.
   * 
   * @return	True if there is a next match, and false if not.
   */
  def hasNext = {
    if (_exhausted)
      false
    else {
      if (_current == null) findNext
      _current != null
    }
  }
  
  /**
   * Find the next match.  At the end of running this method either we
   * have _current set to the next match, or not.
   */
  protected def findNext
}

object MatchIterator {
	/**
	 * Build an iterator to handle completing matches from another match
	 * iterator.
	 * 
	 * This is provided to help in the case that a subordinate match (say of a
	 * child atom) returns many matches.  When this happens, you can use this
	 * method to create an iterator, given two things:
	 *  - A closure for `localMatch`.  This is the "thing to do" with each
	 *    subordinate match to generate a complete overall match.  That is, given
	 *    a single match for a child, we do this to create a complete match.  Any
	 *    outcome is possible: no match, a single match, or many matches.  These
	 *    cases are correctly handled by this iterator.
	 *  - The subordinate match iterator `subiter` that provides the subordinate
	 *    matches.
	 * 
	 * @param localMatch	Additional work to complete a match, given a subordinate
	 * 										match.  This takes the binding from the subordinate match
	 * 										and yields a new match outcome.
	 * @param	subiter			The subordinate iterator.
	 * @return	The new match iterator.
	 */
  def apply(localMatch: (Bindings) => Outcome,
      subiter: MatchIterator): MatchIterator =
        new SubMatchIterator(localMatch, subiter)
}

/**
 * Handle iterating over a collection of matches.
 * 
 * This is provided to help in the case that a subordinate match (say of a child
 * atom) returns many matches.  When this happens, you can create an instance
 * of this class and provide two things:
 *  - A closure for `localMatch`.  This is the "thing to do" with each
 *    subordinate match to generate a complete overall match.  That is, given
 *    a single match for a child, we do this to create a complete match.  Any
 *    outcome is possible: no match, a single match, or many matches.  These
 *    cases are correctly handled by this iterator.
 *  - The subordinate match iterator `subiter` that provides the subordinate
 *    matches.
 * 
 * @param localMatch	Additional work to complete a match, given a subordinate
 * 										match.  This takes the binding from the subordinate match
 * 										and yields a new match outcome.
 * @param	subiter			The subordinate iterator.
 */
private class SubMatchIterator(val localMatch: (Bindings) => Outcome,
  val subiter: MatchIterator) extends MatchIterator {
  
  /*++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
   * This class operates as follows.
   * 
   * An instance is passed a subordinate iterator, perhaps from a child atom,
   * and a closure that performs whatever additional work is necessary to
   * generate a full match.
   * 
   * The next match is generated by first getting the next match from the
   * subordinate iterator, obtaining a binding.  Then the local match closure
   * is invoked to generate a match outcome.  There are three possibilities.
   * 
   * (1) The complete match fails.  In this case the next subordinate match
   *     is obtained from the iterator, until it is exhausted.
   *     
   * (2) There is a single match.  In this case the binding is stored as the
   *     current binding and will be returned by next.
   *     
   * (3) Many matches are obtained.  In this case the resulting match
   *     iterator is stored in the localiter field.  The next match is
   *     obtained and stored as the current binding, and will be returned
   *     by next.
   *     
   * Essentially matching runs in this manner until a binding is successfully
   * obtained - or the subordinate iterator is exhausted.
   * 
   * Generating the next match is the job of getNextMatch, which is responsible
   * for returning a binding.  If no binding can be generated, then null is
   * returned to indicate exhaustion.
   * 
   * The exhausted flag indicates whether this iterator is exhausted.  Setting
   * and checking this flag is the responsibility of the hasNext method; no
   * other methods reference it.
   * 
   * If current is set to a binding, then the next method will return it.  If
   * current is set to null, then the next method invokes hasNext, whose job
   * it is to figure out if there is a next match.  To do that, hasNext
   * invokes getNextMatch, if necessary.
   ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++*/
  
  /** A local iterator generated by the localMatch, if any. */
  private var _local: MatchIterator = null
    
  /**
   * The method to generate the next match.
   */
  protected def findNext {
    // This method is very un-scala and highly imperative, but that seems the
    // simplest, most understandable approach.
    _current = null

    // Keep going until we have exhausted the local iterator, if it is set.
    if (_local != null && _local.hasNext) _current = _local.next
    else {
      // The local iterator can be discarded.
      _local = null
      
	    // Now handle the child iterator.
	    if (subiter.hasNext) {
	      // Obtain the next subordinate match.  We get a collection of bindings.
	      // Then apply the local matching process and handle the result.
	      localMatch(subiter.next) match {
	        case Fail(_,_) =>
	          // We continue to search, and only fail if we exhaust all attempts.
	          findNext
	        case Match(binds) =>
	          // We found a single binding.  Save it as the current match.
	          _current = binds
	        case Many(iter) =>
	          // We've found potentially many matches.  Save this as a local
	          // iterator and then use it in the future.
	          _local = iter
	          findNext
	      }
      } else {
        // We have exhausted the iterator.
        _exhausted = true
      }
    }
  }
}
