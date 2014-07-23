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
package ornl.elision.core

/**
 * Provide an iterator over multiple matches.
 * 
 * == Purpose ==
 * A single attempt to match a pattern and subject can result in no match,
 * a single match, or many possible matches.  In the last case we need a way
 * to communicate these, and that is the purpose of the match iterator.
 * 
 * == Use ==
 * This class is the common base class for the custom match iterators and
 * match iterator combinators.  It should work like any other iterator.
 */
abstract class MatchIterator extends Iterator[Bindings] {
  /**
   * The current match, if any.  Set this in the `hasNext` method and return.
   * It will be the next match returned by the iterator through `next`, which
   * will then set this back to `null`.
   */
  protected var _current: Bindings = null
  
  /**
   * Whether this iterator is exhausted.  Set this to true in `hasNext` to
   * indicate that it is, and then return.
   */
  protected var _exhausted = false
  
  /**
   * If this is non-null, then it is a source of matches to return through
   * the next method until exhausted.  Once these are exhausted, then the
   * `hasNext` method will resume.
   * 
   * You can use this in the case that `hasNext` needs to return a match
   * iterator.  Then set `_local` to the iterator and return.  You do not
   * need to set `_current`.
   */
  protected var _local: MatchIterator = null
  
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
  
  import scala.annotation.tailrec
  /**
   * Determine if there is a next match.
   * 
   * @return	True if there is a next match, and false if not.
   */
  final def hasNext: Boolean = {
//    println("(.)")
    if (_exhausted) {
      // The iterator is exhausted.
      return false
    } else if (BasicAtom.rewriteTimedOut) {
      // Rewriting timed out.
      _exhausted = true
      return false
    } else if (_current != null) {
      // A current binding is available.
      return true
    } else if (_local != null) {
      if (_local.hasNext) {
        // The current binding is the next binding from the local iterator.
//	println("--")
        _current = _local.next
        return true
      } else {
//	println("++")
        // The local iterator is exhausted.
        _local = null
      }
    }
//    println("(..)")
    // Go and find the next match.  This will set the fields properly, so we
    // need to repeat the above.
    findNext
//    println("(...)")
    
    // This needs to be the last thing here so it gets optimized properly.
    return hasNext
  }

  /**
   * Given a match iterator and a generator for a match iterator, build a new
   * match iterator combining the two.
   * 
   * A binding is obtained from the first, and this is used to generate the
   * second.  The second then yields complete matches.
   * 
   * An example is the following.
   * {{{
   * new ConstantMatcher() ~ (binds => new MatchCompleter(binds))
   * }}}
   * 
   * @param second	A closure that builds a new match iterator given initial
   * 								bindings.
   * @return	A new match iterator.
   */
  def ~(second: (Bindings => MatchIterator)) = {
    val first = this
    new MatchIterator {
      def findNext {
//        println("~ findNext")
        // We only come here if the local iterator has been exhausted, so we
        // need to build a new complete match.  We do that now.
        if (first.hasNext) {
          // Get the next match from the first iterator.
          val part1 = first.next
          // Build a complete match.
          _local = second(part1)
        } else {
          // The first iterator is exhausted.
          _exhausted = true
        }
      } // End of findNext method
    } // End of MatchIterator class
  }
  
  /**
   * Given a match iterator and a generator for a match outcome, build a new
   * match iterator combining the two.
   * 
   * A binding is obtained from the first, and this is used to generate the
   * second.  The second then yields complete matches.
   * 
   * An example is the following.
   * {{{
   * new ConstantMatcher() ~> (binds => x.tryMatch(y,binds))
   * }}}
   * 
   * @param second	A closure that builds a new match outcome given initial
   * 								bindings.
   * @return	A new match iterator.
   */
  def ~>(second: (Bindings => Outcome)) = {
    val first = this
    new MatchIterator {
      def findNext {
//        println("~> findNext")
        // We only come here if the local iterator has been exhausted, so we
        // need to build a new complete match.  We do that now.
        if (first.hasNext) {
          // Get the next match from the first iterator.
          val part1 = first.next
          // Build a complete match.
          second(part1) match {
            case fail:Fail =>
              // This match combination did not work, but try again.
              findNext
            case Match(binds) =>
              // We obtained a new complete match.
              _current = binds
            case Many(iter) =>
              // We have a new local iterator.
              _local = iter
          }
        } else {
          // The first iterator is exhausted.
          _exhausted = true
        }
      }
    }
  }
  
  /**
   * Find the next match.  At the end of running this method either we
   * have `_current` set to the next match, `_local` set to an iterator,
   * or `_exhausted` set to true.
   * 
   * If you have a local iterator you may also set `_current` if you wish.
   * Any value you set for `_current` will be used before the local iterator
   * is consulted.
   */
  protected def findNext
}

/**
 * Construct match iterators.
 */
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
  
  /**
   * Build an iterator that returns exactly one binding, and is then
   * exhausted.
   * 
   * @param binds	The one binding to return.
   */
  def apply(binds: Bindings) = new MatchIterator {
    _current = binds
    def findNext = {
//      println("empty findNext")
      _exhausted = true
    }
  }
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
    
  /**
   * The method to generate the next match.
   */
  protected def findNext {
//    println("generic findNext")
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
