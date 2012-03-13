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
 * @author sprowell
 *
 */
class Matcher(patterns: OmitSeq[BasicAtom], subjects: OmitSeq[BasicAtom],
    binds: Bindings) extends MatchIterator {

  /* How unbindables are matched during commutative matching.
   * 
   * We invoke matchUnbindables, passing the patterns and the subjects.  This
   * triggers a call to matchNextUnbindable, passing the indices zero and zero
   * for the pattern and subject.
   * 
   * The first unbindable pattern is obtained, and then the first unbindable
   * subject is obtained, starting from the given indices.  They are matched.
   * If the match fails, then the subject index is advanced.  If we run off
   * the end of the subjects, we exhaust the matcher.  Otherwise we obtain
   * a match (possibly an iterator).
   * 
   * We save the iterator as a local iterator, and then increment the pattern
   * index and set the subject index to zero.  The matching subject is dropped
   * from the list, and we invoke matchNextUnbindable with the new lists and
   * indices.  When we do this, we must pass along the match so far.  If we
   * have a single bind, then we pass it.  If we have a local iterator, and
   * it is not exhausted, we pass the next binding from the iterator.
   * 
   * If we fail to find an unbindable pattern, we have completed the match.
   * We return to the caller.  The bindings we were passed are the next binds
   * to return from the overall iterator.
   * 
   * When matchNextUnbindable returns and we have a local iterator, we get
   * the next match and re-invoke matchNextUnbindable.  When we return to
   * matchUnbindables, we have exhausted the iterator.
   * 
   * Context is preserved inside the overall iterator by having the
   * matchNextUnbindable actually create an object.  We track the last
   * object in the chain.
   */
  
  def findNext {
    // If we had a local iterator, the match iterator infrastructure would use
    // it.  Therefore we do not, and we need to generate the next match.
    
  }
}