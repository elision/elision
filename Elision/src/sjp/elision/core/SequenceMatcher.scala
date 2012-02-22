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

/**
 * Match two sequences of atoms.
 * 
 * The goal here is to make matching simple(r).  For complex objects, you can
 * have several items to match, any of which can return a match iterator, etc.
 * It is best to concentrate that complexity here, and create a more general
 * solution (or so the logic goes).
 * 
 * To use this, provide two sequences of atoms.  The sequences must be the
 * same length, or the whole match is immediately rejected.  This matcher
 * tries to match the respective atom pairs in order, from the first to the
 * last.  Matches are returned iff the entire sequence matches.
 * 
 * To use this invoke `tryMatch`, passing two sequences of atoms.
 */
object SequenceMatcher {

  /**
   * Match two sequences of atoms, in order.
   * @param patterns	The patterns.
   * @param subjects	The subjects.
   * @param binds			Bindings to honor in any match.  This can be omitted.
   * @return	The result of the match.
   */
  def tryMatch(patterns: Seq[BasicAtom], subjects: Seq[BasicAtom],
      binds: Bindings = new Bindings()): Outcome =
        if (patterns.length != subjects.length)
          Fail("Sequences are not the same length.")
        else tryMatch(patterns, subjects, binds, 0)
        
  /**
   * Rewrite a sequence of atoms by applying the given bindings to each.
   * 
   * @param subjects	The atoms to rewrite.
   * @param binds			The bindings to apply.
   * @return	A pair consisting of the rewritten sequence of atoms and a flag
   * 					that is true if any rewrites succeeded.
   */
  def rewrite(subjects: Seq[BasicAtom], binds: Bindings) = {
    var changed = false
    def doit(atoms: List[BasicAtom]): List[BasicAtom] =
      if (atoms.isEmpty) List() else {
        val (newatom, change) = atoms.head.rewrite(binds)
        changed |= change
        newatom :: doit(atoms.tail)
      }
    (doit(subjects.toList), changed)
  }

  /**
   * Match two sequences of atoms, in order.
   * @param patterns	The patterns.
   * @param subjects	The subjects.
   * @param binds			Bindings to honor in any match.
   * @param position	Index into the list.  This is only used to generate
   * 									a failure index to return to the caller.
   * @return	The result of the match.
   */
  private def tryMatch(patterns: Seq[BasicAtom], subjects: Seq[BasicAtom],
      binds: Bindings, position: Int): Outcome = {
    // Watch for the basis case.  If the patterns list is empty, we are done
    // and return a successful match.
    if (patterns.isEmpty) return Match(binds)
    
    // Try to match the heads of the list.  This generates one of three
    // possible outcomes, and we figure out what to do based on the particular
    // outcome.
    patterns.head.tryMatch(subjects.head, binds) match {
      case fail:Fail =>
        // The atoms do not match.  There is nothing further to try; the
        // entire match can be rejected at this point, so return failure.
        Fail("Elements do not match.",
            patterns.head, subjects.head, Some(fail), position)
      case Match(newbinds) =>
        // We found exactly one way in which the pattern could match, given the
        // bindings we received.  This is easy; proceed to the next element.
        tryMatch(patterns.tail, subjects.tail, newbinds, position+1)
      case Many(miter) =>
        // Matching returned a match iterator.  This is the nontrivial case.
        // Now we have to be careful, and build another match iterator that
        // performs the rest of the matching work.  We do that now.
        // NOTE  It may be more efficient to replace this recursion.
        val (pt, st) = (patterns.tail, subjects.tail)
        Many(new MatchIterator(
            (newbinds: Bindings) => tryMatch(pt, st, newbinds, position+1),
            miter))
    }
  }
}