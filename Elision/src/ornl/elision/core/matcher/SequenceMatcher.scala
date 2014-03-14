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

import ornl.elision.core.Apply
import ornl.elision.core.AtomSeq
import ornl.elision.core.BasicAtom
import ornl.elision.core.Bindings
import ornl.elision.core.Fail
import ornl.elision.core.Many
import ornl.elision.core.Match
import ornl.elision.core.MatchIterator
import ornl.elision.core.Operator
import ornl.elision.core.OperatorRef
import ornl.elision.core.Outcome
import ornl.elision.core.giveMkParseString
import ornl.elision.core.wrapBindingsAtom
import ornl.elision.util.Debugger
import ornl.elision.util.OmitSeq
import ornl.elision.util.OmitSeq.fromIndexedSeq
import ornl.elision.core.AtomSeq
import ornl.elision.core.Literal
import ornl.elision.core.Variable
import ornl.elision.core.NoProps

/**
 * Match two sequences of atoms.
 *
 * == Purpose ==
 * The goal here is to make matching simple(r).  For complex objects, you can
 * have several items to match, any of which can return a match iterator, etc.
 * It is best to concentrate that complexity here, and create a more general
 * solution (or so the logic goes).
 *
 * == Use ==
 * To use this, provide two sequences of atoms.  The sequences must be the
 * same length, or the whole match is immediately rejected.  This matcher
 * tries to match the respective atom pairs in order, from the first to the
 * last.  Matches are returned iff the entire sequence matches.
 */
object SequenceMatcher {

  // add_bind adds a binding to a set of bindings, ensuring that it
  // does not conflict (i.e. that the name is not bound to a different
  // value) If bindings conflict, then returns None; otherwise, it
  // adds e to the bindings in binds, and returns Some(binds++e). If
  // the binding is already there, then it leaves it alone and returns
  // the original.
  def add_bind(binds: Option[Bindings],
    e: (String, BasicAtom)): Option[Bindings] = {
    (e, binds) match {
      case ((n, a), Some(b)) =>
        try {
          if (b(n) == a) { binds } else { None }
        } catch {
          case _: Throwable => Some(b + (e))
        }
      case (_, None) => None
    }
  }

  // Adds one set of bindings to another, ensuring that nothing in the
  // second set of bindings conflicts with anything in the first, and
  // that none of the bindings in the second set conflict with each
  // other
  def add_binds(binds: Bindings, newbinds: Bindings): Option[Bindings] = {
    (newbinds.toList).foldLeft[Option[Bindings]](Some(binds))(add_bind)
  }

  // see comments on get_mandatory_bindings in ACMatcher. The
  // SequenceMatcher version of get_mandatory_bindings is not
  // complete! It should have another case in the match statement that
  // handles Apply data structures in the plist.head match {...}, and
  // it should be invoked from SequenceMatcher.tryMatch for
  // completeness.
  def get_mandatory_bindings(plist: AtomSeq, slist: AtomSeq,
    ibinds: Bindings): Option[Bindings] = {
    Debugger("matching", "called SequenceMatcher.get_mandatory_bindings")
    var binds = ibinds

    // add error checking: what if both lists are not the same length
    if (plist.isEmpty && slist.isEmpty) {
      Debugger("matching", "empty lists")
      return Some(binds)
    } else {
      Debugger("matching", "looking at head")
      plist.head match {
        case Variable(typ, nam, gua, lab, byn) =>
          Debugger("matching", "found a variable")
          Debugger("matching", nam + " function " + slist.head.toParseString)
          add_bind(Some(ibinds), (nam, slist.head)) match {
            case None =>
              return None
            case Some(b) =>
              Debugger("matching", "Recursive call to get_mandatory_bindings")
              return get_mandatory_bindings(
                AtomSeq(plist.props, plist.tail),
                AtomSeq(slist.props, slist.tail), b)
          }
        case Apply(OperatorRef(Operator(np, tp, AtomSeq(oprpp, oargp))),
          AtomSeq(prpp, argp)) =>
          Debugger("matching", "SequenceMatcher found pattern operator: " + np + " arg: " + oargp.toString)

          slist.head match {
            case Apply(OperatorRef(Operator(ns, ts, AtomSeq(oprps, oargs))),
              AtomSeq(prps, argss)) =>

              // The pattern operator and the subject operator are different.
              // No match is possible so don't bother looking at the argument
              // list. Return None to indicate no match possible. 
              if (np != ns) {
                Debugger("matching", "SequenceMatcher found nonmatching operators: " + np + " != " + ns)
                return None
              }

              if (oprps.isA(false) && oprps.isC(false)) {
                Debugger("matching", "SeqenceMatcher found AC operator " + ns)
                ACMatcher.get_mandatory_bindings(AtomSeq(prpp, argp), AtomSeq(prps, argss), ibinds) match {
                  case None => return None
                  case Some(b) => binds = b
                }
              } else {
                Debugger("matching", "SeqenceMatcher found non-AC operator " + ns)
                get_mandatory_bindings(AtomSeq(prpp, argp), AtomSeq(prps, argss), ibinds) match {
                  case None => return None
                  case Some(b) => binds = b
                }
              }
          }
        case _ => return Some(binds)
      }
    }
    Some(binds)
  }

  /**
   * Match two sequences of atoms, in order.
   *
   * @param patterns	The patterns.
   * @param subjects	The subjects.
   * @param binds			Bindings to honor in any match.  This can be omitted.
   * @return	The result of the match.
   */
  def tryMatch(patterns: OmitSeq[BasicAtom], subjects: OmitSeq[BasicAtom],
    binds: Bindings = Bindings()): Outcome = {
    Debugger("matching") {
      Debugger("matching", "Sequence Matcher called: ")
      Debugger("matching", "    Patterns: " + patterns.mkParseString("", ",", ""))
      Debugger("matching", "    Subjects: " + subjects.mkParseString("", ",", ""))
      Debugger("matching", "    Bindings: " + binds.toParseString)
    }
    //Properties should have been taken into account at creation time, so we ignore them here.
    val plist = AtomSeq(NoProps, patterns)
    val slist = AtomSeq(NoProps, subjects)
    var mbinds = binds
    get_mandatory_bindings(plist, slist, binds) match {
      case None => return Fail((() => "Mandatory-bindings induced fail"), 0)
      case Some(b) =>
        Debugger("matching", "binding results: ")
        Debugger("matching", b.toParseString)
        mbinds = b
    }

    Debugger("SequenceMatcher", "->trymatch")
    Debugger("SequenceMatcher", "plist:")
    Debugger("SequenceMatcher", plist.mkParseString("", ",", ""))
    Debugger("SequenceMatcher", "slist")
    Debugger("SequenceMatcher", slist.mkParseString("", ",", ""))
    Debugger("SequenceMatcher", "mbinds")
    Debugger("SequenceMatcher", mbinds.toParseString)
    Debugger("SequenceMatcher", "SequenceMatcher mandatory bindings: " + mbinds.toParseString)

    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      Fail("Timed out")
    } else if (patterns.length != subjects.length) {
      Fail("Sequences are not the same length.")
    } else {
      _tryMatch(patterns, subjects, mbinds, 0)
    }
  }

  /**
   * Rewrite a sequence of atoms by applying the given bindings to each.
   *
   * @param subjects	The atoms to rewrite.
   * @param binds			The bindings to apply.
   * @return	A pair consisting of the rewritten sequence of atoms and a flag
   * 					that is true if any rewrites succeeded.
   */
  def rewrite(subjects: OmitSeq[BasicAtom], binds: Bindings) = {
    var changed = false
    var index = 0
    var newseq = OmitSeq[BasicAtom]()
    while (index < subjects.size) {
      val (newatom, change) = subjects(index).rewrite(binds)
      changed |= change
      newseq :+= newatom
      index += 1
    } // Rewrite the subjects.
    if (changed) (newseq, changed) else (subjects, false)
  }

  /**
   * Match two sequences of atoms, in order.
   *
   * @param patterns	The patterns.
   * @param subjects	The subjects.
   * @param binds			Bindings to honor in any match.
   * @param position	Index into the list.  This is only used to generate
   * 									a failure index to return to the caller.
   * @return	The result of the match.
   */
  private def _tryMatch(patterns: OmitSeq[BasicAtom],
    subjects: OmitSeq[BasicAtom], binds: Bindings, position: Int): Outcome = {

    // Watch for the basis case.  If the patterns list is empty, we are done
    // and return a successful match.
    if (patterns.isEmpty) return Match(binds)

    // Try to match the heads of the list.  This generates one of three
    // possible outcomes, and we figure out what to do based on the particular
    // outcome.
    patterns.head.tryMatch(subjects.head, binds, None) match {
      case fail: Fail =>
        // The atoms do not match.  There is nothing further to try; the
        // entire match can be rejected at this point, so return failure.
        Fail("Elements do not match.",
          patterns.head, subjects.head, Some(fail), position)
      case Match(newbinds) =>
        // We found exactly one way in which the pattern could match, given the
        // bindings we received.  This is easy; proceed to the next element.
        _tryMatch(patterns.tail, subjects.tail, (newbinds ++ binds), position + 1)
      case Many(miter) =>
        // Matching returned a match iterator.  This is the nontrivial case.
        // Now we have to be careful, and build another match iterator that
        // performs the rest of the matching work.  We do that now.
        // NOTE  It may be more efficient to replace this recursion.
        val (pt, st) = (patterns.tail, subjects.tail)
        Many(MatchIterator(
          (newbinds: Bindings) =>
            _tryMatch(pt, st, (newbinds ++ binds), position + 1), miter))
    }
  }
}
