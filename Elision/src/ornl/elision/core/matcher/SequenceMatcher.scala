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
import ornl.elision.core.ANY
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
import scala.language.reflectiveCalls
import ornl.elision.core.AtomSeq
import ornl.elision.core.Literal
import ornl.elision.core.Variable
import ornl.elision.core.NoProps
import ornl.elision.core.NamedRootType
import ornl.elision.core.BasicAtomComparator
import scala.Array

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
  
  /**
   * Reorder Apply's and AtomSeq's based on the expected computational cost to
   * match. Less computationally intensive sequences should be considered
   * before the more intensive ones. 
   * 
   * Because this is the sequence matcher, the correspondence between patterns
   * and their subjects is maintained.
   * 
   * @param pattern The pattern sequence to reorder
   * @param subject The subject sequence to reorder
   * 
   * @return A reordered (pattern, subject) tuple.
   */
  private def reorder_matchcost(pattern: OmitSeq[BasicAtom], subject: OmitSeq[BasicAtom]) = {
    // If we have at least two Applys, then sort the atoms, 
    // hopefully putting the easiest matches first      
    // Only sort if cost-to-match is not monotonically increasing
    // (i.e. if it's not already sorted sensibly)
    var maxcost = 0.0
    var monotonic = true
    val toSort = pattern.exists(p =>
      p match {
        case Apply(op, arg: AtomSeq) =>
          if (arg.matchingCost < maxcost) monotonic = false

          if (!monotonic) true
          else {
            maxcost = Math.max(maxcost, arg.matchingCost)
            false
          }
        case arg: AtomSeq =>
          if (arg.matchingCost < maxcost) monotonic = false

          if (!monotonic) true
          else {
            maxcost = Math.max(maxcost, arg.matchingCost)
            false
          }
        case _ => false
      })
    if (toSort) {
      var zippedSeq = pattern zip subject
      val multiplicity_sorted =
        zippedSeq.sortWith((l: (BasicAtom, BasicAtom), r: (BasicAtom, BasicAtom)) => {
          l._1 match {
            case la: BasicAtom =>
              r._1 match {
                case ra: BasicAtom =>
                  BasicAtomComparator(la, ra) == -1
                case _ => false
              }
            case _ => false
          }
        })
      val unzipped = multiplicity_sorted.unzip
      (OmitSeq.fromIndexedSeq(unzipped._1), OmitSeq.fromIndexedSeq(unzipped._2))
    } else (pattern, subject)
  }
  

  /**
   * Add a binding to a set of bindings, ensuring that it does not conflict
   * (i.e. that the same name is bound to the same value).  If bindings do
   * conflict then this will return `None`.  Otherwise this will add the
   * provided bind to the bindings, unless it is already there.
   * 
   * @param binds     The bindings.
   * @param e         The bind to add.
   * @return          The (possibly) modified bindings.
   */
  def add_bind(binds: Option[Bindings],
    e: (Variable, BasicAtom)): Option[Bindings] = {
    Debugger("SequenceMatcher", "Attempting to add binding " + e.toString + " to " + binds.toString())
    (e, binds) match {
      case ((n, a), Some(b)) =>
        val prev_bind = b.getOrElse(n.name, None)
        Debugger("SequenceMatcher", "b(n) = " + prev_bind)
        prev_bind match {
          case None => 
            e._1.bindMe(e._2, b) match {
              case Match(nbinds) =>
                Some(nbinds)
              case Many(nbinds) =>
                binds
              case fail: Fail =>
                None
            }
          
          case _ =>
            if (prev_bind == a) { return binds } else { None }
            var got_any = false
            prev_bind match {
              case typ: NamedRootType => if (typ.name == "ANY") got_any = true
              case _ => None
            }
            a match {
              case typ: NamedRootType => if (typ.name == "ANY") got_any = true
              case _ => None
            }
            if (got_any == true) { binds } else { None }
        }
      case _ => None
    }
  }

  // Adds one set of bindings to another, ensuring that nothing in the
  // second set of bindings conflicts with anything in the first, and
  // that none of the bindings in the second set conflict with each
  // other
  //def add_binds(binds: Bindings, newbinds: Bindings): Option[Bindings] = {
  //  (newbinds.toList).foldLeft[Option[Bindings]](Some(binds))(add_bind)
  // }

  // see comments on get_mandatory_bindings in ACMatcher. The
  // SequenceMatcher version of get_mandatory_bindings is not
  // complete! It should have another case in the match statement that
  // handles Apply data structures in the plist.head match {...}, and
  // it should be invoked from SequenceMatcher.tryMatch for
  // completeness.
  /**
   * Gets matches that must succeed in any potential match or return None
   * if no match is possible.
   * 
   * @param ps      The pattern sequence.
   * @param ss      The subject sequence.
   * @param ibinds  Already discovered bindings that must hold in and potential
   *                 match.
   * @return        Bindings required for any potential match, or None if no
   *                 match is possible.
   */
  def get_mandatory_bindings(plist: AtomSeq, slist: AtomSeq,
    ibinds: Bindings): Option[Bindings] = {
    Debugger("matching", "called SequenceMatcher.get_mandatory_bindings")
    Debugger("matching", "    Patterns: " + plist.mkParseString("", ",", ""))
    Debugger("matching", "    Subjects: " + slist.mkParseString("", ",", ""))
    Debugger("matching", "    Bindings: " + ibinds.toParseString)

    var binds = ibinds

    if (plist.isEmpty && slist.isEmpty) {
      Debugger("matching", "empty lists")
      return Some(binds)
    } else if (plist.length != slist.length) {
      Debugger("matching", "Lists not the same length, no match possible.")
      return None
    } else {
      Debugger("matching", "looking at head")
      plist.head match {
        case p: Variable =>
          Debugger("matching", "found a variable")
          Debugger("matching", p.name + " -> " + slist.head.toParseString)
          /*if(typ == ANY) return get_mandatory_bindings(
                AtomSeq(plist.props, plist.tail),
                AtomSeq(slist.props, slist.tail), binds)*/
          Debugger("matching", "Calling add_bind( " + binds + ", (" + p.name + ", " + slist.head.toParseString + "))")
          add_bind(Some(binds), (p, slist.head)) match {
            case None =>
              Debugger("matching", "add_bind() failed -- conflicting binds")
              return None
            case Some(b) =>
              Debugger("matching", "add_bind() got: " + b)
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

              if (!prpp.isA(false) && !prpp.isC(false)) {
                Debugger("matching", "SeqenceMatcher found non-AC operator " + ns)
                get_mandatory_bindings(AtomSeq(prpp, argp), AtomSeq(prps, argss), binds) match {
                  case None => return None
                  case Some(b) => binds = b
                }
              } else {
                Debugger("matching", "SeqenceMatcher found AC operator " + ns)
                //construct bindings with with this operator peeled off bound terms
                var newbinds:Bindings = Bindings()
                var opwrap: OperatorRef = null
                val pb = MatchHelper.peelBindings(binds, ns)
                newbinds = pb._1
                opwrap = pb._2
                Debugger("matching", "After peeling " + newbinds.toParseString)
                ACMatcher.get_mandatory_bindings(AtomSeq(prpp, argp), AtomSeq(prps, argss), newbinds) match {
                  case None => return None
                  case Some(b) => binds = b
                }
                //rewrap the naked AC terms
                newbinds = MatchHelper.wrapBindings(binds, opwrap)
                binds=newbinds
              }

            //Not certain this is correct. 
            case _ =>
              return None

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
    var _patterns = patterns
    var _subjects = subjects
    
        
    // Has rewriting timed out?
    if (BasicAtom.rewriteTimedOut) {
      Fail("Timed out")
    } else if (patterns.length != subjects.length) {
      Fail("Sequences are not the same length.")
    } else {
      
      val reconsideredvars = reorder_matchcost(_patterns, _subjects)
      _patterns = reconsideredvars._1
      _subjects = reconsideredvars._2

      
      //Properties should have been taken into account at creation time, so we ignore them here.
      val plist = AtomSeq(NoProps, _patterns)
      val slist = AtomSeq(NoProps, _subjects)
      var mbinds = binds
      get_mandatory_bindings(plist, slist, mbinds) match {
        case None => return Fail((() => "SequenceMatcher mandatory-bindings induced fail"), 0)
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
      _tryMatch(_patterns, _subjects, mbinds, 0)

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
