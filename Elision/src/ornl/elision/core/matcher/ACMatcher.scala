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

import scala.annotation.tailrec

import ornl.elision.core.knownExecutor
import ornl.elision.core.Apply
import ornl.elision.core.AlgProp
import ornl.elision.core.AtomSeq
import ornl.elision.core.BasicAtom
import ornl.elision.core.Bindings
import ornl.elision.core.Fail
import ornl.elision.core.Many
import ornl.elision.core.Match
import ornl.elision.core.MatchIterator
import ornl.elision.core.Outcome
import ornl.elision.util.Debugger

import ornl.elision.core.OperatorRef
import ornl.elision.core.Operator
import ornl.elision.core.Variable
import ornl.elision.core.Literal
import ornl.elision.util.OmitSeq
import ornl.elision.util.Loc

abstract class res
case class uninitialized() extends res
case class found(z: AtomSeq) extends res
case class toomany() extends res

/**
 * Match two sequences whose elements can be re-ordered or re-grouped.  That is,
 * the lists are associative and commutative.
 */
object ACMatcher {

  // The objective of get_mandatory_bindings (and the
  // mutually-recursive version in SequenceMatcher) is to do a quick
  // traversal of our term, identifying bindings that are mandatory
  // under all possible matches. These bindings are used to reduce the
  // search space when we do backtracking and explore other possible
  // matches for non-mandatory items in the expression. Backtracking
  // and searching will be necessary to explore the various bipartite
  // graphs arising from various AC terms in the expression being
  // matched. This gets run before the rest of the match, each time
  // tryMatch gets called.

  // I believe that get_mandatory_bindings serves a similar function
  // to build_hierarchy and simplify in Eker's
  // "Associative-Commutative Matching Via Bipartite Graph Matching"
  // paper.

  // Assume plist and slist are both from an AC context.
  // get_mandatory_bindings gets called multiple times at different
  // levels, duplicating parts of its search, possibly many times for
  // large predicates. I think there are cases where calling this at a
  // deeper level of nesting allows the search to get access to more
  // simple bindings, but more thought needs to be given to this.
  def get_mandatory_bindings(ps: AtomSeq, ss: AtomSeq,
    ibinds: Bindings): Option[Bindings] = {
    Debugger("ACmatching", "called ACMatcher.get_mandatory_bindings")
    Debugger("ACmatching", ps.toParseString)
    var binds: Bindings = ibinds
    var (plistv, slist, fail) = MatchHelper.eliminateConstants(ps, ss)
    if (fail.isDefined) {
      Debugger("ACmatching", "failed match")
      return None
    }

    // TODO: error handling for fail?
    var (plist, vlist) = MatchHelper.stripVariables(plistv)

    Debugger("ACmatching", "plist.length " + plist.length)
    var _pindex = 0
    while (_pindex < plist.length) {
      var p = plist(_pindex)

      Debugger("ACmatching", "examining " + p.toParseString)
      p match {
        // presumably p==p2; do we have any guarantee?
        case Apply(OperatorRef(Operator(np, tp, AtomSeq(oprpp, oargp))),
          AtomSeq(prpp, argp)) =>

          var s: res = uninitialized()

          var _sindex = 0
          var _somitme = 0
          while (_sindex < slist.length) {
            var si = slist(_sindex)
            Debugger("ACmatching", "examining subject " + si.toParseString)

            si match {
              case Apply(OperatorRef(Operator(ns, ts, AtomSeq(oprps, oargs))),
                AtomSeq(prps, args)) =>
                if (np == ns) {
                  Debugger("ACmatching", "found identically named operators")
                  s = s match {
                    case uninitialized() =>
                      _somitme = _sindex
                      found(AtomSeq(prps, args))
                    case found(_) => toomany()
                    case toomany() => toomany()
                  }
                } else {
                  Debugger("ACmatching", "found non-identically named operators")
                }
              case _ =>
                Debugger("ACmatching", "si match failed, continuing to next subject.")
            }
            _sindex = _sindex + 1
          }

          s match {
            case uninitialized() =>
              //Does this indicate a failed match or just a failure on this subject?
              Debugger("ACmatching", "no possible match")
            case found(a) =>
              Debugger("ACmatching", "found mandatory terms, descending")
              Debugger("ACmatching", AtomSeq(prpp, argp).toParseString)
              Debugger("ACmatching", a.toParseString)
              slist = slist.omit(_somitme)
              if (prpp.isA(false) && prpp.isC(false)) {
                Debugger("ACmatching", "Calling ACMatcher.get_mandatory_bindings from ACMatcher")
                ACMatcher.get_mandatory_bindings(AtomSeq(prpp, argp),
                  a, binds) match {
                    case None => return None
                    case Some(b) => binds = b
                  }
              } else {
                Debugger("ACmatching", "Calling SequenceMatcher.get_mandatory_bindings from ACMatcher")
                SequenceMatcher.get_mandatory_bindings(AtomSeq(prpp, argp),
                  a, binds) match {
                    case None => return None
                    case Some(b) => binds = b
                  }
              }

            case toomany() => binds = binds
          }
        case _ => binds = binds // do nothing
      }
      plist = plist.omit(_pindex) // _pindex = _pindex + 1
    }

    if ((vlist.length == 1) && (slist.length == 1)) {
      vlist(0) match {
        case Variable(typ, nam, gua, lab, byn) =>
          SequenceMatcher.add_bind(Some(binds), (nam, slist(0))) match {
            case Some(b) =>
              Debugger("ACmatching", "clever binding")
              binds = b
            case None =>
              Debugger("ACmatching", "unclever binding")
              Debugger("ACmatching", binds.toParseString)
              Debugger("ACmatching", nam + " " + slist(0).toParseString)
              return None
          }
      }
    }
    Debugger("ACmatching", "returning to prev level" + binds.toParseString)
    Some(binds)
  }

  /**
   * Attempt to match two lists.  The second list can be re-ordered and
   * re-grouped arbitrarily.
   *
   * @param plist	The pattern list.
   * @param slist	The subject list.
   * @param ibinds 	Bindings that must be honored in any match.
   * @param op		An optional operator to apply to sublists.
   * @return	The match outcome.
   */
  def tryMatch(plist: AtomSeq, slist: AtomSeq, ibinds: Bindings,
    op: Option[OperatorRef]): Outcome = {
    if (BasicAtom.rewriteTimedOut) {
      return Fail("Timed out", plist, slist)
    }
    var binds = ibinds
    get_mandatory_bindings(plist, slist, binds) match {
      case None => return Fail((() => "Mandatory-bindings induced fail"), 0)
      case Some(b) =>
        Debugger("ACmatching", "binding results: ")
        Debugger("ACmatching", b.toParseString)
        binds = b
    }

    Debugger("ACmatching", "->trymatch")
    Debugger("ACmatching", "plist:")
    Debugger("ACmatching", plist.mkParseString("", ",", ""))
    Debugger("ACmatching", "slist")
    Debugger("ACmatching", slist.mkParseString("", ",", ""))
    Debugger("ACmatching", "ibinds")
    Debugger("ACmatching", ibinds.toParseString)

    // Check the length.
    if (plist.length > slist.length)
      return Fail("More patterns than subjects, so no match is possible.",
        plist, slist)
    //    Debugger("ACmatching","(1)")

    // If there are patterns, but not subjects, no match is possible.  If
    // there are subjects, but not patterns, no match is possible.
    if (plist.length == 0 && slist.length > 0)
      return Fail("No patterns to bind to the subjects.", plist, slist)
    if (slist.length == 0 && plist.length > 0)
      return Fail("No subjects to be bound to patterns.", plist, slist)
    // If there are no patterns (and no subjects), there is nothing to do.
    if (plist.length == 0) return Match(binds)

    // If there are the same number, then this is a simple case of commutative
    // matching.
    if (plist.length == slist.length) {
      return CMatcher.tryMatch(plist, slist, binds)
    }
    //    Debugger("ACmatching","(2)")

    // If there is exactly one pattern then match it immediately.
    if (plist.length == 1) {
      // If there is an operator, apply it to the subjects, then try to match
      // the single pattern against the result.
      return plist.atoms(0).tryMatch(op match {
        case Some(opref) =>
          Apply(opref, slist)
        case None =>
          slist
      }, binds)
    }
    //    Debugger("ACmatching","(3)")

    // Conduct a test to see if matching is even possible.  Try to match
    // every pattern against some subject.  Note that this does not work
    // in general, since a pattern might match a grouping of subjects.
    // We should not have to worry about that case here, since such patterns
    // would have been "flattened" earlier by associativity.  That is, the
    // operator would have arisen because it is the "outer" operator and
    // thus the list would be flattened.  This is hard to explain, but an
    // example might help.
    //
    // Suppose we end up with the operator being foo, and the following
    // patterns and subjects.
    //
    // patterns: foo($a,$b), $c, bar($a)
    // subjects: $x, $y, $z, bar($x)
    //
    // We could group $x and $y to foo($x,$y) and then match the (unbindable)
    // pattern foo($a,$b).  However the operator foo must have arisen as the
    // original outer operator for the patterns; that is the original pattern
    // was foo(foo($a,$b),$c,bar($a)), and since foo is associative (or we
    // would not be here) lists get flattened, giving new pattern
    // foo($a,$b,$c,bar($a)).  So we wind up with the following patterns and
    // subjects.
    //
    // patterns: $a, $b, $c, bar($a)
    // subjects: $x, $y, $z, bar($x)
    //
    // We match just as we should.

    // First see if there is at least 1 subject child that matches
    // each item in the pattern.
    // var _pindex = 0
    // while (_pindex < plist.length) {
    //   var _sindex = 0
    //   var _matched = false
    //   while ((!_matched) && (_sindex < slist.length)) {
    //     plist(_pindex).tryMatch(slist(_sindex), binds) match {
    //       case fail:Fail =>
    //       case m1:Match => _matched = true
    //       case m2:Many => _matched = true
    //     }
    //     _sindex += 1
    //   } // Search for a matching subject.
    //   if (!_matched) return Fail("Matching precheck failed.", plist, slist)
    //   _pindex += 1
    // } // Test for a match for each pattern.

    // Step one is to perform constant elimination.  Any constants must match
    // exactly, and we match and remove them.

    var (patterns, subjects, fail) = MatchHelper.eliminateConstants(plist, slist)
    if (fail.isDefined) return fail.get

    // Step two is to match and eliminate any unbindable atoms.  These are
    // atoms that are not variables, and so their matching is much more
    // restrictive.  We obtain an iterator over these, and then combine it
    // with the iterator for "everything else."
    var um = new UnbindableMatcher(patterns, subjects, binds)
    //    Debugger("ACmatching","(4)")

    // This is not so simple.  We need to perform the match.  Build the
    // iterator.
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
            //	    Debugger("ACmatching","empty findNext")
            _exhausted = true
          }
        }
      } else {
        // Get the patterns and subjects that remain.
        val pats = AtomSeq(plist.props, bindings.patterns.getOrElse(patterns))
        val subs = AtomSeq(slist.props, bindings.subjects.getOrElse(subjects))

        Debugger("ACmatching", "->anon iter")
        Debugger("ACmatching", pats.mkParseString("", ",", ""))
        Debugger("ACmatching", subs.mkParseString("", ",", ""))

        // Are we trying to aggresively fail ACMatching at the risk of not matching something
        // that could match?
        if (knownExecutor.getProperty("rewrite_aggressive_fail")) {
          // If there is exactly one pattern then match it immediately. Note that there could
          // be other matches left in the unbindable matcher, which we are now skipping.
          if (pats.atoms.length == 1) {
            return pats.atoms(0).tryMatch(op match {
              case Some(opref) =>
                Apply(opref, subs)
              case None =>
                subs
            }, (bindings ++ binds))
          }
        }

        // If there are no patterns, and all subjects have been
        // matched, there is nothing to do.  If there are no patterns,
        // and all subjects have NOT been matched, there is no match.
        if ((pats.atoms.length == 0) && (subs.atoms.length == 0)) {
          MatchIterator(bindings ++ binds)
        } else if ((pats.atoms.length == 0) && (subs.atoms.length != 0)) {
          // Return an empty iterator.
          new MatchIterator {
            _current = null
            _local = null
            _exhausted = true
            def findNext = {
              Debugger("ACmatching", "another empty findNext")
              _exhausted = true
            }
          }
        } else {
          // Merge the current bindings with the old bindings.
          val newBinds = (bindings ++ binds)

          // We are currently walking through the bindings generated by
          // the unbindable match iterator. The unbindable match
          // iterator matches ALL the unbindable things in the pattern,
          // so all of the items in the current pattern should be
          // bindable items, i.e., variables. In addition, some of these
          // pattern variables may already be bound to subject items in
          // the current binding.
          //
          // In an attempt to fail fast, we are going to check each
          // pattern variable left and see if:
          // 1. Is the left-over pattern variable currently bound in the
          //    binding?
          // 2. If so, is there an atom in the subject list is exactly
          //    equal to the atom to which the pattern variable is
          //    bound? If not, we can fail immediately.
          var failFast = false
          var newPats = scala.collection.immutable.Vector.empty[BasicAtom]
          var newSubs = subs
          //var discardSubs = scala.collection.immutable.Vector.empty[BasicAtom]
          for (patItem <- pats) {
            // Is the current pattern variable currently bound to
            // something?
            patItem match {
              case patVar: Variable => {
                // The pattern item is a variable. This is what we
                // expect.
                newBinds.get(patVar.name) match {
                  case None => {
                    // Nothing is bound to this pattern variable, so we have
                    // nothing to check. Since nothing is bound to this
                    // pattern variable it must remain in the pattern
                    // list.
                    newPats = newPats :+ patItem
                  }

                  case Some(atom) => {
                    // The pattern variable is already bound to
                    // something. That something MUST appear in the subject
                    // list.
                    var atom_subs = atom match {
                      case Apply(opapllied, arguments) => {
                        Debugger("ACmatching", "Doing something with an operator")
                        Debugger("ACmatching", opapllied.toParseString + " ")
                        Debugger("ACmatching", arguments.toParseString)
                        if (opapllied == op.getOrElse(None)) AtomSeq(slist.props, arguments)
                        else AtomSeq(slist.props, atom)
                      }
                      case _ => AtomSeq(slist.props, atom)
                    }
                    var gotIt = false
                    for (subVal <- subs) {
                      // Have we found the bound value?

                      if (atom_subs.contains(subVal)) {
                        gotIt = true
                        // We have now found a match in the subjects for
                        // the prior match of this pattern variable. We
                        // do not need to try to match this pattern
                        // variable any more. The matched subject item
                        // is also now out of play. Therefore we will
                        // NOT add the pattern or subject to the new
                        // pattern/subject list.
                        //
                        // Note that the pattern variable is already
                        // bound to this subject value, so the bindings
                        // do not need to be updated.
                        //discardSubs = discardSubs :+ subVal
                        newSubs = AtomSeq(slist.props, newSubs.diff(Seq(subVal)))
                        atom_subs = AtomSeq(slist.props, atom_subs.diff(Seq(subVal)))

                      }

                    }

                    // Did we find something in the subject list equal to
                    // the already bound pattern variable?
                    if (!gotIt) {
                      // No, we did not. There is no way this can match.
                      failFast = true
                    }
                  }
                }
              }

              case _ => {
                // This is unexpected. We expect all the remaining
                // things in the pattern to be variables.

                // Since nothing is bound to this pattern variable it
                // must remain in the pattern list.
                newPats = newPats :+ patItem
              }
            }
          } // Loop over patterns.

          // If we get here all of the previously bound pattern
          // variables that still appear in the pattern have at least 1
          // thing they match in the subject. Do the actual matching.
          if (!failFast) {
            // We might have already discarded some patterns/subjects
            // based on the bindings from the unbindable matcher. Make
            // new pattern/subject sequences here.

            val pats1 = AtomSeq(plist.props, newPats)
            val subs1 = newSubs
            new ACMatchIterator(pats1, subs1, newBinds, op)
          } else {
            // This set of bindings can never match. Return an empty iterator.
            new MatchIterator {
              _current = null
              _local = null
              _exhausted = true
              def findNext = {
                Debugger("ACmatching", "another empty findNext (2)")
                _exhausted = true
              }
            }
          }
        }
      }
    }) // Building the iterator iter.

    //    Debugger("ACmatching","(5)")
    if (iter.hasNext) return Many(iter)
    else Fail("The lists do not match.", plist, slist)
  }

  /* How associative and commutative matching works.
   * 
   * The subject list must be at least as long as the pattern list, or no
   * match is possible.
   * 
   * We first permute the subjects, and then iterate over all groupings of
   * the subjects.
   */

  private class ACMatchIterator(patterns: AtomSeq, subjects: AtomSeq,
    binds: Bindings, op: Option[OperatorRef]) extends MatchIterator {
    /** An iterator over all permutations of the subjects. */
    private val _perms = subjects.atoms.permutations

    /**
     * Find the next match.  At the end of running this method either we
     * have `_current` set to the next match or we have exhausted the
     * iterator.
     */
    import scala.annotation.tailrec
    @tailrec
    final protected def findNext {
      //      Debugger("ACmatching",">")
      Debugger("ACmatching", "AC Searching... ")

      // Has rewriting timed out?
      if (BasicAtom.rewriteTimedOut) {
        _exhausted = true
        return
      }

      _current = null
      if (_local != null && _local.hasNext) _current = _local.next
      else {
        _local = null
        Debugger("ACmatching", "Starting AMatcher from AC matcher")
        if (_perms.hasNext) {
          // We're about to run the associative matcher. In order to do that we
          // need an atom sequence. However, both the patterns and subjects are
          // associative and commutative. If we give commutativity to AtomSeq
          // it will always sort the atoms at creation time. We don't want that.
          // So, we create a new AlgProp that's just associative to facilitate 
          // matching as we go through the permutations.
          AMatcher.tryMatch(patterns,
            AtomSeq(AlgProp(Loc.internal,
              Option(subjects.props.isA(true)),
              Option(Literal.FALSE),
              Option(subjects.props.isI(false)),
              Option(subjects.props.getB(Literal.FALSE)),
              Option(subjects.props.getD(Literal.FALSE))),
              _perms.next),
            binds, op) match {
              case fail: Fail => {
                // We ignore this case.  We only fail if we exhaust all attempts.
                Debugger("ACmatching", fail.toString)
                findNext
              }
              case Match(binds) => {
                // This case we care about.  Save the bindings as the current match.
                Debugger("ACmatching", "AC Found.")
                _current = binds
              }
              case Many(iter) => {
                // We've potentially found many matches.  We save this as a local
                // iterator and then use it in the future.
                Debugger("ACmatching", "Iterating...")
                _local = iter
                findNext
              }
            }
        } else {
          // We have exhausted the permutations.  We have exhausted this
          // iterator.
          Debugger("ACmatching", "AC Exhausted.")
          _exhausted = true
        }
      }
    }
  }
}
