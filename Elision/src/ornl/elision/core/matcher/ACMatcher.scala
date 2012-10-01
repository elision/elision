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
import ornl.elision.core._

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
   * @param op		An optional operator to apply to sublists.
   * @return	The match outcome.
   */
  def tryMatch(plist: AtomSeq, slist: AtomSeq, binds: Bindings,
               op: Option[OperatorRef]): Outcome = {

    // Check the length.
    //println("** ACMatcher...")
    //println("** original bindings = " + binds)
    if (plist.length > slist.length)
      return Fail("More patterns than subjects, so no match is possible.",
          plist, slist)

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
    
    // @@@@ BEGIN KIRK CHANGES
    // If the subject we are trying to match is big enough, do some
    // initial checks to see if matching is even possible.
    println("Kirk precheck...")
    //if (slist.length > 5) {
      
    // First see if there is at least 1 subject child that matches
    // each item in the pattern.
    for (pat <- plist) {
      
      // Check each subject child against the current pattern child,
      // looking for a match.
      var gotMatch = false
      for (sub <- slist) {
        if (!gotMatch) {
          pat.tryMatch(sub, binds) match {
            case fail:Fail => {
              gotMatch = false
            }
	    case Match(binds1) => {
              gotMatch = true
            }
	    case Many(iter) => {
              gotMatch = true
            }
          }
        }
      }
      
      // Did we find a match for the current pattern child?
      if (!gotMatch) {
        
        // If there is no possible match for the current pattern
        // child, there is no way this overall match will work.
        println("FAST FAIL...")
        return Fail("Matching is impossible. Matching precheck failed.", plist, slist)
      }
    }
    
    // Are we going to need to do some associative grouping to get
    // things to match?
    if (slist.length > plist.length) {
      
      // We have more targets to match than we have in the
      // pattern. This means that the excess (at a minimum) targets
      // will all be grouped together via associativity as an
      // instance of an operator the same type as the target. See if
      // there is a potential match for an operator of the same type
      // as the target.
    }
    //}
    // @@@@ END KIRK CHANGES

    // Step one is to perform constant elimination.  Any constants must match
    // exactly, and we match and remove them.
    var (patterns, subjects, fail) = MatchHelper.eliminateConstants(plist, slist)
    if (fail.isDefined) return fail.get
    
    // Step two is to match and eliminate any unbindable atoms.  These are
    // atoms that are not variables, and so their matching is much more
    // restrictive.  We obtain an iterator over these, and then combine it
    // with the iterator for "everything else."
    var um = new UnbindableMatcher(patterns, subjects, binds)
    val foundUMMatches = um.hasNext
    println("** Elision: UnbindableMatcher for P:" + patterns + " and S:" +
            subjects + " hasNext() == " + foundUMMatches);
    
    // Does the pattern actually contain any unbindable items?
    if (patterns.indexWhere(!_.isBindable) >= 0) {

      // The pattern contains at least 1 unbindable item. Were we able
      // to match all the unbindable items?
      if (!foundUMMatches) {

        // We were not able to match up all the unbindable items in
        // the pattern. This pattern will never match and anything
        // more we do is wasted work.
        println("FAST FAIL, UnbindableMatch Failed...")
        return Fail("Matching is impossible. Cannot match unbindables in pattern..", plist, slist)
      }
    }
    
    // Regenerate the unbindable matcher in case calling hasNext()
    // messed it up.
    um = new UnbindableMatcher(patterns, subjects, binds)

    // This is not so simple.  We need to perform the match.
    val iter = um ~ (bindings => {

      // Get the patterns and subjects that remain.
      val pats = AtomSeq(plist.props, bindings.patterns.getOrElse(patterns))
      val subs = AtomSeq(slist.props, bindings.subjects.getOrElse(subjects))
      
      // If there is exactly one pattern then match it immediately.
      if (pats.atoms.length == 1) {
        println("** bindings = " + bindings)
        //println("** binds = " + binds)
        //println("** patterns = " + patterns)
        println("** pats = " + pats)
        //println("** subjects = " + subjects)
        println("** subs = " + subs)
        return pats.atoms(0).tryMatch(op match {
          case Some(opref) =>
            Apply(opref, subs)
          case None =>
            subs
        }, (bindings ++ binds))
        //return pats.atoms(0).tryMatch(subs,binds)
      }
      
      // If there are no patterns, there is nothing to do.
      if (pats.atoms.length == 0) {
        //println("** new bindings (1) = " + bindings)
        MatchIterator(bindings ++ binds)
      }
      else {

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
        // In an attemp to fail fast, we are going to check each
        // pattern vartiable left and see if:
        // 1. Is the left-over pattern variable currently bound in the
        //    binding?
        // 2. If so, is there an atom in the subject list is exactly
        //    equal to the atom to which the pattern variable is
        //    bound? If not, we can fail immediately.
        var failFast = false
        for (patItem <- pats) {
          
          // Sanity check.
          if (!patItem.isBindable) {
            println("BOGUS!!: '" + patItem.toString + "' is not bindable!!!")
          }

          // Is the current pattern variable currently bound to
          // something?
          patItem match {
            case patVar : Variable => {
              
              // The pattern item is a variable. This is what we
              // expect.
              newBinds.get(patVar.name) match {
                
                case None => {
                  
                  // Nothing is bound to this pattern variable, so we have
                  // nothing to check.
                }
                
                case Some(atom) => {
                  
                  // The pattern variable is already bound to
                  // something. That something MUST appear in the subject
                  // list.
                  println("** Elision: Pattern variable '" + patVar.name +
                          "' already bound to '" + atom.toParseString + "'")
                  var gotIt = false
                  for (subVal <- subs) {
                    
                    // Have we found the bound value?
                    if (!gotIt && (subVal == atom)) {
                      println("** Elision: Found subject match.")
                      gotIt = true
                    }
                  }
                  
                  // Did we find something in the subject list equal to
                  // the already bound pattern variable?
                  if (!gotIt) {
                    
                    // No, we did not. There is no way this can match.
                    println("FAST FAIL, Match already bound variables failed...")
                    failFast = true
                  }
                }
              }
            }
            
            case _ => {
              
              // This is unexpected. We expect all the remaining
              // things in the pattern to be variables.
              println("BOGUS!!: '" + patItem.toString + "' is not a variable!!!")
            }
          }
        }
        
        // If we get here all of the previously bound pattern
        // variables that still appear in the pattern have at least 1
        // thing they match in the subject. Do the actual matching.
        
        //println("** new bindings (2) = " + bindings)
        if (!failFast) {
          new ACMatchIterator(pats, subs, newBinds, op)
        }
        else {

          // This set of bindings can never match.
          new MatchIterator {
            _current = null
            _local = null
            _exhausted = true
            def findNext = {
              _exhausted = true
            }
          }
        }
      }
    })
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
      if (BasicAtom.traceMatching) print("AC Searching... ")
      _current = null
      if (_local != null && _local.hasNext) _current = _local.next
      else {
        _local = null
	if (_perms.hasNext)
	  AMatcher.tryMatch(patterns, AtomSeq(subjects.props, _perms.next),
	                    binds, op) match {
	    case fail:Fail =>
	      // We ignore this case.  We only fail if we exhaust all attempts.
              if (BasicAtom.traceMatching) println(fail)
	    findNext
	    case Match(binds) =>
	      // This case we care about.  Save the bindings as the current match.
	      _current = binds
	    if (BasicAtom.traceMatching) println("AC Found.")
	    case Many(iter) =>
	      // We've potentially found many matches.  We save this as a local
	      // iterator and then use it in the future.
	      _local = iter
	    findNext
	  } else {
	    // We have exhausted the permutations.  We have exhausted this
	    // iterator.
	    _exhausted = true
	    if (BasicAtom.traceMatching) println("AC Exhausted.")
	  }
      }
    }
  }
}
