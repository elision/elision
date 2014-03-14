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

import ornl.elision.core.AtomSeq
import ornl.elision.core.BasicAtom
import ornl.elision.core.Fail
import ornl.elision.core.giveMkParseString
import ornl.elision.util.Debugger
import ornl.elision.util.OmitSeq
import ornl.elision.util.OmitSeq.fromIndexedSeq

import ornl.elision.core.Variable

/**
 * Provide some support methods for matching.
 */
object MatchHelper {

  /**
   * Given two lists of atoms, identify and remove any constants from the two
   * lists, returning the resulting lists.
   *
   * The atoms in the two lists are assumed to be unordered.  That is, this
   * method is only suitable for use when performing commutative matching
   * (whether or not associative).
   *
   * @param plist	The pattern list.
   * @param slist	The subject list.
   * @return  A triple that contains the new patterns, new subjects, and an
   *          optional failure instance in the event matching does not succeed.
   */
  def eliminateConstants(plist: AtomSeq, slist: AtomSeq): (OmitSeq[BasicAtom], OmitSeq[BasicAtom], Option[Fail]) = {
    var patterns = plist.atoms
    var subjects = slist.atoms
    for ((pat, pindex) <- plist.constantMap) {
      slist.constantMap.get(pat) match {
        case None =>
          return (patterns, subjects, Some(Fail("Element " + pindex +
            " not found in subject list.", plist, slist)))
        case Some(sindex) =>
          patterns = patterns.omit(pindex)
          subjects = subjects.omit(sindex)
      }
    } // Omit constants from the lists.
    Debugger("matching") {
      Debugger("matching", "Removing Constants: Patterns: " +
        patterns.mkParseString("", ",", ""))
      Debugger("matching", "                    Subjects: " +
        subjects.mkParseString("", ",", ""))
    }
    (patterns, subjects, None)
  }

  def stripVariables(plist: OmitSeq[BasicAtom]): (OmitSeq[BasicAtom], OmitSeq[BasicAtom]) = {
    var (pl, vl) = (plist, plist)
    if (plist.length <= 0) {
      return (pl, vl)
    }

    var _pindex = plist.length - 1
    while (_pindex >= 0) {
      plist(_pindex) match {
        case _: Variable => pl = pl.omit(_pindex)
        case _ => vl = vl.omit(_pindex)
      }
      _pindex = _pindex - 1
    }

    (pl, vl)
  }
}
