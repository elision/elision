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
import scala.language.reflectiveCalls
import ornl.elision.core.Variable
import ornl.elision.core.Bindings
import ornl.elision.core.OperatorRef
import ornl.elision.core.Apply

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
  def eliminateConstants(plist: AtomSeq, slist: AtomSeq, binds: Bindings): (OmitSeq[BasicAtom], OmitSeq[BasicAtom], Option[Fail]) = {
    var patterns = plist.atoms
    var subjects = slist.atoms
    val bindings = binds
    var pat: BasicAtom = null
    var pindex = -1

    Debugger("matching") {
      Debugger("matching", "Removing Constants: Patterns: " +
        patterns.mkParseString("", ",", ""))
      Debugger("matching", "                    Subjects: " +
        subjects.mkParseString("", ",", ""))
    }
    
    //store a list of omissions to be made
    var omissions = List[Int]()
    plist.constantMap.foreach(((thing): (BasicAtom, Int)) => {
      pat = thing._1
      //pindex = thing._2
      omissions = thing._2 +: omissions 
    })
    
    // We want to run across the list of indexes highest-to-lowest so that when
    // we omit we don't wind up with wrong indexes
    omissions.sorted.reverse.foreach((pindex) =>
    slist.constantMap.get(patterns(pindex)) match {
        case None =>
          return (patterns, subjects, Some(Fail("Element " + pindex +
            " not found in subject list.", plist, slist)))
        case Some(sindex) =>
          Debugger("constant-elimination", "pindex:  " + pindex)
          Debugger("constant-elimination", "Eliminating pattern item: " + patterns(pindex))
          Debugger("constant-elimination", "sindex:  " + sindex)
          Debugger("constant-elimination", "Eliminating subject item:  " + subjects(sindex))
          patterns = patterns.omit(pindex)
          subjects = subjects.omit(sindex)
      })

    val ret = (patterns, subjects, None)
      
    Debugger("matching") {
      Debugger("matching", "After Removing Constants: Patterns: " +
        ret._1.mkParseString("", ",", ""))
      Debugger("matching", "                    Subjects: " +
        ret._2.mkParseString("", ",", ""))
    }
    ret
  }
  

  def eliminateBoundVariables(plist: AtomSeq, slist: AtomSeq, binds: Bindings):
                              (OmitSeq[BasicAtom], OmitSeq[BasicAtom], Option[Fail]) = {
    var patterns = plist.atoms
    var subjects = slist.atoms
    var pomissions = List[Int]()
    var somissions = List[BasicAtom]()
    /*plist.variableMap.foreach(((thing): (String, Int)) => {
      val pat = thing._1
      //pindex = thing._2
      if (binds.contains(pat)) vomissions = thing._2 +: vomissions
    })*/

    
    //Mark patterns for removal
    binds.foreach(thing => {
      //If this variable exists in the map, get its ID. Otherwise, set -1
      //to indicate nonexistance.
      val pomission = plist.variableMap.getOrElse(thing._1, -1)
      //Add the pattern index to pomissions for later removal. Add the atom to
      //the subject omissions to be searched for and removed.
      if (pomission >= 0 && thing._2.isConstant) pomissions = pomission +: pomissions; somissions = thing._2 +: somissions 
    })
    //If there are no omissions to take care of, go ahead and return
    if (pomissions.length == 0) return (patterns, subjects, None)

    // For each omission, 
    somissions.foreach(somission =>
      {
        somission match {
          case as: AtomSeq => as.atoms.foreach(a => { 
                                val sindex = subjects.indexOf(a) 
                                if(sindex >= 0){
                                  Debugger("constant-elimination", "sindex:  " + sindex)
                                  Debugger("constant-elimination", "Eliminating subject item:  " + subjects(sindex).toParseString)
                                  subjects = subjects.omit(sindex)}
                                else{ 
                                  Debugger("constant-elimination", "sindex:  " + sindex)
                                  Debugger("constant-elimination", "Unable to eliminate subject item:  " + a.toParseString)
                                  return (patterns, subjects, Some(Fail("Unable to eliminate item from subject.")))
                                }
                              }
            )
          case _ => {
            val sindex = subjects.indexOf(somission)
            if (sindex >= 0){
              Debugger("constant-elimination", "sindex:  " + sindex)
              Debugger("constant-elimination", "Eliminating subject item:  " + subjects(sindex).toParseString)
              subjects = subjects.omit(sindex)
            }
            else {
              Debugger("constant-elimination", "sindex:  " + sindex)
              Debugger("constant-elimination", "Unable to eliminate subject item:  " + somission.toParseString)
              return (patterns, subjects, Some(Fail("Unable to eliminate item from subject.")))
            }
          }
        }
      })

    pomissions.sorted.reverse.foreach(pindex => {
      Debugger("constant-elimination", "pindex:  " + pindex)
      Debugger("constant-elimination", "Eliminating pattern item: " + patterns(pindex).toParseString)
      patterns = patterns.omit(pindex)
    })
    (patterns, subjects, None)
  }

    /**
   *  Given a pattern, split it into non-variable and variable lists.
   *  
   *  @param plist The pattern to split
   */
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

  def peelBindings(binds: Bindings, name: String) = {
    var newbinds: Bindings = Bindings()
    var opwrap:OperatorRef = null
    binds.foreach(item => {
      Debugger("matching", "Want to peel " + item._1 + " -> " + item._2.toParseString)
      item._2 match {
        case Apply(opref: OperatorRef, seq) if (opref.name == name) =>
          newbinds = newbinds + (item._1 -> seq)
          opwrap = opref
        case _ => newbinds = newbinds + (item)
      }
    })
    (newbinds, opwrap)
  }

  def wrapBindings(binds: Bindings, opref: OperatorRef) = {
    var newbinds = Bindings()
    binds.foreach(item => {
      Debugger("matching", "Want to wrap " + item._1 + " -> " + item._2.toParseString)
      item._2 match {
        case as: AtomSeq if (as.props.isA(false) && as.props.isC(false)) => newbinds = newbinds + (item._1 -> Apply(opref, as))
        case _ => newbinds = newbinds + (item)
      }
    })
    newbinds
  }
    
  
}
