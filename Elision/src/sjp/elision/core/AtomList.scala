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

import scala.collection.immutable.HashMap
import java.util.LinkedList

/**
 * Encapsulate an ordered list of atoms.
 * @param atoms		The list of atoms.  Note that order may be important.
 */
case class AtomList(atoms: Seq[BasicAtom]) extends BasicAtom {
  require(atoms != null)
  
  // The type of all lists is the type universe.  This may be changed later.
  val theType = TypeUniverse
  
  // The De Brujin index is equal to the maximum index of the atoms in the
  // sequence.  Compute that now.
  val deBrujinIndex = atoms.map(_.deBrujinIndex).max

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    // Ordered lists only match other ordered lists with matching elements in
    // the same order.
    subject match {
    	case AtomList(oatoms) =>
    	  // The lists must be the same length, or they cannot match.
    	  if (atoms.length != oatoms.length)
    	    Fail("Lists are different sizes.", this, subject)
    	  else Fail("Not implemented.")
    	    // Match with backtracking.  This is tricky, tricky.
      case _ => Fail("Not implemented.")
    }

  def rewrite(binds: Bindings) = {
    // We must rewrite every child atom, and collect them into a new sequence.
    var changed = false
    def doit(atoms: List[BasicAtom]): List[BasicAtom] =
      if (atoms.isEmpty) List() else {
        val (newatom, change) = atoms.head.rewrite(binds)
        changed |= change
        newatom :: doit(atoms.tail)
      }
    val newlist = doit(atoms.toList)
    if (changed) (AtomList(newlist), true) else (this, false)
  }

  // An atom list is just the list of atoms, separated by commas.  It is up to
  // the enclosing scope to determine if parens are needed.
  def toParseString = atoms.mkParseString("", ", ", "")
}
