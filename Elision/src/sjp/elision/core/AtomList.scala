/* Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 */
package sjp.elision.core

import scala.collection.immutable.HashMap
import java.util.LinkedList

/**
 * Encapsulate an ordered list of atoms.
 */
case class AtomList(atoms: BasicAtom*) extends BasicAtom {
  val theType = TypeUniverse

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    // Ordered lists only match other ordered lists with matching elements in
    // the same order.
    subject match {
    	case AtomList(oatoms@_*) =>
    	  // The lists must be the same length, or they cannot match.
    	  if (atoms.length != oatoms.length) Fail("Lists are different sizes.", this, subject)
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
    if (changed) (AtomList(newlist: _*), true) else (this, false)
  }

  override def toString = atoms.mkString(", ")
}