/* Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 */
package sjp.elision.core

/**
 * A lambda creates an operator that binds a single variable in a term.
 * @param lvar		The lambda variable.
 * @param body		The lambda body.
 */
case class Lambda(lvar: Variable, body: BasicAtom) extends BasicAtom {
  // The type is a mapping from one type to another.
	val theType =
	  Apply(Operator("MAP"), AtomList(Seq(lvar.theType, body.theType)))
  
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    subject match {
	  case Lambda(olvar, obody) => if (olvar == lvar) {
	    body.tryMatch(obody, binds) match {
	      case fail: Fail =>
	        Fail("Lambda bodies do not match.", this, subject)
	      case mat: Match => mat
	      case mat: Many => mat
	    }
	  } else Fail("Lambda variables do not match.", this, subject)
	  case _ => Fail("Lambdas only match other lambdas.", this, subject)
	}

  def rewrite(binds: Bindings): (BasicAtom, Boolean) =
    body.rewrite(binds) match {
	    case (newatom, changed) if changed => (Lambda(lvar, newatom), true)
	    case _ => (this, false)
	  }
  
  override def toString = "\\" + lvar.toString + "." + body.toString
}
