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
 * @param deBrujinIndex			The De Brujin index.
 * @param lvar							The lambda variable which must match the De Brujin
 * 													index.
 * @param body							The lambda body.
 */
case class Lambda(deBrujinIndex: Int, lvar: Variable, body: BasicAtom)
extends BasicAtom {
  // The type is a mapping from one type to another.
	val theType =
	  Apply(Operator("MAP"), AtomList(Seq(lvar.theType, body.theType)))
	  	
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    subject match {
	  case Lambda(odbi, olvar, obody) => if (olvar == lvar) {
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
  
  def toParseString = "\\" + lvar.toParseString + "." + body.toParseString
}

/**
 * Companion object with convenient methods to create lambdas.
 */
object Lambda {
  def apply(lvar: Variable, body: BasicAtom): Lambda = {
    println("==========")
    println("lvar = " + lvar)
    println("body = " + body)
    
    
    // First compute the De Brujin index of the term.  It is equal to one
    // greater than the maximum index of the body.
    val deBrujinIndex = body.deBrujinIndex + 1
    println("deBrujinIndex = " + deBrujinIndex)
    
    // Now make a new De Brujin variable for the index.
    val newvar = new Variable(lvar.theType, ":"+deBrujinIndex) {
      override val isDeBrujinIndex = true
    }
    println("newvar = " + newvar)
    
    // Bind the old variable to the new one.
    var binds = new Bindings
    binds += (lvar.name -> newvar)
    println("binds = " + binds)
    val (newbody, _) = body.rewrite(binds)
    println("body = " + body)
    println("newbody = " + newbody)
    println("==========")
    
    // Make and return the new lambda.
    Lambda(deBrujinIndex, newvar, newbody)
  }
}