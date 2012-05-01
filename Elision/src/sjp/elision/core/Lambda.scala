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
import sjp.elision.ElisionException

/* Notes on De Bruijn indices.
 * 
 * The De Bruijn index (DBI) is the number of binders in scope for a given
 * lambda variable.
 * 
 * \$x.\$y.\$z.($x.$z.($y.$z))
 *               3  1   2  1
 *               
 * We rewrite this in the De Bruijn notation as:
 * 
 * λ.λ.λ.3 1 (2 1)
 * 
 * (This is the S combinator from SKI calculus.)
 * 
 * As another example, consider this atom.
 * 
 * \$x.\$y.$x
 *         2
 * 
 * λ.λ.2
 * 
 * (This is the K combinator from SKI calculus.)
 * 
 * Let's consider $S.$K.
 *     
 * \$x.\$y.\$z.($x.$z.($y.$z)).\$x.\$y.$x
 *     \$y.\$z.((\$x.\$y.$x).$z.($y.$z))
 *     \$y.\$z.(\$y.$z.($y.$z))
 *     \$y.\$z.($z)
 *     
 * And now $S.$K.$K.
 * 
 * $S.$K.$K
 * \$y.\$z.($z).(\$x.\$y.\$x)
 * \$z.$z
 * 
 * Thus we get the identity.
 */

/**
 * A lambda variable does not match the body.
 */
class LambdaVariableMismatchException(msg: String)
extends ElisionException(msg)

/**
 * A lambda application results in unbounded recursion.
 */
class LambdaUnboundedRecursionException(msg: String)
extends ElisionException(msg)

/**
 * A lambda creates an operator that binds a single variable in a term.
 * 
 * ==Structure and Syntax==
 * A lambda is indicated by a backslash (`\`) followed by the lambda variable,
 * a dot (`.`), and the lambda body.
 * {{{
 * \\$``x.7                -> Constant function
 * \\$``x.$``x               -> Identity function
 * \\$``x.add($``x,$``x)       -> Doubling function
 * }}}
 * In order to protect the lambda variable from rewriting or binding it is
 * converted to a De Bruijn index as described in the documentation for
 * [[sjp.elision.core.BasicAtom]] (see the field `deBruijnIndex`).
 * 
 * ==Type==
 * The type of a lambda is a mapping from the type of the lambda variable to
 * the type of the lambda body.  Of course either - or both - may be variables.
 * 
 * ==Equality and Matching==
 * Lambdas are equal iff their variables and bodies are equal ''after'' the
 * De Bruijn index substitution.  This means that the following two lambdas
 * are equal.
 * {{{
 * \\$``x.$``x
 * \\$``y.$``y
 * }}}
 * Both are rewritten to <code>\\$`:1`.$`:1`</code>.
 * 
 * @param lvar							The lambda variable which must match the De Bruijn
 * 													index.
 * @param body							The lambda body.
 * @param isFixed						If true, this is a fixed lambda, meaning that the
 * 													body is always returned and never rewritten.
 */
class Lambda private (val lvar: Variable, val body: BasicAtom, isFixed: Boolean)
extends BasicAtom with Applicable {
  /** The type is a mapping from the variable type to the body type. */
  val theType = SymbolicOperator.MAP(lvar.theType, body.theType)
  
  /**
   * A lambda is constant iff its body is constant.  This is different from
   * saying that the lambda is itself constant.  The lambda `\\$``x.$``y`
   * is a constant, but its body contains a variable, so it is not constant in
   * this sense.
   */
  val isConstant = body.isConstant
  
  /** The De Bruijn index is the max of the parameter and body. */
  val deBruijnIndex = body.deBruijnIndex max lvar.deBruijnIndex
  
  /** The lambda is a term iff its body is a term. */
  val isTerm = body.isTerm
  
  /** The depth is equal to the depth of the body, plus one. */
  val depth = body.depth + 1
  
  /** A lambda's body may be a constant. */
  val constantPool =
    Some(BasicAtom.buildConstantPool(theType.hashCode, lvar, body))
    
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) =
    subject match {
	  case Lambda(olvar, obody) => if (olvar == lvar) {
	    body.tryMatch(obody, binds, hints) match {
	      case fail: Fail =>
	        Fail("Lambda bodies do not match.", this, subject)
	      case mat: Match => mat
	      case mat: Many => mat
	    }
	  } else Fail("Lambda variables do not match.", this, subject)
	  case _ => Fail("Lambdas only match other lambdas.", this, subject)
	}

  def rewrite(binds: Bindings): (BasicAtom, Boolean) = {
    // We test for a special case here.  If the bindings specify that we
    // should rewrite our own bound De Bruijn index, we explicitly ignore
    // it.
    val newbinds = binds - lvar.name
    body.rewrite(newbinds) match {
	    case (newatom, changed) if changed => (Lambda(lvar, newatom), true)
	    case _ => (this, false)
	  }
  }
  
  def toParseString = "\\" + lvar.toParseString + "." + body.toParseString
  
  override def toString = "Lambda(" + lvar + "," + body + ")"
  
  override lazy val hashCode = lvar.hashCode * 31 + body.hashCode
  
  override def equals(other: Any) = other match {
    case lambda:Lambda =>
      lvar == lambda.lvar &&
      body == lambda.body
    case _ => false
  }
  
  def doApply(atom: BasicAtom) = {
    // Lambdas are very general; their application can lead to a stack overflow
    // because it is possible to model unbounded recursion.  Catch the stack
    // overflow here, and bail out.
    try {
	    // Make it possible to check types by matching the variable against the
	    // argument instead of just binding.  For pure binding without checking
	    // types, use a bind.
	    lvar.tryMatch(atom) match {
	      case fail:Fail =>
	        throw new LambdaVariableMismatchException(
	            "Lambda variable does not match body: " + fail.theReason)
	      case Match(binds) =>
	        // Great!  Now rewrite the body with the bindings.
		      body.rewrite(binds)._1
	      case Many(iter) =>
	        body.rewrite(iter.next)._1
	    }
    } catch {
      case ex:java.lang.StackOverflowError =>
        // Trapped unbounded recursion.
        throw new LambdaUnboundedRecursionException(
            "Lambda application results in unbounded recursion: (" +
            this.toParseString + ").(" + atom.toParseString + ")")
    }
  }
}

/**
 * Companion object with convenient methods to create lambdas.
 */
object Lambda {
  /** Whether to use De Bruijn indices. */
  var useDeBruijnIndices = true
  
  //+var depth: Int = 0
  
  //+def print(str: String) = println("  " * depth + str)
  
  def unapply(lambda: Lambda) = Some(lambda.lvar, lambda.body)
  
  def apply(lvar: Variable, body: BasicAtom): Lambda = {
    //+print("Asked to make \\" + lvar.toParseString + "." + body.toParseString)
    //+depth += 1
    // Make and return the new lambda.
    if (useDeBruijnIndices) {
      // Decide what De Bruijn index to use for this lambda.  We will use one
      // greater than the maximum index of the body.
	    val dBI = body.deBruijnIndex + 1
	    //+print("Body " + body.toParseString + " (dBI: " + body.deBruijnIndex + ")")
	    //+print("Using DBI " + dBI)

	    // Classes that implement De Bruijn indices.
	    class DBIV(typ: BasicAtom, val dBI: Int, guard: BasicAtom, lvar: Set[String])
	    extends Variable(typ, ":" + dBI, guard, lvar) {
	      override val isDeBruijnIndex = true
	      override val deBruijnIndex = dBI
      }
      class DBIM(typ: BasicAtom, val dBI: Int, guard: BasicAtom, lvar: Set[String])
      extends MetaVariable(typ, ":" + dBI, guard, lvar) {
	      override val isDeBruijnIndex = true
	      override val deBruijnIndex = dBI
      }
	    
	    // Now make a new De Bruijn variable for the index.
	    val newvar = (
        if (lvar.isTerm)
        	new DBIV(lvar.theType, dBI, lvar.guard, lvar.labels)
        else
        	new DBIM(lvar.theType, dBI, lvar.guard, lvar.labels)
      )
      //+print("Constructed variable: " + newvar.toParseString)
	    
	    // Bind the old variable to the new one and rewrite the body.
	    var binds = Bindings()
	    binds += (lvar.name -> newvar)
	    //+print("Bindings are: " + binds.toParseString)
	    val (newbody, notfixed) = body.rewrite(binds)
	    //+print("Adjusted body to: " + newbody.toParseString)
	    
	    // Compute the new lambda.
	    //+print("Lambda is: \\" + newvar.toParseString + "." + newbody.toParseString)
	    //+depth -= 1
	    if (notfixed)	new Lambda(newvar, newbody, false)
	    else new Lambda(newvar, body, true)
    }
    else new Lambda(lvar, body, false)
  }
}
