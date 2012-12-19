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
package ornl.elision.core
import ornl.elision.util.ElisionException
import ornl.elision.actors.ReplActor

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
 * \.\.\.3 1 (2 1)
 * 
 * (This is the S combinator from SKI calculus.)
 * 
 * As another example, consider this atom.
 * 
 * \$x.\$y.$x
 *         2
 * 
 * \.\.2
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
 * To create an instance (or to match an instance) use the methods in the
 * companion object.
 * 
 * == Structure and Syntax ==
 * A lambda is indicated by a backslash (`\`) followed by the lambda variable,
 * a dot (`.`), and the lambda body.
 * {{{
 * \\$``x.7                -> Constant function
 * \\$``x.$``x               -> Identity function
 * \\$``x.add($``x,$``x)       -> Doubling function
 * }}}
 * In order to protect the lambda variable from rewriting or binding it is
 * converted to a De Bruijn index as described in the documentation for
 * [[ornl.elision.core.BasicAtom]] (see the field `deBruijnIndex`).
 * 
 * == Type ==
 * The type of a lambda is a mapping from the type of the lambda variable to
 * the type of the lambda body.  Of course either - or both - may be variables.
 * 
 * == Equality and Matching ==
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
  lazy val theType = SymbolicOperator.MAP(lvar.theType, body.theType)
  
  /**
   * A lambda is constant iff its body is constant.  This is different from
   * saying that the lambda is itself constant.  The lambda `\\$``x.$``y`
   * is a constant, but its body contains a variable, so it is not constant in
   * this sense.
   */
  lazy val isConstant = body.isConstant
  
  /** The De Bruijn index is the max of the parameter and body. */
  lazy val deBruijnIndex = body.deBruijnIndex max lvar.deBruijnIndex
  
  /**
   * The lambda is a term iff its body is a term.  
   */
  lazy val isTerm = body.isTerm  
  lazy val depth = body.depth + 1
    
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) =
    subject match {
	  case Lambda(olvar, obody) => if (olvar == lvar) {

      // Has rewriting timed out?
      if (BasicAtom.rewriteTimedOut) {
        Fail("Timed out", this, subject)
      }

      else {
	      body.tryMatch(obody, binds, hints) match {
	        case fail: Fail =>
	          Fail("Lambda bodies do not match.", this, subject)
	        case mat: Match => mat
	        case mat: Many => mat
	      }
      }
	  } else Fail("Lambda variables do not match.", this, subject)
	  case _ => Fail("Lambdas only match other lambdas.", this, subject)
	}

  def rewrite(binds: Bindings): (BasicAtom, Boolean) = {
    ReplActor ! ("Eva", "pushTable", "Lambda rewrite")
    ReplActor ! ("Eva", "addToSubroot", ("rwNode", "Lambda rewrite: "))
    ReplActor ! ("Eva", "addTo", ("rwNode", "body", body))
    ReplActor ! ("Eva", "setSubroot", "body")
	
    // We test for a special case here.  If the bindings specify that we
    // should rewrite our own bound De Bruijn index, we explicitly ignore
    // it.
    val newbinds = binds - lvar.name
    body.rewrite(newbinds) match {
	    case (newatom, changed) if changed => 
  			val newLambda = Lambda(lvar, newatom)
  			ReplActor ! ("Eva", "addTo", ("rwNode", "", newLambda))
        ReplActor ! ("Eva", "popTable", "Lambda rewrite")
        (newLambda, true)
	    case _ => 
        ReplActor ! ("Eva", "popTable", "Lambda rewrite")
        (this, false)
	  }
  }
  
  override lazy val hashCode = lvar.hashCode * 31 + body.hashCode
  lazy val otherHashCode = lvar.otherHashCode + 8191*body.otherHashCode
  
  override def equals(other: Any) = other match {
    case lambda:Lambda =>
      feq(lambda, this, lvar == lambda.lvar && body == lambda.body)
      
    case _ =>
      false
  }
  
  def doApply(atom: BasicAtom, bypass: Boolean) = {
    ReplActor ! ("Eva", "pushTable", "Lambda doApply")
    // top node of this subtree
    ReplActor ! ("Eva", "addToSubroot", ("rwNode", "Lambda doApply: ")) // val rwNode = RWTree.addToCurrent("Lambda doApply: ")
	
    ReplActor ! ("Eva", "addTo", ("rwNode", "body", "body: ", body)) //val bodyNode = RWTree.addTo(rwNode, "body", body)
  	ReplActor ! ("Eva", "addTo", ("rwNode", "atom", "argument: ", atom)) // val atomNode = RWTree.addTo(rwNode, "match atom :?", atom)
  	ReplActor ! ("Eva", "setSubroot", "body") // RWTree.current = bodyNode
	
    // Lambdas are very general; their application can lead to a stack overflow
    // because it is possible to model unbounded recursion.  Catch the stack
    // overflow here, and bail out.
    try {
	    // Make it possible to check types by matching the variable against the
	    // argument instead of just binding.  For pure binding without checking
	    // types, use a bind.
	    lvar.tryMatch(atom) match {
	      case fail:Fail =>
          ReplActor ! ("Eva", "popTable", "Lambda doApply")
	        throw new LambdaVariableMismatchException(
	            "Lambda argument does not match parameter: " + fail.theReason)
	      case Match(binds) =>
	        // Great!  Now rewrite the body with the bindings.
		      val newbody = body.rewrite(binds)._1
	        ReplActor ! ("Eva", "addTo", ("rwNode", "", newbody)) // RWTree.addTo(rwNode, newbody)
          ReplActor ! ("Eva", "popTable", "Lambda doApply")
          newbody
	      case Many(iter) =>
	        val newbody = body.rewrite(iter.next)._1
          ReplActor ! ("Eva", "addTo", ("rwNode", "", newbody)) // RWTree.addTo(rwNode, newbody)
          ReplActor ! ("Eva", "popTable", "Lambda doApply")
          newbody
	    }
    } catch {
      case ex:java.lang.StackOverflowError =>
        // Trapped unbounded recursion.
        val errorString = "Lambda application results in unbounded recursion: (" +
        this.toParseString + ").(" + atom.toParseString + ")"
        ReplActor ! ("Eva", "addTo", ("rwNode", "", errorString)) // RWTree.addTo(rwNode, errorString)
        ReplActor ! ("Eva", "popTable", "Lambda doApply")
        throw new LambdaUnboundedRecursionException(errorString)
    }
  }
}

/**
 * Companion object with convenient methods to create lambdas.
 */
object Lambda {
  /**
   * Control whether we are using De Bruijn indices.  This is `true` by
   * default, and you shoud probably '''leave it alone''' unless you are
   * doing something that involves debugging lambdas.  You aren't, so don't
   * modify this.
   */
  var useDeBruijnIndices = true
  
  /**
   * Break a lambda into its parameter and body.
   * 
   * @param lambda	The lambda to match.
   * @return	The variable and then body.
   */
  def unapply(lambda: Lambda) = Some(lambda.lvar, lambda.body)
  
  
  /**
   * Make a lambda from the provided parameter and body.
   *
   * @param lvar	The lambda parameter.
   * @param body	The lambda body.
   */
  def apply(lvar: Variable, body: BasicAtom): Lambda = {
    ReplActor ! ("Eva", "pushTable", "obj Lambda apply")
    ReplActor ! ("Eva", "addToSubroot", ("rwNode", "object Lamda apply: "))
    ReplActor ! ("Eva", "addTo", ("rwNode", "lvar", "parameter: ", lvar))
    ReplActor ! ("Eva", "addTo", ("rwNode", "body", "body: ", body))
	
    // Make and return the new lambda.
    if (useDeBruijnIndices) {
      // Decide what De Bruijn index to use for this lambda.  We will use one
      // greater than the maximum index of the body.
	    val dBI = body.deBruijnIndex + 1

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
      ReplActor ! ("Eva", "setSubroot", "lvar") // RWTree.current = lvarNode
	    val newvar = (
        if (lvar.isTerm)
        	new DBIV(lvar.theType, dBI, lvar.guard, lvar.labels)
        else
        	new DBIM(lvar.theType, dBI, lvar.guard, lvar.labels)
        )
        ReplActor ! ("Eva", "addTo", ("lvar", "", newvar)) // RWTree.addTo(lvarNode, newvar) 
		
	    // Bind the old variable to the new one and rewrite the body.
	    var binds = Bindings()
	    binds += (lvar.name -> newvar)
	    ReplActor ! ("Eva", "setSubroot", "body") // RWTree.current = bodyNode
	    val (newbody, notfixed) = body.rewrite(binds)
	    ReplActor ! ("Eva", "addTo", ("body", "", newbody)) // RWTree.addTo(bodyNode, newbody)
	    
	    // Compute the new lambda.
	    if (notfixed)	{
        ReplActor ! ("Eva", "popTable", "obj Lambda apply")
        new Lambda(newvar, newbody, false)
      }
	    else {
        ReplActor ! ("Eva", "popTable", "obj Lambda apply")
        new Lambda(newvar, body, true)
      }
    }
    else {
      ReplActor ! ("Eva", "popTable", "obj Lambda apply")
      new Lambda(lvar, body, false)
    }
  }
}
