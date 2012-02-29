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
 * @param deBruijnIndex			The De Bruijn index.
 * @param lvar							The lambda variable which must match the De Bruijn
 * 													index.
 * @param body							The lambda body.
 */
case class Lambda private (deBruijnIndex: Int, lvar: Variable, body: BasicAtom)
extends BasicAtom with Applicable {
  /** The type is a mapping from the variable type to the body type. */
  val theType = OperatorLibrary.MAP(lvar.theType, body.theType)
  
  /**
   * A lambda is constant iff its body is constant.  This is different from
   * saying that the lambda is itself constant.  The lambda `\\$``x.$``y`
   * is a constant, but its body contains a variable, so it is not constant in
   * this sense.
   */
  val isConstant = body.isConstant
  
  /** The depth is equal to the depth of the body, plus one. */
  val depth = body.depth + 1
  
  /** A lambda's body may be a constant. */
  val constantPool =
    Some(BasicAtom.buildConstantPool(theType.hashCode, lvar, body))
	  	
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
  
  override lazy val hashCode = lvar.hashCode * 31 + body.hashCode
  
  override def equals(other: Any) = other match {
    case lambda:Lambda =>
      lvar == lambda.lvar &&
      body == lambda.body
    case _ => false
  }
  
  def doApply(atom: BasicAtom, binds: Bindings) =
	      // Curry the lambda body by binding the variable to the argument and then
	      // rewriting the body.
	      Applicable.bind1(body.rewrite(binds + (lvar.name -> atom))._1)
}

/**
 * Companion object with convenient methods to create lambdas.
 */
object Lambda {
  def apply(lvar: Variable, body: BasicAtom): Lambda = {
    /*
     * Note on De Bruijn indices.
     * 
     * This apply method is the point at which new De Bruijn indices are
     * created.  Basically when a lambda is being created, the De Bruijn
     * index of the body is checked.  This is incremented and a new variable
     * created based on the index.  The original variable is then replaced
     * in the body with the new variable.
     * 
     * For example:
     * \$x.fred($x,$y)  becomes  \$:1.fred($:1,$y)
     * \$x.(\$:1.fred($:1,$x))  becomes  \$:2.(\$:1.fred($:1,$:2))
     * 
     * There is a special case to consider, however.  Suppose we have a
     * lambda with this form.
     * 
     * \$x.$body
     * 
     * Lambdas represent functions, and in this case the lambda represents
     * a constant function: \$x.$body.7 = $body.
     * 
     * The difficulty arises if we later rewrite the body.  Consider this.
     * 
     * \$x.$body  becomes  \$:1.$body
     * 
     * We then rewrite $body to \$x.fred($x,$y), itself rewritten to be
     * \$:1.fred($:1,$y), with De Bruijn index of one.  Thus since we are
     * building a new lambda (thanks to rewriting the body) we create new
     * variable $:2 and rewrite the body mapping the old variable $:1 to
     * the new one $:2.  We get the following.
     * 
     * \$:2.\$:1.fred(\$:2,$y)
     * 
     * This isn't at all what we want.  What we really want is the following.
     * 
     * \$:1.$body      $body = \$:1.fred($:1,$y)
     * 
     * \$:2.\$:1.fred($:1,$y)
     * 
     * To get this we compute the index of the outer lambda as one more than
     * the index of the body (so it gets index 2).  We then do not rewrite the
     * lambda body since the lambda variable is already a De Bruijn index.
     * 
     * The simple answer is that we do not rewrite De Bruijn indices.  Terms
     * are immutable, and the De Bruijn indices were created (correctly) when
     * the term was originally created.  So...  We reject rewriting De Bruijn
     * indices when creating lambdas.
     * 
     * Now, when evaluating lambdas, we have to rewrite De Bruijn indices.  So
     * we only block rewriting of one De Bruijn index to a different De Bruijn
     * index.  That logic can be found in the Variable class.
     */
    
    // First compute the De Bruijn index of the term.  It is equal to one
    // greater than the maximum index of the body.
    val deBruijnIndex = body.deBruijnIndex + 1
    
    // Now make a new De Bruijn variable for the index.
    val newvar = new Variable(lvar.theType, ":"+deBruijnIndex) {
      override val isDeBruijnIndex = true
    }
    
    // Bind the old variable to the new one.
    var binds = new Bindings
    binds += (lvar.name -> newvar)
    val (newbody, _) = body.rewrite(binds)
    
    // Make and return the new lambda.
    Lambda(deBruijnIndex, newvar, newbody)
  }
}
