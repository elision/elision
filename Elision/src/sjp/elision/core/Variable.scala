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

import scala.collection.mutable.HashMap

/**
 * Represent a variable.
 * 
 * ==Structure and Syntax==
 * A variable is indicated with a leading dollar sign (`$`) followed by a
 * valid symbol.  So the following are valid variables:
 * - `$``x`
 * - `$``Fred51_2`
 * - <code>$`1`</code>
 * 
 * ==Type==
 * Every variable must have a type, and the type can be `ANYTYPE`.
 * 
 * ==Equality and Matching==
 * Variables are equal iff their name and type are equal.
 * 
 * Variables can be bound, so that gets checked during matching.  A variable
 * pattern matches a subject iff it is already bound to that subject, or if it
 * is unbound and the types match.
 * 
 * @param typ			The variable type.
 * @param name		The variable name.
 * @param guard		The variable's guard.
 * @param labels	Labels for this variable.
 */
class Variable(typ: BasicAtom, val name: String,
    val guard: BasicAtom = Literal.TRUE,
    val labels: Set[String] = Set[String]()) extends BasicAtom {
  /** The prefix for this variable. */
  protected val prefix = "$"
    
  /** This variable is a term. */
  val isTerm = true
  
  /** The type of this variable. */
  val theType = typ
  
  /** The De Bruijn index is zero. */
  val deBruijnIndex = 0
  
  /** Variables are not constant. */
  val isConstant = false
  
  /** The depth of a variable is zero. */
  val depth = 0
  
  /** By default, variables can be bound. */
  override val isBindable = true
  
  /** Variables contain no constant children. */
  val constantPool = None
  
  def bindMe(subject: BasicAtom, binds: Bindings): Outcome = {
    // Compute the new bindings.
    val newbinds = binds + (name -> subject)
    // Check any guard.
    if (guard.isTrue) Match(newbinds)
    else {
      val newterm = guard.rewrite(newbinds)._1
      if (newterm.isTrue) Match(newbinds)
      else Fail("Variable guard failed.  Is now: " + newterm.toParseString)
    }
  }
  
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) =
    // if the variable allows binding, and it is not already bound to a
    // different atom.  We also allow the variable to match ANYTYPE.
    if (isBindable) binds.get(name) match {
      case None =>
        // This is tricky.  We bind if we match against ANYTYPE.  Are
        // there unforseen consequences to this decision?  Otherwise we have
        // to add a binding of the variable name to the subject.
        bindMe(subject, binds)
      case Some(ANYTYPE) =>
        // We should re-bind this variable now.
        bindMe(subject, binds)
      case Some(atom) if subject == ANYTYPE || atom == subject =>
        // The variable is already bound, and it is bound to the subject, so
        // the match succeeds with the bindings as they are.
        Match(binds)
      case _ =>
        // The variable is already bound and it is bound to an unequal subject,
        // so the match fails.
        Fail("Variable " + this.toParseString +
          " is already bound to the term " + binds.get(name).get.toParseString +
          ".", this, subject)
    }
    else
      // Variables that are not bindable cannot be bound, and cannot match
      // any subject.  This is to prevent allowing them to "match" a bindable
      // variable of the same name and type, and having chaos ensue.
      Fail("Variable is not bindable.", this, subject)

  def rewrite(binds: Bindings) = {
    // If this variable is bound in the provided bindings, replace it with the
    // bound value.
    binds.get(name) match {
      case Some(atom) =>
        // We don't rewrite De Bruijn indices to different indices.  So if this
        // variable is a De Bruijn index, stop immediately without rewriting.
        // Otherwise return the bound value.
        if (isDeBruijnIndex && atom.isDeBruijnIndex) (this, false)
        else (atom, true)
      case None =>
        // While the atom is not bound, its type might have to be rewritten.
        theType.rewrite(binds) match {
          case (newtype, changed) =>
            if (changed) (Variable(newtype, name), true) else (this, false)
          case _ => (this, false)
        }
    }
  }

  def toParseString = prefix + toESymbol(name) +
  		(if (guard != Literal.TRUE) "{" + guard.toParseString + "}" else "") +
  		(if (theType != ANYTYPE) ":" + typ.toParseString else "") +
  		labels.map(" @" + toESymbol(_)).mkString("")
  
  override def toString =
    "Variable(" +
    typ + "," +
    toEString(name) + "," +
    guard.toString + "," +
    labels.map(toEString(_)).mkString("Set(", ",", ")") + ")"
  
  override lazy val hashCode = typ.hashCode * 31 + name.hashCode
  
  override def equals(varx: Any) = varx match {
    case ovar:Variable => ovar.theType == theType && ovar.name == name
    case _ => false
  }
}

object Variable {
  /**
   * Make a new variable instance.
   * 
	 * @param typ			The variable type.
	 * @param name		The variable name.
	 * @param guard		The variable's guard.
	 * @param labels	Labels for this variable.
	 */
  def apply(typ: BasicAtom, name: String, guard: BasicAtom = Literal.TRUE,
      labels: Set[String] = Set[String]()) =
        new Variable(typ, name, guard, labels)
  
  /**
   * Extract the parts of a variable.
   * 
   * @param vx	The variable.
   * @return	The type, name, guard, and labels.
   */
  def unapply(vx: Variable) = Some(vx.theType, vx.name, vx.guard, vx.labels)
}

/**
 * Define a metavariable.
 * 
 * @param typ			The variable type.
 * @param name		The variable name.
 * @param guard		The variable's guard.
 * @param labels	Labels for this variable.
 */
class MetaVariable(typ: BasicAtom, name: String,
    guard: BasicAtom = Literal.TRUE,
    labels: Set[String] = Set[String]())
    extends Variable(typ, name, guard, labels) {
  override val isTerm = false
  override val prefix = "$$"
  override def toString =
    "MetaVariable(" +
    typ + "," +
    toEString(name) + "," +
    guard.toString + "," +
    labels.map(toEString(_)).mkString("Set(", ",", ")") + ")"
}

object MetaVariable {
  /**
   * Make a new metavariable instance.
   * 
	 * @param typ			The variable type.
	 * @param name		The variable name.
	 * @param guard		The variable's guard.
	 * @param labels	Labels for this variable.
	 */
  def apply(typ: BasicAtom, name: String, guard: BasicAtom = Literal.TRUE,
      labels: Set[String] = Set[String]()) =
        new MetaVariable(typ, name, guard, labels)
  
  /**
   * Extract the parts of a metavariable.
   * 
   * @param vx	The variable.
   * @return	The type, name, guard, and labels.
   */
  def unapply(vx: MetaVariable) = Some(vx.theType, vx.name, vx.guard, vx.labels)
}
