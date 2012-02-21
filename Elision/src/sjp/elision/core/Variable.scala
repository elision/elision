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
 * 
 * ==Type==
 * 
 * ==Equality and Matching==
 * 
 * @param typ		The variable type.
 * @param name	The variable name.
 */
case class Variable(typ: BasicAtom, name: String) extends BasicAtom {
  /** The type of this variable. */
  val theType = typ
  val deBruijnIndex = 0
  val isConstant = false
  
  /** By default, variables can be bound. */
  override val isBindable = true

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    // We don't need to worry about the types here.  We can bind the variable
    // if the variable allows binding, and it is not already bound to a
    // different atom.  We also allow the variable to match ANYTYPE.
    if (isBindable) binds.get(name) match {
      case None =>
        // This is tricky.  We don't bind if we match against ANYTYPE.  Are
        // there unforseen consequences to this decision?
        if (subject == ANYTYPE) Match(binds)
        else Match(binds + (name -> subject))
      case Some(atom) if atom == ANYTYPE || atom == subject => Match(binds)
      case _ => Fail("Variable " + this.toParseString +
          " is already bound to the term " + binds.get(name).get.toParseString +
          ".", this, subject)
    }
    else Fail("Variable is not bindable.", this, subject)

  def rewrite(binds: Bindings) = {
    // If this variable is bound in the provided bindings, replace it with the
    // bound value.
    binds.get(name) match {
      case Some(atom) =>
        // We don't rewrite De Bruijn indices to different indices.
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

  def toParseString = "$" + toESymbol(name) +
  		(if (theType != ANYTYPE) ":" + typ.toParseString else "")
  
  override def toString = "Variable(" + typ + "," + toEString(name) + ")"
  
  override lazy val hashCode = typ.hashCode * 31 + name.hashCode
  
  override def equals(varx: Any) = varx match {
    case ovar:Variable => ovar.typ == typ && ovar.name == name
    case _ => false
  }
}
