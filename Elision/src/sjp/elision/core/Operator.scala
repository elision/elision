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

/**
 * A type for all operators.
 */
object OPTYPE extends RootType {
  val theType = TypeUniverse
  
  def toParseString = "OPTYPE"
    
  override def toString = "OPTYPE"
}

/**
 * Encapsulate an operator.
 * @param opdef	The operator definition.
 */
case class Operator(opdef: OperatorDefinition) extends BasicAtom {
  val theType = OPTYPE
  
  /** Provide quick access to the operator name. */
  lazy val name = opdef.proto.name
  
  val deBrujinIndex = 0

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    subject match {
      case Operator(oopdef) if opdef == oopdef => Match(binds)
      case _ => Fail("Operators do not match.", this, subject)
    }

  def rewrite(binds: Bindings) = (this, false)

  def toParseString = toESymbol(name) + ":OPTYPE"
  
  /**
   * Apply this operator to the given argument.  This is the correct way to
   * apply an operator.  The argument can be any atom.
   * @param arg	The argument.
   * @return	A new atom.
   */
  def apply(arg: BasicAtom) = arg match {
    /*
     * How Operator Applications Get Created
     * 
     * Okay, so you come here to create an operator.  If the argument is not
     * an atom list, then the application is immediately created and returned.
     * No native handler or immediate rewrite is applied.
     * 
     * Otherwise you have to do some work.  In order to do this properly, you
     * have to know what the operator is, have its prototype and properties,
     * and do some processing.  All those ingredients are present here, so
     * this is where the magic happens.
     * 
     * This is how an operator application is handled.
		 * 
		 *  # If the argument is not an atom list, then the application is immediately
		 *    created and returned.  No native handlers are invoked and no immediate
		 *    rewrites are applied.
		 *  # If the argument is an atom list, then the atom list properties are
		 *    checked.  If they are set, they must match the operator's properties,
		 *    or an ArgumentListException is thrown.
		 *  # A new argument list is allocated with the same properties as the operator.
		 *  # Each argument in the original list is considered, and the following is
		 *    done.
		 *  # The arguments are matched against the parameter list.  If the match fails,
		 *    then an ArgumentListException is thrown.
		 *  # Any instances of an identity are discarded.
		 *  # If an absorber is found, it is immediately returned as the result.
		 *  # If no arguments are left, and the operator has an identity, then the
		 *    identity is returned as the result.
		 *  # If the operator is a native operator, and a closure has been registered,
		 *    then the new argument list is passed to the closure.  Note that this means
		 *    that the closure might not get invoked; for instance, if an absorber is
		 *    found.  The result of the closure is returned as the result.
		 *  # If no closure is present for a native operator, or if the operator is a
		 *    symbolic operator, then the operator prototype is rewritten with the
		 *    bindings from the parameter match.  The result is returned.
		 *  # Finally, for an immediate operator the bindings are applied to the
		 *    replacement, and the result is returned.
     */
    
    case al:AtomList =>
      // See if the atom list provides properties.
      al.props match {
        case None => // No properties specified; nothing to check.
        case Some((assoc,comm)) =>
      }
      Apply(this, arg)
    // If not an atom list, immediately make and return an apply.
    case _ => Apply(this, arg)
  }
}
