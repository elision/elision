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
import scala.collection.mutable.ListBuffer

/**
 * An incorrect argument list was supplied to an operator.
 * @param msg	The human-readable message describing the problem.
 */
class ArgumentListException(msg: String) extends Exception(msg)

/**
 * Encapsulate an operator.
 * 
 * ==Structure and Syntax==
 * 
 * ==Type==
 * 
 * ==Equality and Matching==
 * 
 * @param opdef	The operator definition.
 */
case class Operator(opdef: OperatorDefinition) extends BasicAtom {
  // The type of all operators is OPTYPE.  This is not a good thing; it should
  // really be a MAP from the operator domain to the codomain.  Something to
  // fix...
  // TODO Fix operator type.
  val theType = OPTYPE
  val isConstant = opdef.isConstant
  
  /** The native handler, if one is declared. */
  protected[core] var handler: Option[(String,AtomList) => BasicAtom] = None
  
  /** Provide quick access to the operator name. */
  lazy val name = opdef.proto.name
  
  val deBruijnIndex = 0

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    subject match {
      case Operator(oopdef) if opdef == oopdef => Match(binds)
      case _ => Fail("Operators do not match.", this, subject)
    }

  def rewrite(binds: Bindings) = (this, false)

  def toParseString = toESymbol(name) + ":OPTYPE"
    
  override lazy val hashCode = opdef.hashCode
  
  override def equals(other: Any) = other match {
    case op:Operator => opdef == op.opdef
    case _ => false
  }
}
