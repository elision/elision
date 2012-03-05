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
 * An operator, by itself, is simply a name for a function that maps from some
 * domain to some codomain.  An operator appears as a simple symbol whose type
 * is designated as OPTYPE (to force looking up the operator).  Otherwise
 * operators are detected when they are applied to some argument list.
 * 
 * ==Type==
 * The type of an operator is taken from its definition.
 * 
 * ==Equality and Matching==
 * Operators are equal iff their operator definitions are equal.  They match
 * if their operator definitions match.
 * 
 * @param opdef	The operator definition.
 */
case class Operator(opdef: OperatorDefinition) extends BasicAtom {
	import OperatorLibrary._

	/**
   * The type of an operator is a mapping from the operator domain to the
   * operator codomain.
   */
  lazy val theType = opdef.proto.pars match {
	  case Seq() => MAP(Literal.NOTHING, opdef.proto.typ)
	  case Seq(atom) => MAP(atom.theType, opdef.proto.typ)
	  case _ => MAP(xx(opdef.proto.pars.map(_.theType)), opdef.proto.typ)
	}
  
  /** The operator is constant iff its definition is a constant. */
  val isConstant = opdef.isConstant
  
  /**
   * Since operator definitions do not maintain a constant pool, neither does
   * an operator.
   */
  val constantPool = None
  
  /** The native handler, if one is declared. */
  protected[core]
  var handler: Option[(Operator,AtomList,Option[Bindings]) => BasicAtom] = None
  
  /** Provide quick access to the operator name. */
  lazy val name = opdef.proto.name
  
  val deBruijnIndex = 0
  
  /** The depth of an operator is zero. */
  val depth = 0

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

/**
 * A ''proto''operator is an operator whose type can be overridden.
 * 
 * Typically the type of an operator is inferred from the types of its
 * parameter list, but the result would itself be an operator application
 * (a MAP) and this means we could potentially have an infinite regress.
 * To avoid that we define this class solely for use in defining the special
 * operators that are used to specify the types of other operators.
 * 
 * @param typ			The typ of this operator.
 * @param opdef		The operator definition.
 */
class ProtoOperator(typ: BasicAtom, override val opdef: OperatorDefinition)
extends Operator(opdef) {
  override lazy val theType = typ
}

/**
 * Companion object for the proto operator.
 */
object ProtoOperator {
  /**
   * Make a new proto operator.
   * 
	 * @param typ			The typ of this operator.
	 * @param opdef		The operator definition.
   */
  def apply(typ: BasicAtom, opdef: OperatorDefinition) =
    new ProtoOperator(typ, opdef)
}
