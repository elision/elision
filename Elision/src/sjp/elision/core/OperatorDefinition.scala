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
import scala.collection.mutable.LinkedList
import scala.collection.mutable.ListBuffer

/**
 * An exception indicating an illegal operator definition.
 */
class IllegalOperatorDefinition(msg: String) extends Exception(msg)

/**
 * Common root class for an operator definition.
 */
abstract class OperatorDefinition(val proto: OperatorPrototype)
extends BasicAtom {
  val theType = TypeUniverse
  val deBrujinIndex = 0
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    Fail("Operator definition matching is not implemented.", this, subject)
	def rewrite(binds: Bindings) = (this, false)
}

/**
 * Encapsulate an operator definition.
 * 
 * ==Structure and Syntax==
 * 
 * ==Type==
 * 
 * ==Equality and Matching==
 * 
 * @param proto		The prototype.
 * @param props		Operator properties.
 */
case class SymbolicOperatorDefinition(override val proto: OperatorPrototype,
    props: OperatorProperties) extends OperatorDefinition(proto) {
  lazy val isConstant = props.isConstant
	def toParseString =
	  "operator { " + proto.toParseString + " " + props.toParseString + " }"
	override lazy val hashCode = proto.hashCode * 31 + props.hashCode
	override def equals(other: Any) = other match {
    case sod:SymbolicOperatorDefinition =>
      proto == sod.proto &&
      props == sod.props
    case _ => false
  }
}

/**
 * Encapsulate an immediate operator definition.
 * @param proto		The prototype.
 * @param body		The operator definition.
 */
case class ImmediateOperatorDefinition(override val proto: OperatorPrototype,
    body: BasicAtom) extends OperatorDefinition(proto) {
  lazy val isConstant = body.isConstant
	def toParseString =
	  "operator { " + proto.toParseString + " = " + body.toParseString + " }"
	override lazy val hashCode = proto.hashCode * 31 + body.hashCode
	override def equals(other: Any) = other match {
    case iod:ImmediateOperatorDefinition =>
      proto == iod.proto &&
      body == iod.body
    case _ => false
  }
}

/**
 * Encapsulate a native operator definition.
 * @param proto		The prototype.
 * @param props		Operator properties.
 */
case class NativeOperatorDefinition(override val proto: OperatorPrototype,
    props: OperatorProperties) extends OperatorDefinition(proto) {
  lazy val isConstant = props.isConstant
	def toParseString =
	  "native { " + proto.toParseString + " " + props.toParseString + " }"
	override lazy val hashCode = proto.hashCode * 31 + props.hashCode
	override def equals(other: Any) = other match {
    case nod:NativeOperatorDefinition =>
      proto == nod.proto &&
      props == nod.props
    case _ => false
  }
}

/**
 * The operator prototype.
 * @param name			The operator name.
 * @param typepars	The type parameters.
 * @param pars			The parameters.
 * @param typ				The type.
 */
case class OperatorPrototype(name: String, pars: List[Variable], typ: BasicAtom) {
  def toParseString = name + pars.mkParseString("(", ",", ")") +
  	": " + typ.toParseString
  	
  // To make a Scala parseable string we have to make the name parseable.
  override def toString = "OperatorPrototype(" + toEString(name) + ", " +
  	pars.toString + ", " + typ.toString + ")"
  	
  override lazy val hashCode = (name.hashCode * 31 + pars.hashCode) * 31 +
      typ.hashCode
      
  override def equals(other: Any) = other match {
    case op:OperatorPrototype =>
      name == op.name &&
      pars == op.pars &&
      typ == op.typ
    case _ => false
  }
}

/**
 * Encapsulate operator properties.
 * @param assoc			True iff associative.  Default is false.
 * @param comm			True iff commutative.  Default is false.
 * @param idem			True iff idempotent.  Default is false.
 * @param absorber	The absorber, if any.  Default is None.
 * @param identity	The identity, if any.  Default is None.
 */
case class OperatorProperties(
    assoc: Boolean = false,
    comm: Boolean = false,
    idem: Boolean = false,
    absorber: Option[BasicAtom] = None,
    identity: Option[BasicAtom] = None) {
  lazy val isConstant = (absorber, identity) match {
    case (None, None) => true
    case (Some(atom), None) => atom.isConstant
    case (None, Some(atom)) => atom.isConstant
    case (Some(a1), Some(a2)) => a1.isConstant && a2.isConstant
  }
  
  // Absorbers, identities, and idempotency are only allowed when an operator
  // is associative.
  if (!assoc) {
    if (idem) throw new IllegalOperatorDefinition(
        "Idempotent operators must be associative.")
    if (absorber != None) throw new IllegalOperatorDefinition(
        "Operators with an absorber must be associative.")
    if (identity != None) throw new IllegalOperatorDefinition(
        "Operators with an identity must be associative.")
  }
  
  /**
   * The properties as a comma-delimited string.
   */
  private lazy val propstr = {
    var list = ListBuffer[String]()
    if (assoc) list += "associative"
    if (comm) list += "commutative"
    if (idem) list += "idempotent"
    absorber match {
      case Some(atom) => list += "absorber " + atom.toParseString
      case None =>
    }
    identity match {
      case Some(atom) => list += "identity " + atom.toParseString
      case None =>
    }
    list.mkString("", ", ", "")
  }
  
  def toParseString = if (propstr.length > 0) "is " + propstr else ""
    
  override lazy val hashCode = (((assoc.hashCode * 31 + comm.hashCode) * 31 +
      idem.hashCode) * 31 + absorber.hashCode) * 31 + identity.hashCode

  override def equals(other: Any) = other match {
    case op:OperatorProperties =>
      assoc == op.assoc &&
      comm == op.comm &&
      idem == op.idem &&
      absorber == op.absorber &&
      identity == op.identity
    case _ => false
  }
}
