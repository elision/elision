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
import sjp.elision.ElisionException

/**
 * An exception indicating an illegal operator definition.
 */
class IllegalOperatorDefinition(msg: String) extends ElisionException(msg)

/**
 * Common root class for an operator definition.
 */
abstract class OperatorDefinition(val proto: OperatorPrototype)
extends BasicAtom {
  val theType = TypeUniverse
  val deBruijnIndex = 0
  
  /** All operator definitions are terms. */
  val isTerm = true
  
  /**
   * Operator definitions do not maintain a constant pool since they are seldom
   * involved in matching.
   */
  val constantPool = None
  
  /**
   * The depth of an operator definition is equal to the depth of the prototype.
   */
  val depth = proto.depth
  
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) =
    Fail("Operator definition matching is not implemented.", this, subject)
    
	def rewrite(binds: Bindings) = (this, false)
  
  protected def check(proto: OperatorPrototype, props: AlgProp) {
    if (!props.associative.getOrElse(false) &&
        !props.commutative.getOrElse(false)) return
        
	  // Absorbers, identities, and idempotency are only allowed when an operator
	  // is associative.
	  if (!props.associative.getOrElse(false)) {
	    if (props.idempotent.getOrElse(false)) throw new IllegalOperatorDefinition(
	        "Idempotent operators must be associative.")
	    if (props.absorber != None) throw new IllegalOperatorDefinition(
	        "Operators with an absorber must be associative.")
	    if (props.identity != None) throw new IllegalOperatorDefinition(
	        "Operators with an identity must be associative.")
	  }
    
    // Associative operators must have exactly two parameters, and the
    // parameters must have the same type, and this type must match the
    // type of the fully-applied operator.
    
    // All parameters of a commutative operator must have the same type.
    
    // Both associative and commutative operators must have at least two
    // parameters.  An associative operator must have exactly two parameters.
    val pars = proto.pars
    if (props.associative.getOrElse(false) && pars.length != 2) {
      throw new IllegalOperatorDefinition(
          "Illegal definition for operator " + proto.toParseString +
          ".  Associative operators must have exactly two parameters.")
    }
    if (props.commutative.getOrElse(false) && pars.length < 2) {
      throw new IllegalOperatorDefinition(
          "Illegal definition for operator " + proto.toParseString +
          ".  Commutative operators must have at least two parameters.")
    }
    
    // All parameters must have the same type.
    val typ = pars(0).theType
    if (pars.exists(_.theType != typ))
      throw new IllegalOperatorDefinition(
          "Illegal definition for operator " + proto.toParseString +
          ".  All parameters of an associative or commutative operator must " +
          "have the same type.")
    
    // An associative operator must have the same type.
    if (props.associative.getOrElse(false) && proto.typ != typ)
      throw new IllegalOperatorDefinition(
          "Illegal definition for operator " + proto.toParseString +
          ".  The type an associative operator must be the same as the type " +
          "of the parameters.")
  }
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
 * @param ptype		The prototype.
 * @param props		Operator properties.
 */
case class SymbolicOperatorDefinition(ptype: OperatorPrototype,
    props: AlgProp) extends OperatorDefinition(ptype) {
  check(proto, props)
  lazy val isConstant = props.isConstant
	def toParseString =
	  "{ operator " + proto.toParseString + " " + props.toParseString + " }"
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
 * @param ptype		The prototype.
 * @param body		The operator definition.
 */
case class ImmediateOperatorDefinition(ptype: OperatorPrototype,
    body: BasicAtom) extends OperatorDefinition(ptype) {
  lazy val isConstant = body.isConstant
	def toParseString =
	  "{ operator " + proto.toParseString + " = " + body.toParseString + " }"
	override lazy val hashCode = proto.hashCode * 31 + body.hashCode
	override def equals(other: Any) = other match {
    case iod:ImmediateOperatorDefinition =>
      proto == iod.proto &&
      body == iod.body
    case _ => false
  }
}

/**
 * The operator prototype.
 * @param name			The operator name.
 * @param typepars	The type parameters.
 * @param pars			The parameters.
 * @param typ				The type.
 * @param guards		The guards, if any.
 */
case class OperatorPrototype(name: String, pars: IndexedSeq[Variable],
    typ: BasicAtom, guards: List[BasicAtom]) {
  def toParseString = name + pars.mkParseString("(", ",", ")") +
  	": " + typ.toParseString + guards.mkParseString(" ", "if ", "")

	/**
	 * This is a map from label name to the list of (zero-based) indices of the
	 * parameters that have that particular label, in order.
	 */
  val labelmap = scala.collection.mutable.HashMap[String,ListBuffer[Int]]()
  
  // Extract the labels into a convenient form.  We build a map from label
  // to the list of indices of the parameters that are marked with that label.
  private var pos = 0
  for (par <- pars; label <- par.labels) {
    labelmap.getOrElseUpdate(label, ListBuffer[Int]()).append(pos)
    pos += 1
  }
  	
  /**
   * We declare the depth of an operator prototype to be equal to the maximum
   * depth of its parameters, plus one.
   */
  val depth = pars.foldLeft(0)(_ max _.depth) + 1
  	
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

/** Simple operator prototype creation and pattern matching. */
object Proto {
  /**
   * Make a new operator prototype.  The operator cannot have guards.
   * 
   * @param name				The operator name.
   * @param typ					The type of a fully-applied operator.
   * @param parameters	The formal parameters.
   * @return	The new operator prototype.
   */
  def apply(name: String, typ: BasicAtom, parameters: (String, BasicAtom)*) =
    OperatorPrototype(name,
        parameters.map(x => Variable(x._2, x._1)).toIndexedSeq, typ, List())
}

case class OperatorHelp(short: String, long: String)