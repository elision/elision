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
  
  /**
   * Operator definitions do not maintain a constant pool since they are seldom
   * involved in matching.
   */
  val constantPool = None
  
  /**
   * The depth of an operator definition is equal to the depth of the prototype.
   */
  val depth = proto.depth
  
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    Fail("Operator definition matching is not implemented.", this, subject)
	def rewrite(binds: Bindings) = (this, false)
  
  protected def check(proto: OperatorPrototype, props: OperatorProperties) {
    if (!props.associative && !props.commutative) return
    
    // Associative operators must have exactly two parameters, and the
    // parameters must have the same type, and this type must match the
    // type of the fully-applied operator.
    
    // All parameters of a commutative operator must have the same type.
    
    // Both associative and commutative operators must have at least two
    // parameters.  An associative operator must have exactly two parameters.
    val pars = proto.pars
    if (props.associative && pars.length != 2) {
      throw new IllegalOperatorDefinition(
          "Illegal definition for operator " + proto.toParseString +
          ".  Associative operators must have exactly two parameters.")
    }
    if (props.commutative && pars.length < 2) {
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
    if (props.associative && proto.typ != typ)
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
 * @param proto		The prototype.
 * @param props		Operator properties.
 */
case class SymbolicOperatorDefinition(override val proto: OperatorPrototype,
    props: OperatorProperties) extends OperatorDefinition(proto) {
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
 * @param proto		The prototype.
 * @param body		The operator definition.
 */
case class ImmediateOperatorDefinition(override val proto: OperatorPrototype,
    body: BasicAtom) extends OperatorDefinition(proto) {
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
 * Encapsulate a native operator definition.
 * @param proto		The prototype.
 * @param props		Operator properties.
 */
case class NativeOperatorDefinition(override val proto: OperatorPrototype,
    props: OperatorProperties = Noprops) extends OperatorDefinition(proto) {
  check(proto, props)
  lazy val isConstant = props.isConstant
	def toParseString =
	  "{ native " + proto.toParseString + " " + props.toParseString + " }"
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
case class OperatorPrototype(name: String, pars: IndexedSeq[Variable],
    typ: BasicAtom) {
  def toParseString = name + pars.mkParseString("(", ",", ")") +
  	": " + typ.toParseString

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

/** A "no properties" object. */
object Noprops extends OperatorProperties(false, false, false, None, None)

/**
 * Encapsulate operator properties.
 * @param assoc			True iff associative.  Default is false.
 * @param comm			True iff commutative.  Default is false.
 * @param idem			True iff idempotent.  Default is false.
 * @param absorber	The absorber, if any.  Default is None.
 * @param identity	The identity, if any.  Default is None.
 */
case class OperatorProperties(
    associative: Boolean = false,
    commutative: Boolean = false,
    idempotent: Boolean = false,
    absorber: Option[BasicAtom] = None,
    identity: Option[BasicAtom] = None) {
  
  /**
   * The properties object is constant iff the absorber and identity are
   * constants.
   */
  lazy val isConstant = (absorber, identity) match {
    case (None, None) => true
    case (Some(atom), None) => atom.isConstant
    case (None, Some(atom)) => atom.isConstant
    case (Some(a1), Some(a2)) => a1.isConstant && a2.isConstant
  }
  
  /** Operator properties do not keep around a constant pool. */
  val constantPool = None
  
  // Absorbers, identities, and idempotency are only allowed when an operator
  // is associative.
  if (!associative) {
    if (idempotent) throw new IllegalOperatorDefinition(
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
    if (associative) list += "associative"
    if (commutative) list += "commutative"
    if (idempotent) list += "idempotent"
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
    
  override lazy val hashCode = (((associative.hashCode * 31 + commutative.hashCode) * 31 +
      idempotent.hashCode) * 31 + absorber.hashCode) * 31 + identity.hashCode

  override def equals(other: Any) = other match {
    case op:OperatorProperties =>
      associative == op.associative &&
      commutative == op.commutative &&
      idempotent == op.idempotent &&
      absorber == op.absorber &&
      identity == op.identity
    case _ => false
  }
}

/** Operator properties. */
sealed abstract class OpProperty

/** Indicate that an operator is associative. */
object Associative extends OpProperty

/** Indicate that an operator is commutative. */
object Commutative extends OpProperty

/** Indicate that an operator is idempotent. */
object Idempotent extends OpProperty

/**
 * Indicate that an operator has an identity.
 * 
 * @param id	The identity.
 */
case class Identity(id: BasicAtom) extends OpProperty

/**
 * Indicate that an operator has an absorber.
 * 
 * @param ab	The absorber.
 */
case class Absorber(ab: BasicAtom) extends OpProperty

/** Simple operator properties creation and pattern matching. */
object Props {
  /**
   * Make a new operator properties object by passing the individual properties.
   * 
   * @param properties	The operator properties.
   * @return	The new operator properties object.
   */
  def apply(properties: OpProperty*) = {
    var (assoc, comm, idem) = (false, false, false)
    var absorber: Option[BasicAtom] = None
    var identity: Option[BasicAtom] = None
    for (prop <- properties) prop match {
      case Associative => assoc = true
      case Commutative => comm = true
      case Idempotent => idem = true
      case Identity(id) => identity = Some(id)
      case Absorber(ab) => absorber = Some(ab)
    }
    new OperatorProperties(assoc, comm, idem, absorber, identity)
  }
  
  /**
   * Deconstruct an operator properties object into its components.
   * 
   * @param	properties	The operator properties object.
   * @return	The properties, in order, as associativity, commutativity,
   * 					idempotence, any absorber, and any identity.
   */
  def unapply(properties: OperatorProperties) =
    Some(properties.associative, properties.commutative, properties.idempotent,
        properties.absorber, properties.identity)
}
