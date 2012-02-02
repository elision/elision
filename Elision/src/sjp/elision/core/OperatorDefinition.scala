/* Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 */
package sjp.elision.core
import scala.collection.mutable.LinkedList
import scala.collection.mutable.ListBuffer

/**
 * Common root class for an operator definition.
 */
abstract class OperatorDefinition(val proto: OperatorPrototype)
extends BasicAtom {
  val theType = TypeUniverse
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    Fail("Operator definition matching is not implemented.", this, subject)
	def rewrite(binds: Bindings) = (this, false)
}

/**
 * Encapsulate an operator definition.
 * @param proto		The prototype.
 * @param props		Operator properties.
 */
case class SymbolicOperatorDefinition(override val proto: OperatorPrototype,
    props: OperatorProperties) extends OperatorDefinition(proto) {
	def toParseString =
	  "operator { " + proto.toParseString + " " + props.toParseString + " }"
}

/**
 * Encapsulate an immediate operator definition.
 * @param proto		The prototype.
 * @param body		The operator definition.
 */
case class ImmediateOperatorDefinition(override val proto: OperatorPrototype,
    body: BasicAtom) extends OperatorDefinition(proto) {
	def toParseString =
	  "operator { " + proto.toParseString + " = " + body.toParseString + " }"
}

/**
 * Encapsulate a native operator definition.
 * @param proto		The prototype.
 * @param props		Operator properties.
 */
case class NativeOperatorDefinition(override val proto: OperatorPrototype,
    props: OperatorProperties) extends OperatorDefinition(proto) {
	def toParseString =
	  "native { " + proto.toParseString + " " + props.toParseString + " }"
}

/**
 * The operator prototype.
 * @param name			The operator name.
 * @param typepars	The type parameters.
 * @param pars			The parameters.
 * @param typ				The type.
 */
case class OperatorPrototype(name: String, typepars: List[Variable],
    pars: List[Variable], typ: BasicAtom) {
  def toParseString = name +
  	(if (!typepars.isEmpty) typepars.mkParseString("[", ",", "]") else "") +
  	pars.mkParseString("(", ",", ")") +
  	": " + typ.toParseString
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
}
