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

abstract class OperatorDefinition extends BasicAtom {
  val theType = TypeUniverse
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    Fail("Operator definition matching is not implemented.", this, subject)
	def rewrite(binds: Bindings) = (this, false)
}

/**
 * Encapsulate an operator definition.
 */
case class SymbolicOperatorDefinition(proto: OperatorPrototype,
    props: OperatorProperties) extends OperatorDefinition {
	def toParseString =
	  "operator { " + proto.toParseString + " " + props.toParseString + " }"
}

case class ImmediateOperatorDefinition(proto: OperatorPrototype,
    body: BasicAtom) extends OperatorDefinition {
	def toParseString =
	  "operator { " + proto.toParseString + " = " + body.toParseString + " }"
}

case class NativeOperatorDefinition(proto: OperatorPrototype,
    props: OperatorProperties) extends OperatorDefinition {
	def toParseString =
	  "native { " + proto.toParseString + " " + props.toParseString + " }"
}

case class OperatorPrototype(name: String, typepars: List[Variable],
    pars: List[Variable], typ: BasicAtom) {
  def toParseString = name +
  	(if (!typepars.isEmpty) typepars.mkParseString("[", ",", "]")) +
  	pars.mkParseString("(", ",", ")") +
  	": " + typ.toParseString
}

case class OperatorProperties(assoc: Boolean, comm: Boolean, idem: Boolean,
    absorber: Option[BasicAtom], identity: Option[BasicAtom]) {
  lazy val propstr = {
    var list = new LinkedList
    if (assoc) list :+ "associative"
    if (comm) list :+ "commutative"
    if (idem) list :+ "idempotent"
    absorber match {
      case Some(atom) => list :+ atom.toParseString
      case None =>
    }
    identity match {
      case Some(atom) => list :+ atom.toParseString
      case None =>
    }
    list.mkParseString("", " ", "")
  }
  def toParseString = if (propstr.length > 0) "is " + propstr else ""
}
