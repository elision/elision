/* Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 */
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
  val deBrujinIndex = 0

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    subject match {
      case Operator(oopdef) if opdef == oopdef => Match(binds)
      case _ => Fail("Operators do not match.", this, subject)
    }

  def rewrite(binds: Bindings) = (this, false)

  def toParseString = toESymbol(opdef.proto.name)
  
  def apply(arg: BasicAtom) = arg match {
    case AtomList(atoms) =>
      // TODO We need to check everything.
      Apply(this, arg, true)
    case _ => Apply(this, arg, true)
  }
}