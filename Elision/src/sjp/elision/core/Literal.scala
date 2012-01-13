/* Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 */
package sjp.elision.core

import scala.collection.mutable.HashMap

/**
 * Literals can be either an integer, a string, or a floating point number.
 * In order to accomodate that without having to convert to a string, we
 * build a special case class here that is used to manager the value of the
 * literal.
 */
sealed abstract class LitVal

/**
 * Represent the value of a literal as an integer.
 * @param ival	The interger value.
 */
case class IntVal(ival: Int) extends LitVal {
	override def toString = ival.toString
}

/**
 * Represent the value of a literal as a string.
 * @param sval	The string value.
 */
case class StrVal(sval: String) extends LitVal {
	override def toString = toEString(sval)
}

/**
 * Represent the value of a literal as a Scala symbol.
 * @param sval	The Scala symbol.
 */
case class SymVal(sval: Symbol) extends LitVal {
	override def toString = toESymbol(sval.name)
}

/**
 * Represent the value of a floating point number as a mantissa and exponent,
 * using a specified radix.
 * @param mantissa		The mantissa.
 * @param exponent		The exponent.
 * @param radix				The radix.
 */
case class ExpandedFloatVal(mantissa:Int, exponent:Int = 1, radix:Int = 10) 
extends LitVal {
  /** The prefix to use, indicating the known radix. */
  private val _prefix = radix match {
    case 16 => "0x"
    case 10 => ""
    case 8 => "0"
    case 2 => "0b"
    case _ => require(false)
  }
  /** Is the mantissa negative. */
  private val _mneg = mantissa < 0
  /** Positive mantissa.  This avoids a method call. */
  private val _posmantissa = if (_mneg) -mantissa else mantissa
  /** Is the exponent negative. */
  private val _eneg = exponent < 0
  /** Positive exponent.  This avoids a method call. */
  private val _posexponent = if (_eneg) -exponent else exponent
  
  override def toString = (if (_mneg) "-" else "") + _prefix +
  	Integer.toString(_posmantissa, radix) + "P" +
  	(if (_eneg) "-" else "") + _prefix + Integer.toString(_posexponent, radix)
  	
  /**
   * Get a simple native floating point representation of this number.
   */
  def toFloat = mantissa * scala.math.pow(radix, exponent)
}

/**
 * Represent the value of a floating point number.
 * @param fval	The floating point value.
 */
case class FltVal(fval: Float) extends LitVal {
	override def toString = fval.toString
}

/**
 * Represent the value of a Boolean.
 * @param bool	The boolean value.
 */
case class BooVal(bool: Boolean) extends LitVal {
	override def toString = bool.toString
}

/**
 * Represent a literal.
 * @param typ		The type of the literal.
 * @param value	The value for the literal.
 */
case class Literal(typ: BasicAtom, value: LitVal) extends BasicAtom {
	val theType = typ

	override val isTrue = value match {
	case BooVal(true) => true
	case _ => false
	}

	override val isFalse = value match {
	case BooVal(false) => true
	case _ => false
	}

	def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
		subject match {
		case Literal(_, ovalue) if ovalue == value => Match(binds)
		case _ => Fail("Literals do not match.", this, subject)
	}

	def rewrite(binds: Bindings) =
		// Even though literals cannot be rewritten, there is a chance their type
		// can be rewritten, so check that.
		theType.rewrite(binds) match {
		case (newtype, changed) =>
		if (changed) (Literal(theType, value), true) else (this, false)
		case _ => (this, false)
	}

	override def toString = value.toString + ":" + theType.toString
}

/**
 * Extend the literal object to add some convenient constructors.
 */
object Literal {
	/**
	 * Make a string value.
	 * @param typ		The type.
	 * @param sval	The string value.
	 */
	def apply(typ: BasicAtom, sval: String) = new Literal(typ, StrVal(sval))

	/**
	 * Make a symbol value.
	 * @param typ		The type.
	 * @param sval	The symbol value.
	 */
	def apply(typ: BasicAtom, sval: Symbol) = new Literal(typ, SymVal(sval))

	/**
	 * Make a integer value.
	 * @param typ		The type.
	 * @param ival	The integer value.
	 */
	def apply(typ: BasicAtom, ival: Int) = new Literal(typ, IntVal(ival))
	
	/**
	 * Make a floating point value.  The value represented is equal to
	 * mantissa * scala.math.pow(exponent, radix).
	 * @param typ				The type.
	 * @param mantissa	The mantissa.
	 * @param exponent	The exponent.
	 * @param radix			The radix.
	 */
	def apply(typ: BasicAtom, mantissa: Int, exponent: Int, radix: Int) =
	  new Literal(typ, ExpandedFloatVal(mantissa, exponent, radix))

	/**
	 * Make a float value.
	 * @param typ		The type.
	 * @param fval	The float value.
	 */
	def apply(typ: BasicAtom, fval: Float) = new Literal(typ, FltVal(fval))

	/**
	 * Make a Boolean value.
	 * @param typ		The type.
	 * @param bool	The Boolean value.
	 */
	def apply(typ: BasicAtom, bool: Boolean) = new Literal(typ, BooVal(bool))
}

// Make some well-known types.
object STRING extends Literal(TypeUniverse, SymVal('STRING))
object SYMBOL extends Literal(TypeUniverse, SymVal('SYMBOL))
object INTEGER extends Literal(TypeUniverse, SymVal('INTEGER))
object FLOAT extends Literal(TypeUniverse, SymVal('FLOAT))
object BOOLEAN extends Literal(TypeUniverse, SymVal('BOOLEAN))
