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

import scala.collection.mutable.HashMap

/**
 * Literals can be either an integer, a string, or a floating point number.
 * In order to deal with that without having to convert to a string, we
 * build a special case class here that is used to manager the value of the
 * literal.
 */
sealed abstract class LitVal {
  def toParseString: String
}

/**
 * Represent the value of a literal as an integer.
 * @param ival	The integer value.
 */
case class IntVal(ival: BigInt) extends LitVal {
	def toParseString = ival.toString
	override lazy val hashCode = ival.hashCode
	override def equals(other: Any) = other match {
	  case value: IntVal => ival == value.ival
	  case _ => false
	}
}

/**
 * Represent the value of a literal as a string.
 * @param sval	The string value.
 */
case class StrVal(sval: String) extends LitVal {
	def toParseString = toEString(sval)
	override lazy val hashCode = sval.hashCode
	override def equals(other: Any) = other match {
	  case value: StrVal => sval == value.sval
	  case _ => false
	}
}

/**
 * Represent the value of a literal as a Scala symbol.
 * @param sval	The Scala symbol.
 */
case class SymVal(sval: Symbol) extends LitVal {
	def toParseString = toESymbol(sval.name)
	override def toString = "SymVal(Symbol(" + toEString(sval.name) + "))"
	override lazy val hashCode = sval.hashCode
	override def equals(other: Any) = other match {
	  case value: SymVal => sval == value.sval
	  case _ => false
	}
}

/**
 * Represent the value of a floating point number as a significand and exponent,
 * using a specified radix.
 * @param significand		The significand.
 * @param exponent		The exponent.
 * @param radix				The radix.
 */
case class ExpandedFloatVal(significand:BigInt, exponent:Int = 0, radix:Int = 10) 
extends LitVal {
  /** The prefix to use, indicating the known radix. */
  private val _prefix = radix match {
    case 16 => "0x"
    case 10 => ""
    case 8 => "0"
    case 2 => "0b"
    case _ => require(false)
  }
  /** Is the significand negative. */
  private val _mneg = significand < 0
  /** Positive significand.  This avoids a method call. */
  private val _possignificand = if (_mneg) -significand else significand
  /** Is the exponent negative. */
  private val _eneg = exponent < 0
  /** Positive exponent.  This avoids a method call. */
  private val _posexponent = if (_eneg) -exponent else exponent
  
  def toParseString = (if (_mneg) "-" else "") + _prefix +
  	_possignificand.toString(radix) +
  	(if (radix == 16) "P" else "e") +
  	(if (_eneg) "-" else "") + _prefix + Integer.toString(_posexponent, radix)
  	
  /**
   * Get a simple native floating point representation of this number.
   */
  def toFloat = significand * BigInt(radix).pow(exponent)
  
  override lazy val hashCode = (significand.hashCode * 31 +
  	exponent.hashCode) * 31 + radix.hashCode
  	
  override def equals(other: Any) = other match {
    case value:ExpandedFloatVal =>
      value.significand == significand &&
      value.exponent == exponent &&
      value.radix == radix
    case _ => false
  }
}

/**
 * Represent the value of a floating point number.
 * @param fval	The floating point value.
 */
case class FltVal(fval: Float) extends LitVal {
	def toParseString = fval.toString
	override lazy val hashCode = fval.hashCode
	override def equals(other: Any) = other match {
	  case value:FltVal => fval == value.fval
	  case _ => false
	}
}

/**
 * Represent the value of a Boolean.
 * @param bool	The boolean value.
 */
case class BooVal(bool: Boolean) extends LitVal {
	def toParseString = bool.toString
	override lazy val hashCode = bool.hashCode
	override def equals(other: Any) = other match {
	  case value:BooVal => bool == value.bool
	  case _ => false
	}
}

/**
 * Represent a literal.
 * 
 * ==Structure and Syntax==
 * 
 * ==Type==
 * 
 * ==Equality and Matching==
 * 
 * @param typ		The type of the literal.
 * @param value	The value for the literal.
 */
case class Literal(typ: BasicAtom, value: LitVal) extends BasicAtom {
	val theType = typ
	val deBrujinIndex = 0
	val isConstant = true

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

	def rewrite(binds: Bindings) = {
		// Even though literals cannot be rewritten, there is a chance their type
		// can be rewritten, so check that.
	  val (newtype, changed) = theType.rewrite(binds)
		if (changed) (Literal(newtype, value), true) else (this, false)
	}

	override def toParseString = value.toParseString + ":" + theType.toParseString
	
	override lazy val hashCode = 31 * theType.hashCode + value.hashCode
	
	override def equals(other: Any) = other match {
	  case lit:Literal =>
	    typ == lit.typ &&
	    value == lit.value
	  case _ => false
	}
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
	def apply(typ: BasicAtom, ival: BigInt) = new Literal(typ, IntVal(ival))
	
	/**
	 * Make a floating point value.  The value represented is equal to
	 * significand * scala.math.pow(exponent, radix).
	 * @param typ				The type.
	 * @param significand	The significand.
	 * @param exponent	The exponent.
	 * @param radix			The radix.
	 */
	def apply(typ: BasicAtom, significand: BigInt, exponent: Int, radix: Int) =
	  new Literal(typ, ExpandedFloatVal(significand, exponent, radix))

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
