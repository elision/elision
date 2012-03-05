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
 * Literals can be an integer, a string, or a floating point number.
 * 
 * In order to deal with different kinds of literals without having to convert
 * to a string, we build a special case class here that is used to manage the
 * value of the literal.
 */
sealed abstract class LitVal {
  /**
   * Every literal value must provide a parse string.  This is required here
   * since each kind of literal has a different representation.
   * 
   * @return A string that is parseable to re-create the literal.
   */
  def toParseString: String
}

/**
 * Represent the value of an integer literal.
 * 
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
 * Represent the value of a string literal.
 * 
 * @param sval	The string value.
 */
case class StrVal(sval: String) extends LitVal {
	def toParseString = toEString(sval)
	override def toString = toEString(sval)
	override lazy val hashCode = sval.hashCode
	override def equals(other: Any) = other match {
	  case value: StrVal => sval == value.sval
	  case _ => false
	}
}

/**
 * Represent the value of a literal as a Scala symbol.
 * 
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
 * using a specified radix.  The radix is limited; you ''must'' use a radix of
 * 2, 8, 10, or 16.  Otherwise you will generate an exception here.
 * 
 * @param significand		The significand.
 * @param exponent		The exponent.  This is zero by default.
 * @param radix				The radix.  This is ten by default.
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
  
  /**
   * Whether the significand is negative.  This is used to build the parse
   * string, since the sign has to go before the prefix.
   */
  private val _mneg = significand < 0
  
  /**
   * Positive significand.  This avoids a method call.
   */
  private val _possignificand = if (_mneg) -significand else significand
  
  /**
   * Whether the exponent is negative.  This is used to build the parse
   * string, since the sign has to go before the prefix.
   */
  private val _eneg = exponent < 0
  
  /**
   * Positive exponent.  This avoids a method call.
   */
  private val _posexponent = if (_eneg) -exponent else exponent
  
  def toParseString = (if (_mneg) "-" else "") + _prefix +
  	_possignificand.toString(radix) +
  	(if (radix == 16) "P" else "e") +
  	(if (_eneg) "-" else "") + _prefix + Integer.toString(_posexponent, radix)
  	
  /**
   * Get a simple native floating point representation of this number.
   * 
   * '''WARNING''': This is potentially lossy.  Certain floating point numbers
   * may not have a terminating representation in a different radix.  Further,
   * there is no limit on the significand (it is a `BigInt`), while there is a
   * fixed limit on the precision of the significand here. 
   */
  def toFloat = significand * BigInt(radix).pow(exponent)
  
  override lazy val hashCode = (significand.hashCode * 31 +
  	exponent.hashCode) * 31 + radix.hashCode
  	
  override def equals(other: Any) = other match {
    // Two extended floating point numbers are equal iff their significands,
    // exponents, and radices are equal.  If you require a "fuzzier" version
    // of equals, such as numeric equality, use `toFloat` and compare.
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
case class FltVal(fval: Double) extends LitVal {
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
case class BooVal (val bool: Boolean) extends LitVal {
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
 * Literals come in the following forms.
 *  - An '''integer''' of arbitrary precision, and one of the following radices:
 *    - Binary, indicated with a prefix of `0b`
 *    - Octal, indicated with a prefix of `0`
 *    - Decimal, indicated by starting the number with any non-zero digit.
 *    - Hexadecimal, indicated with a prefix of `0x`
 *    A negative sign (`-`) may precede the prefix.  Any number of appropriate
 *    digits may follow.  Numbers are case-insensitive, both for the prefix and
 *    digits.
 *   
 *  - A '''floating point number''' consisting of a ''significand'' and an
 *    ''exponent''.  Both of these are integers, as described above, with sign
 *    and radix.  The radix of the exponent is significant, and the significand
 *    is converted to the same radix as the exponent.  Let \(s\) denote the
 *    significand, let \(x\) denote the exponent, and let \(r\) denote the
 *    radix.  Then the ''value'' \(v\) of the number is \(v = s\times r^e\).
 *   
 *  - A '''string''' consisting of any sequence of characters enclosed in
 *    double quotation marks.  If a double quotation mark is to be included in
 *    the string, it must be escaped in the usual C manner.  See
 *    [[sjp.elision.core.toEString]] for more details on the escapes that are
 *    interpreted.
 *   
 *  - A '''symbol''' that can be in either of two forms.
 *    - An initial letter or underscore, followed by any number of letters,
 *      underscores, and digits.
 *    - Any sequence of characters enclosed in backticks (<code>`</code>).
 *      If a backtick is present in the symbol value, it must be escaped in
 *      the usual C manner (<code>\`</code>).
 *     
 *  - A '''Boolean''' value that may be either `true` or `false`.
 * 
 * ==Type==
 * Literals have the following types by default.
 *  - Integers are of type INTEGER.
 *  - Floating point numbers are of type FLOAT.
 *  - String values are of type STRING.
 *  - Symbol values are of type SYMBOL.
 *  - Boolean values are of type BOOLEAN.
 * The type can be supplied at construction time to override these choices
 * with any other type.
 * 
 * ==Equality and Matching==
 * Two instances are equal iff their types and values are equal.  Literals
 * match iff their types match and their values match.
 * 
 * @param typ		The type of the literal.
 * @param value	The value for the literal.
 */
case class Literal(typ: BasicAtom, value: LitVal) extends BasicAtom {
	val theType = typ
	
	/** The De Bruijn index is zero. */
	val deBruijnIndex = 0
	
	/** Literals are constant. */
	val isConstant = true
	
	/** The depth of all literals is zero. */
	val depth = 0
  
  /** Literals contain no constant "children". */
  val constantPool = None

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
	 * 
	 * @param typ		The type.
	 * @param sval	The string value.
	 */
	def apply(typ: BasicAtom, sval: String) = new Literal(typ, StrVal(sval))

	/**
	 * Make a symbol value.
	 * 
	 * @param typ		The type.
	 * @param sval	The symbol value.
	 */
	def apply(typ: BasicAtom, sval: Symbol) = new Literal(typ, SymVal(sval))

	/**
	 * Make a integer value.
	 * 
	 * @param typ		The type.
	 * @param ival	The integer value.
	 */
	def apply(typ: BasicAtom, ival: Int): Literal =
	  new Literal(typ, IntVal(ival))

	/**
	 * Make a integer value.
	 * 
	 * @param typ		The type.
	 * @param ival	The integer value.
	 */
	def apply(typ: BasicAtom, ival: BigInt): Literal =
	  new Literal(typ, IntVal(ival))
	
	/**
	 * Make a floating point value.  The value represented is equal to
	 * significand * scala.math.pow(exponent, radix).
	 * 
	 * @param typ					The type.
	 * @param significand	The significand.
	 * @param exponent		The exponent.
	 * @param radix				The radix.
	 */
	def apply(typ: BasicAtom, significand: BigInt, exponent: Int, radix: Int) =
	  new Literal(typ, ExpandedFloatVal(significand, exponent, radix))

	/**
	 * Make a float value.
	 * 
	 * @param typ		The type.
	 * @param fval	The float value.
	 */
	def apply(typ: BasicAtom, fval: Double) = new Literal(typ, FltVal(fval))
	
	/** The value true. */
	val TRUE = new Literal(BOOLEAN, BooVal(true))
	
	/** The value false. */
	val FALSE = new Literal(BOOLEAN, BooVal(false))
	
	/** The special Nothing value. */
	val NOTHING = new Literal(ANYTYPE, SymVal(Symbol("Nothing")))

	/**
	 * Make a Boolean value.
	 * 
	 * @param typ		The type.
	 * @param bool	The Boolean value.
	 */
	def apply(typ: BasicAtom, bool: Boolean) = if (bool) TRUE else FALSE
}
