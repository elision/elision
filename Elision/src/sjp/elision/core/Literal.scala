/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package sjp.elision.core

import scala.collection.immutable.HashMap

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
 * @param TYPE	The type of data stored in this literal.
 * @param typ		The Elision type of this literal.
 */
abstract class Literal[TYPE](typ: BasicAtom) extends BasicAtom {
  /** The type. */
  val theType = typ
  
  /** Literals are constants. */
  val isConstant = true
  
  /** Literals have no children. */
  val constantPool = None
  
  /** The depth of all literals is zero. */
  val depth = 0
  
  /** The De Bruijn index of all literals is zero. */
  val deBruijnIndex = 0
  
  /** The value stored in this literal. */
  val value:TYPE
  
  /**
   * Two literals match iff their values are equal.
   * 
   * @param subject	The subject.
   * @param binds		Bindings to honor.
   * @return	The match outcome.
   */
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) = subject match {
    case lit: Literal[_] if value == lit.value => Match(binds)
    case _ => Fail("Literal pattern does not match subject.", this, subject)
  }
  
  /**
   * The hash code is computed from the type and the value.
   */
  override lazy val hashCode = theType.hashCode * 31 + value.hashCode
  
  /**
   * Two literals are equal iff their types are equal and their values are
   * equal.
   */
  override def equals(other: Any) = other match {
    case lit: Literal[_] => typ == lit.theType && value == lit.value
    case _ => false
  }
}

/**
 * Provide more convenient ways to construct and extract literals. 
 */
object Literal {
	def apply(value: BigInt): IntegerLiteral = new IntegerLiteral(value)
	def apply(value: Int): IntegerLiteral = new IntegerLiteral(value)
	def apply(value: String): StringLiteral = new StringLiteral(value)
	def apply(value: Symbol): SymbolLiteral = new SymbolLiteral(value)
	def apply(value: Boolean): BooleanLiteral = if (value) TRUE else FALSE
	def apply(significand: BigInt, exponent: Int, radix: Int): FloatLiteral =
	  new FloatLiteral(significand, exponent, radix)
	def apply(typ: BasicAtom, value: BigInt): IntegerLiteral =
	  IntegerLiteral(typ, value)
	def apply(typ: BasicAtom, value: Int): IntegerLiteral =
	  IntegerLiteral(typ, value)
	def apply(typ: BasicAtom, value: String): StringLiteral =
	  StringLiteral(typ, value)
	def apply(typ: BasicAtom, value: Symbol): SymbolLiteral =
	  SymbolLiteral(typ, value)
	def apply(typ: BasicAtom, value: Boolean): BooleanLiteral =
	  BooleanLiteral(typ, value)
	def apply(typ: BasicAtom, significand: BigInt, exponent: Int,
	    radix: Int): FloatLiteral = FloatLiteral(typ, significand, exponent, radix)
	val TRUE = new BooleanLiteral(BOOLEAN, true)
	val FALSE = new BooleanLiteral(BOOLEAN, false)
	val NOTHING = new SymbolLiteral(ANYTYPE, Symbol("Nothing"))
}

/**
 * Provide an integer literal.
 * 
 * @param typ		The type.
 * @param value	The value.
 */
case class IntegerLiteral(typ: BasicAtom, value: BigInt)
extends Literal[BigInt](typ) {
  def this(value: BigInt) = this(INTEGER, value)
  def this(value: Int) = this(INTEGER, value)
  def rewrite(binds: Bindings) = theType.rewrite(binds) match {
	  case (newtype, true) =>
	    (Literal(newtype, value), true)
	  case _ =>
	    (this, false)
	}
  def toParseString = value.toString +
    (if (typ != INTEGER) ":" + typ.toParseString else "") 
}

case class StringLiteral(typ: BasicAtom, value: String)
extends Literal[String](typ) {
  def this(value: String) = this(STRING, value)
  def rewrite(binds: Bindings) = theType.rewrite(binds) match {
	  case (newtype, true) =>
	    (Literal(newtype, value), true)
	  case _ =>
	    (this, false)
	}
  override def toString = "StringLiteral(" + typ + ", " + toEString(value) + ")"
  def toParseString = toEString(value) +
    (if (typ != STRING) ":" + typ.toParseString else "") 
}

case class SymbolLiteral(typ: BasicAtom, value: Symbol)
extends Literal[Symbol](typ) {
  def this(value: Symbol) = this(SYMBOL, value)
  def rewrite(binds: Bindings) = theType.rewrite(binds) match {
	  case (newtype, true) =>
	    (Literal(newtype, value), true)
	  case _ =>
	    (this, false)
	}
  override def toString = "SymbolLiteral(" + typ +
  		", Symbol(" + toEString(value.name) + ")"
  def toParseString = toESymbol(value.name) + ":" + typ.toParseString 
}

case class BooleanLiteral(typ: BasicAtom, value: Boolean)
extends Literal[Boolean](typ) {
  def this(value: Boolean) = this(BOOLEAN, value)
  def rewrite(binds: Bindings) = theType.rewrite(binds) match {
	  case (newtype, true) =>
	    (Literal(newtype, value), true)
	  case _ =>
	    (this, false)
	}
  override def toParseString = value.toString +
    (if (typ != BOOLEAN) ":" + typ.toParseString else "")
}

case class IEEE754(width: Int, significand: Int) {
  require (significand < (width - 2))
  /** The exponent width. */
  lazy val exponent = width - significand - 1
  /** The sign mask. */
  lazy val signMask:Long = 1L << (width - 1)
  /** The exponent mask. */
  lazy val exponentMask = ((1L << exponent) - 1) << significand
  /** The significand mask. */
  lazy val significandMask = (1L << significand) - 1
  /** The exponent bias. */
  lazy val exponentBias = (1L << (exponent - 1)) - 1
  /** The hidden one. */
  lazy val hiddenOne = 1L << significand
}

object IEEE754Quadruple extends IEEE754(128, 112)
object IEEE754Double extends IEEE754(64, 52)
object IEEE754Single extends IEEE754(32, 23)
object IEEE754Half extends IEEE754(16, 10)

case class FloatLiteral(typ: BasicAtom, significand: BigInt, exponent: Int,
    radix: Int) extends Literal[(BigInt, Int, Int)](typ) {
  private lazy val _prefix = radix match {
    case 16 => "0x"
    case 10 => ""
    case 8  => "0"
    case 2  => "0b"
    case _  => require(false, "Invalid radix.")
  }
  private val _sneg = significand < 0
  private val _eneg = exponent < 0
  private val _spos = if (_sneg) -significand else significand
  private val _epos = if (_eneg) -exponent else exponent
  override def toParseString =
    (if (_sneg) "-" else "") + _prefix + _spos.toString(radix) +
    (if (radix == 16) "P" else "e") +
    (if (_eneg) "-" else "") + _prefix + Integer.toString(_epos, radix) +
    (if (typ != FLOAT) ":" + typ.toParseString else "")
  lazy val toFloat = significand.toFloat * scala.math.pow(radix, exponent).toFloat
  lazy val toDouble = significand.toDouble * scala.math.pow(radix, exponent)
  def toIEEE754(plaf: IEEE754 = FloatLiteral.platform) = {
    // Get the representation in bits.
    val bits = java.lang.Double.doubleToRawLongBits(toDouble)
    // Extract the sign.
    val sign = (bits & plaf.signMask) != 0
    // Extract the exponent and adjust for the bias.
    val exponent = ((bits & plaf.exponentMask) >> plaf.significand) -
    	plaf.exponentBias
    // Extract the significand.
    val significand = bits & plaf.significandMask
    // Return the parts.
    (if (sign) 1 else 0, exponent, significand)
  }
  def toUnreducedBinary(plaf: IEEE754 = FloatLiteral.platform) = {
    var (sign, ne, ns) = toIEEE754(plaf)
    // We want to use the significand as an integer, so we need to multiply the
    // significand by 2 to the power of its width.  Since the significand comes
    // to us as an integer already, we just need to adjust the exponent here.
    ne -= plaf.significand
    // If the exponent is the smallest value, then there is no hidden one;
    // otherwise there is.
    ns = ns | (if (ne != -plaf.exponentBias) plaf.hiddenOne else 0)
    // Done!
    FloatLiteral(theType, if (sign != 0) -ns else ns, ne.toInt, 2)
  }
  def toBinary(plaf: IEEE754 = FloatLiteral.platform) = {
    var (sign, ne, ns) = toIEEE754(plaf)
    // We want to use the significand as an integer, so we need to multiply the
    // significand by 2 to the power of its width.  Since the significand comes
    // to us as an integer already, we just need to adjust the exponent here.
    ne -= plaf.significand
    // If the exponent is the smallest value, then there is no hidden one;
    // otherwise there is.
    ns = ns | (if (ne != -plaf.exponentBias) plaf.hiddenOne else 0)
    // Now remove any trailing zeros from the significand.
    while (ns != 0 && (ns % 2) == 0) {
      ns /= 2
      ne += 1
    }
    FloatLiteral(theType, ns, ne.toInt, 2)
  }
  val value = (significand, exponent, radix)
  def this(significand: BigInt, exponent: Int, radix: Int) =
    this(FLOAT, significand, exponent, radix)
  def rewrite(binds: Bindings) = theType.rewrite(binds) match {
	  case (newtype, true) =>
	    (Literal(newtype, significand, exponent, radix), true)
	  case _ =>
	    (this, false)
	}
}

object FloatLiteral {
  var platform: IEEE754 = IEEE754Double
}