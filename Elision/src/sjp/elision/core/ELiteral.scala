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
abstract class ELiteral[TYPE](typ: BasicAtom) extends BasicAtom {
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
  
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    subject match {
    case lit: ELiteral[_] if value == lit.value => Match(binds)
    case _ => Fail("Literal pattern does not match subject.", this, subject)
  }
  def toParseString = value + ":" + theType.toParseString
  override lazy val hashCode = theType.hashCode * 31 + value.hashCode
  override def equals(other: Any) = other match {
    case lit: ELiteral[_] => typ == lit.theType && value == lit.value
    case _ => false
  }
}

object ELiteral {
	def apply(value: BigInt) = new IntegerLiteral(value)
	def apply(value: Int) = new IntegerLiteral(value)
	def apply(value: String) = new StringLiteral(value)
	def apply(value: Symbol) = new SymbolLiteral(value)
	def apply(value: Boolean) = if (value) TRUE else FALSE
	def apply(significand: BigInt, exponent: Int, radix: Int) =
	  new FloatLiteral(significand, exponent, radix)
	def apply(typ: BasicAtom, value: BigInt) = IntegerLiteral(typ, value)
	def apply(typ: BasicAtom, value: Int) = IntegerLiteral(typ, value)
	def apply(typ: BasicAtom, value: String) = StringLiteral(typ, value)
	def apply(typ: BasicAtom, value: Symbol) = SymbolLiteral(typ, value)
	def apply(typ: BasicAtom, value: Boolean) = BooleanLiteral(typ, value)
	def apply(typ: BasicAtom, significand: BigInt, exponent: Int, radix: Int) =
	  FloatLiteral(typ, significand, exponent, radix)
	def unapply(lit: IntegerLiteral) = Some(lit.typ, lit.value)
	def unapply(lit: StringLiteral) = Some(lit.typ, lit.value)
	def unapply(lit: SymbolLiteral) = Some(lit.typ, lit.value)
	def unapply(lit: BooleanLiteral) = Some(lit.typ, lit.value)
	def unapply(lit: FloatLiteral) = Some(lit.significand, lit.exponent, lit.radix)
	val TRUE = new BooleanLiteral(BOOLEAN, true)
	val FALSE = new BooleanLiteral(BOOLEAN, false)
	val NOTHING = new SymbolLiteral(ANYTYPE, Symbol("Nothing"))
}

case class IntegerLiteral(typ: BasicAtom, value: BigInt)
extends ELiteral[BigInt](typ) {
  def this(value: BigInt) = this(INTEGER, value)
  def this(value: Int) = this(INTEGER, value)
  def rewrite(binds: Bindings) = theType.rewrite(binds) match {
	  case (newtype, true) =>
	    (ELiteral(newtype, value), true)
	  case _ =>
	    (this, false)
	}
}

case class StringLiteral(typ: BasicAtom, value: String)
extends ELiteral[String](typ) {
  def this(value: String) = this(STRING, value)
  def rewrite(binds: Bindings) = theType.rewrite(binds) match {
	  case (newtype, true) =>
	    (ELiteral(newtype, value), true)
	  case _ =>
	    (this, false)
	}
}

case class SymbolLiteral(typ: BasicAtom, value: Symbol)
extends ELiteral[Symbol](typ) {
  def this(value: Symbol) = this(SYMBOL, value)
  def rewrite(binds: Bindings) = theType.rewrite(binds) match {
	  case (newtype, true) =>
	    (ELiteral(newtype, value), true)
	  case _ =>
	    (this, false)
	}
}

case class BooleanLiteral(typ: BasicAtom, value: Boolean)
extends ELiteral[Boolean](typ) {
  def this(value: Boolean) = this(BOOLEAN, value)
  def rewrite(binds: Bindings) = theType.rewrite(binds) match {
	  case (newtype, true) =>
	    (ELiteral(newtype, value), true)
	  case _ =>
	    (this, false)
	}
}

case class FloatLiteral(typ: BasicAtom, significand: BigInt, exponent: Int,
    radix: Int) extends ELiteral[(BigInt, Int, Int)](typ) {
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
    (if (_eneg) "-" else "") + _prefix + Integer.toString(_epos, radix)
  def toFloat = significand.toDouble * Math.pow(radix, exponent)
  val value = (significand, exponent, radix)
  def this(significand: BigInt, exponent: Int, radix: Int) =
    this(FLOAT, significand, exponent, radix)
  def rewrite(binds: Bindings) = theType.rewrite(binds) match {
	  case (newtype, true) =>
	    (ELiteral(newtype, significand, exponent, radix), true)
	  case _ =>
	    (this, false)
	}
}
