/*======================================================================
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 * The Elision Term Rewriter
 * 
 * Copyright (c) 2012 by UT-Battelle, LLC.
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
 * Collection of administrative costs for redistribution of the source code or
 * binary form is allowed. However, collection of a royalty or other fee in excess
 * of good faith amount for cost recovery for such redistribution is prohibited.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER, THE DOE, OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
======================================================================
* */
package ornl.elision.core

import scala.collection.immutable.HashMap
import ornl.elision.util.other_hashify

/**
 * Represent a literal.  This is the common root class for all literals.
 * See the companion object for construction and matching of literals.
 * 
 * == Structure and Syntax ==
 * Literals come in the following forms.
 *  - An '''integer''' of arbitrary precision, and one of the following radices:
 *    - Binary, indicated with a prefix of `0b`
 *    - Octal, indicated with a prefix of `0o`
 *    - Decimal, indicated with no special prefix.
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
 *  - A '''bit string''' consisting of a defined, fixed-length sequence of
 *    bits.  These are indicated by suffixing an integer with an L (case is
 *    not significant) followed by a second integer literal that specifies the
 *    number of bits.  For example, 0x4EL8 specifies a length 8 bit string
 *    whose value is 01001110.
 *   
 *  - A '''string''' consisting of any sequence of characters enclosed in
 *    double quotation marks.  If a double quotation mark is to be included in
 *    the string, it must be escaped in the usual C manner.  See
 *    [[ornl.elision.core.toEString]] for more details on the escapes that are
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
 * == Type ==
 * Literals have the following types by default.
 *  - Integers are of type `INTEGER`.
 *  - Floating point numbers are of type `FLOAT`.
 *  - Bit string values are of type `BITSTRING`.
 *  - String values are of type `STRING`.
 *  - Symbol values are of type `SYMBOL`.
 *  - Boolean values are of type `BOOLEAN`.
 * The type can be supplied at construction time to override these choices
 * with any other type.
 * 
 * == Equality and Matching ==
 * Two instances are equal iff their types and values are equal.  Literals
 * match iff their types match and their values match.
 * 
 * @param TYPE	The type of data stored in this literal.
 * @param typ		The Elision type of this literal.
 * @param mTYPE	The manifest for `TYPE`.
 */
abstract sealed class Literal[TYPE](typ: BasicAtom)(implicit mTYPE: Manifest[TYPE])
extends BasicAtom {
  
  /** The type. */
  val theType = typ
  
  /** Literals are constants. */
  val isConstant = true
  
  /** Literals are terms. */
  val isTerm = true
  
  /** The depth of all literals is zero. */
  val depth = 0
  
  /** The De Bruijn index of all literals is zero. */
  val deBruijnIndex = 0
  
  /** The value stored in this literal. */
  val value:TYPE
  
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) = subject match {
    case lit: Literal[_] if value == lit.value => Match(binds)
    case _ => Fail("Literal pattern does not match subject.", this, subject)
  }
  
  /**
   * The hash code is computed from the type and the value.
   */
  override lazy val hashCode = theType.hashCode * 31 + value.hashCode
  lazy val otherHashCode = theType.otherHashCode +
    8191*(value.toString).foldLeft(BigInt(0))(other_hashify)
  
  /**
   * Two literals are equal iff their types are equal and their values are
   * equal.
   */
  override def equals(other: Any) = other match {
    case lit: Literal[_] =>
      feq(lit, this, typ == lit.theType && value == lit.value)
      
    case _ =>
      false
  }
}

/**
 * Provide more convenient ways to construct and extract literals.
 */
object Literal {
  /** Make an integer literal from a big integer. */
  def apply(value: BigInt): IntegerLiteral = new IntegerLiteral(value)
  /** Make an integer literal from a Scala integer value. */
  def apply(value: Int): IntegerLiteral = new IntegerLiteral(value)
  /** Make a bit string literal from two Scala integer values. */
  def apply(bits: BigInt, len: Int): BitStringLiteral =
    new BitStringLiteral(bits, len)
  /** Make a string literal from a Scala string value. */
  def apply(value: String): StringLiteral = new StringLiteral(value)
  /** Make a symbol literal from a Scala symbol value. */
  def apply(value: Symbol): SymbolLiteral = new SymbolLiteral(value)
  /**
   * Get the appropriate Boolean literal for a Scala Boolean value.
   */
  def apply(value: Boolean): BooleanLiteral = if (value) TRUE else FALSE
  /**
   * Make a floating point literal.
   * 
   * @param significand		The significand.
   * @param exponent			The exponent.
   * @param radix					The radix, which must be 2, 8, 10, or 16.
   */
  def apply(significand: BigInt, exponent: Int, radix: Int): FloatLiteral =
    new FloatLiteral(significand, exponent, radix)
  /** Make an integer literal from a big integer, and override the type. */
  def apply(typ: BasicAtom, value: BigInt): IntegerLiteral =
    IntegerLiteral(typ, value)
  /** Make an integer literal from a Scala integer value and override the type. */
  def apply(typ: BasicAtom, value: Int): IntegerLiteral =
    IntegerLiteral(typ, value)
  /** Make an string literal from a Scala string value and override the type. */
  def apply(typ: BasicAtom, value: String): StringLiteral =
    StringLiteral(typ, value)
  /** Make an symbol literal from a Scala symbol value and override the type. */
  def apply(typ: BasicAtom, value: Symbol): SymbolLiteral =
    SymbolLiteral(typ, value)
  /**
   * Get the appropriate Boolean literal for a Scala Boolean value, and
   * override the type.
   */
  def apply(typ: BasicAtom, value: Boolean): BooleanLiteral =
    BooleanLiteral(typ, value)
  /** Boolean true literal. */
  val TRUE = new BooleanLiteral(BOOLEAN, true)
  /** Boolean false literal. */
  val FALSE = new BooleanLiteral(BOOLEAN, false)  
  /**
   * Make a floating point literal, and override the default type.
   *
   * @param typ						The type to use.
   * @param significand		The significand.
   * @param exponent			The exponent.
   * @param radix					The radix, which must be 2, 8, 10, or 16.
   */
  def apply(typ: BasicAtom, significand: BigInt, exponent: Int,
	    radix: Int): FloatLiteral = FloatLiteral(typ, significand, exponent, radix)
	/** Make a bit string literal from two Scala integers and override the type. */
	def apply(typ: BasicAtom, bits: BigInt, len: Int): BitStringLiteral =
	  new BitStringLiteral(typ, bits, len)
}

/**
 * Provide an integer literal.  Integer literals are backed by the Scala
 * `BigInt` type, so they can contain arbitrarily large values.
 * 
 * @param typ		The type.
 * @param value	The value.
 */
case class IntegerLiteral(typ: BasicAtom, value: BigInt)
extends Literal[BigInt](typ) {
  /**
   * Alternate constructor with default `INTEGER` type.
   */
  def this(value: BigInt) = this(INTEGER, value)
  /**
   * Alternate constructor with default `INTEGER` type.
   */
  def this(value: Int) = this(INTEGER, value)
  
  def rewrite(binds: Bindings) = {
		theType.rewrite(binds) match {
		  case (newtype, true) =>
        (Literal(newtype, value), true)
		  case _ =>
        (this, false)
		}
	}
  
  def replace(map: Map[BasicAtom, BasicAtom]) = {
    map.get(this) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = theType.replace(map)
        if (flag) {
          (IntegerLiteral(newtype, value), true)
        } else {
          (this, false)
        }
    }
  }
}

/**
 * Provide an bit string literal.  Bit string literals are backed by the Scala
 * `BigInt` type, so they can contain arbitrarily large values.
 * 
 * @param typ   The type.
 * @param bits  The bits.
 * @param len   The length.
 */
case class BitStringLiteral(typ: BasicAtom, var bits: BigInt, len: Int)
extends Literal[(BigInt, Int)](typ) {
  /** If true, prefer to display this as a signed value.  If false, do not. */
  val neghint = (bits < 0)

  // Figure out the minimum number of bits required to hold the base.
  if (neghint) {
    bits = BigInt(2).pow(len) + bits
  }
  private val _bbl = bits.bitLength
  if (len < _bbl) {
    // Truncate the base to obtain the new base.
    bits = bits & (BigInt(2).pow(len)-1)
  } else if (len == 0) {
    // Just return the well-known zero.
    bits = 0
  }
  
  /** Value as a pair of bits and length. */
  val value = (bits, len)
  
  /** Return the bits as an unsigned integer. */
  val unsigned = bits.abs
  
  /** Interpret the bits as a signed integer. */
  val signed = {
    /* About Signed Integers in 2's Complement Arithmetic
     * 
     * You have N bits.  If the highest-order bit is zero, your number is
     * positive.  If it is one, your number is negative.  In this latter case
     * we have to do a bit of interpretation.
     * 
     * The two's complement of a number X in N bits is the result of
     * subtracting that number from 2^N, or 2^N - X.  In two's complement
     * arithmetic the negation of a number is its two's complement.
     * 
     * Consider 7 in 4 bits.  That's 0b0111L4.  Now we consider -7.  To
     * get that we subtract from 2^4 = 16.  16 - 7 = 9, or 0b1001L4.  Another
     * (simpler?) way to think of this is that we first take the one's
     * complement, obtaining 0b1000L4, and then add one, to obtain 0b1001L4.
     * 
     * So, to interpret our number as a signed integer we first check whether
     * it is negative by testing the highest-order bit.  If zero, we just
     * return the unsigned result.  If one, we do a tiny bit of math.  We are
     * given V, but we want X, the original number that was negated.
     * 
     *   2^N - X = V
     *   2^N - X - V = 0
     *   2^N - V = X
     * 
     * Ah ha!  Two's complement works just like a "good" complement should.
     * We can get it by flipping the bits and adding one (negation is negation)
     * or by subtracting from 2^N.  Here we choose the latter.
     */
    if (len > 0 && bits.testBit(len-1)) {
      // Number is negative.  Complement it and return the Scala negation.
      -(BigInt(2).pow(len) - bits)
    } else {
      // Number is positive.  Return its unsigned value.
      unsigned
    }
  }
  
  /**
   * Alternate constructor with default `BITSTRING` type.
   */
  def this(bits: BigInt, len: Int) = this(BITSTRING, bits, len)
  
  def rewrite(binds: Bindings) = {
    theType.rewrite(binds) match {
      case (newtype, true) =>
        (Literal(newtype, unsigned), true)
      case _ =>
        (this, false)
    }
  }
  
  def replace(map: Map[BasicAtom, BasicAtom]) = {
    map.get(this) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = theType.replace(map)
        if (flag) {
          (IntegerLiteral(newtype, unsigned), true)
        } else {
          (this, false)
        }
    }
  }
}

/**
 * Provide a string literal.  String literals are backed by the `String`
 * type.
 */
case class StringLiteral(typ: BasicAtom, value: String)
extends Literal[String](typ) {
  /**
   * Alternate constructor with default `STRING` type.
   */
  def this(value: String) = this(STRING, value)
  
  def rewrite(binds: Bindings) = {
		theType.rewrite(binds) match {
		  case (newtype, true) =>
        (Literal(newtype, value), true)
		  case _ =>
        (this, false)
		}
	}
  
  def replace(map: Map[BasicAtom, BasicAtom]) = {
    map.get(this) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = theType.replace(map)
        if (flag) {
          (StringLiteral(newtype, value), true)
        } else {
          (this, false)
        }
    }
  }
}

/**
 * Provide a symbol literal.  Symbol literals are backed by the Scala
 * `Symbol` type.
 */
case class SymbolLiteral(typ: BasicAtom, value: Symbol)
extends Literal[Symbol](typ) {
  /**
   * Alternate constructor with default `SYMBOL` type.
   */
  def this(value: Symbol) = this(SYMBOL, value)
  
  override lazy val otherHashCode = (value.toString).foldLeft(BigInt(0))(other_hashify)

  def rewrite(binds: Bindings) = {
		theType.rewrite(binds) match {
		  case (newtype, true) =>
        (Literal(newtype, value), true)
		  case _ =>
        (this, false)
		}
	}
  
  def replace(map: Map[BasicAtom, BasicAtom]) = {
    map.get(this) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = theType.replace(map)
        if (flag) {
          (SymbolLiteral(newtype, value), true)
        } else {
          (this, false)
        }
    }
  }
}

/**
 * Provide a Boolean literal.  Boolean literals are backed by the
 * `Boolean` type.  '''Do not use this.'''  It is wasteful; instead
 * just use the predefined constants contained in the `Literal`
 * companion object.
 * 
 * The only reason to use this is to create a Boolean literal with a
 * type other than `BOOLEAN`.  Why?
 */
case class BooleanLiteral(typ: BasicAtom, value: Boolean)
extends Literal[Boolean](typ) {

  override lazy val hashCode = theType.hashCode * 31 + value.hashCode
  override lazy val otherHashCode = typ.otherHashCode + 8191*(value.toString).foldLeft(BigInt(0))(other_hashify)

  override val isTrue = value == true
  override val isFalse = value == false
  /**
   * Alternate constructor with default `BOOLEAN` type.
   */
  def this(value: Boolean) = this(BOOLEAN, value)
  
  def rewrite(binds: Bindings) = {
		theType.rewrite(binds) match {
		  case (newtype, true) =>
        (Literal(newtype, value), true)
		  case _ =>
        (this, false)
		}
	}
  
  def replace(map: Map[BasicAtom, BasicAtom]) = {
    map.get(this) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = theType.replace(map)
        if (flag) {
          (BooleanLiteral(newtype, value), true)
        } else {
          (this, false)
        }
    }
  }
}

/**
 * Provide data and methods for representing values from the IEEE 754 floating
 * point standard.
 */
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
  
  /**
   * Break a number into its components.
   * 
   * @param value	The bits making up the number.  This must be correct for
   * 							the specific platform.
   * @return	A triple consisting of the sign bit, the exponent, and the
   * 					significand.  These raw pieces are extracted and returned as
   * 					integers.  The exponent bias is not removed, nor is the hidden
   *					bit added.
   */
  def fromBits(bits: Long) = {
    // Extract the sign.
    val sign = (bits & signMask) != 0
    // Extract the exponent and adjust for the bias.
    val epart = ((bits & exponentMask) >> significand) - exponentBias
    // Extract the significand.
    val spart = bits & significandMask
    // Return the parts.
    (if (sign) 1 else 0, epart, spart)
  }
  
  /**
   * Create a number from its components.
   * 
   * @param sign	The sign bit.
   * @param epart	The exponent part, not adjusted for bias.
   * @param spart	The significand, minus any hidden bit.
   * @return	The sequence of bits, assembled.
   */
  def toBits(sign: Int, epart: Long, spart: Long) = {
    // The number starts with the sign.
    (if (sign != 0) signMask else 0) |
    // It continues with the exponent, which we shift into place.
    ((epart << exponent) & exponentMask) |
    // Finally the significand is added.
    (spart & significandMask)
  }
}

/** IEEE 754 128-bit float. */
object IEEE754Quadruple extends IEEE754(128, 112)
/** IEEE 754 64-bit float. */
object IEEE754Double extends IEEE754(64, 52)
/** IEEE 754 32-bit float. */
object IEEE754Single extends IEEE754(32, 23)
/** IEEE 754 16-bit float. */
object IEEE754Half extends IEEE754(16, 10)

/**
 * Provide a floating-point literal value.
 * 
 * @param typ						The type.
 * @param significand		The significand.
 * @param exponent			The exponent.
 * @param radix					The radix, which must be 2, 8, 10, or 16.
 */
case class FloatLiteral(typ: BasicAtom, significand: BigInt, exponent: Int,
    radix: Int) extends Literal[(BigInt, Int, Int)](typ) {
  // Validate the radix and compute the prefix string.
  private lazy val _prefix = radix match {
    case 16 => "0x"
    case 10 => ""
    case 8  => "0o"
    case 2  => "0b"
    case _  => require(false, "Invalid radix.")
  }
  /** Is the significand negative. */
  private val _sneg = significand < 0
  /** Is the exponent negative. */
  private val _eneg = exponent < 0
  /** Absolute value of significand. */
  private val _spos = if (_sneg) -significand else significand
  /** Absolute value of exponent. */
  private val _epos = if (_eneg) -exponent else exponent
  
  /**
   * This is a string representing the number.  It should be parseable by
   * the Elision parser as a floating point number.  No type information
   * is included in the string.
   */
  val numberString = 
    (if (_sneg) "-" else "") + _prefix + _spos.toString(radix) +
    (if (radix == 16) "P" else "e") +
    (if (_eneg) "-" else "") + _prefix + Integer.toString(_epos, radix)
    
  /** This value as a platform-dependent floating point value. */
  lazy val toFloat = significand.toFloat * scala.math.pow(radix, exponent).toFloat
  
  /** This value as a platform-dependent double-precision floating point value. */
  lazy val toDouble = significand.toDouble * scala.math.pow(radix, exponent)
  
  /**
   * Get this in the specified IEEE 754 representation.
   * 
   * @param plaf		The floating point representation to use.
   */
  def toIEEE754(plaf: IEEE754 = FloatLiteral.platform) = {
    // Get the representation in bits.
    val bits = java.lang.Double.doubleToRawLongBits(toDouble)
    plaf.fromBits(bits)
  }
  
  /**
   * Transform this into a new floating point literal by passing it through
   * the specified IEEE 754 representation.  No reduction in exponent is
   * performed.
   * 
   * @param plaf		The floating point representation to use.
   */
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

  /**
   * Transform this into a new floating point literal by passing it through
   * the specified IEEE 754 representation.  Trailing zeros of the
   * significand are removed, and the exponent correspondingly adjusted.
   * 
   * @param plaf		The floating point representation to use.
   */
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
  
  /** The value as a triple: significand, exponent, and radix. */
  val value = (significand, exponent, radix)
  
  /**
   * Alternate constructor using the default type `FLOAT`.
   * 
	 * @param significand		The significand.
	 * @param exponent			The exponent.
	 * @param radix					The radix, which must be 2, 8, 10, or 16.
   */
  def this(significand: BigInt, exponent: Int, radix: Int) =
    this(FLOAT, significand, exponent, radix)
	
  def rewrite(binds: Bindings) = {
		theType.rewrite(binds) match {
		  case (newtype, true) =>
        (Literal(newtype, significand, exponent, radix), true)
		  case _ =>
        (this, false)
		}
	}
  
  def replace(map: Map[BasicAtom, BasicAtom]) = {
    map.get(this) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag) = theType.replace(map)
        if (flag) {
          (Literal(newtype, significand, exponent, radix), true)
        } else {
          (this, false)
        }
    }
  }
}

/**
 * Companion object specifying the default IEEE 754 platform to use.
 */
object FloatLiteral {
  /** The default platform to use for floating point work. */
  var platform: IEEE754 = IEEE754Double
}
