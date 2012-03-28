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

/**
 * Represent algebraic properties of operators.
 */
sealed case class AlgProp(associative: Option[Boolean] = None,
  commutative: Option[Boolean] = None, idempotent: Option[Boolean] = None,
  absorber: Option[BasicAtom] = None, identity: Option[BasicAtom] = None) {
  
  val isConstant = (absorber match {
    case None => true
    case Some(atom) => atom.isConstant
  }) && (identity match {
    case None => true
    case Some(atom) => atom.isConstant
  })

  /**
   * Reflect the value of an optional Boolean using a string.
   *
   * @param char	The string.
   * @param opt		The optional Boolean.
   * @return	Empty string for None, the char for Some(true), and the negated
   * 					(!) char for Some(false).
   */
  private def optToString(char: String, opt: Option[Boolean]) = opt match {
    case None => ""
    case Some(true) => char
    case Some(false) => "!" + char
  }

  /**
   * Create the shorthand representation of this property object.
   *
   * @return The string representation.
   */
  def toShortString = optToString("A", associative) +
    optToString("C", commutative) + optToString("I", idempotent)
    
  /**
   * Create a string representing this property object.
   *
   * @return The string representation.
   */
  def toParseString = {
    var list = List[String]()
    identity match {
      case Some(atom) => list ::= "identity(" + atom.toParseString + ")"
      case _ =>
    }
    absorber match {
      case Some(atom) => list ::= "absorber(" + atom.toParseString + ")"
      case _ =>
    }
    idempotent match {
      case Some(true) => list ::= "idempotent"
      case _ =>
    }
    commutative match {
      case Some(true) => list ::= "commutative"
      case _ =>
    }
    associative match {
      case Some(true) => list ::= "associative"
      case _ =>
    }
    list.mkString("is ", ", ", "")
  }

  /**
   * Implement the logic to join two Boolean options.
   *
   * @param o1	The first option.
   * @param o2	The second option.
   * @return	The joined option.
   */
  private def join(o1: Option[Boolean], o2: Option[Boolean]) = (o1, o2) match {
    // The second option always wins if it is specified.
    case (_, None) => o1
    case (_, Some(true)) => o2
    case (_, Some(false)) => o2
  }

  /**
   * Combine this with another property and yield the resulting property.
   *
   * @param other	Another property to consider.
   * @return	A new list property.
   */
  def and(other: AlgProp) = {
    AlgProp(
      join(associative, other.associative),
      join(commutative, other.commutative),
      join(idempotent, other.idempotent))
  }

  /**
   * Invert a single Boolean option.
   */
  private def invert(opt: Option[Boolean]) = opt match {
    case None => None
    case Some(flag) => Some(!flag)
  }

  /**
   * Invert the sense of the specified properties.
   *
   * @return	The new list property.
   */
  def unary_! = {
    AlgProp(invert(associative), invert(commutative), invert(idempotent))
  }
}

/** No properties. */
case object NoProps extends AlgProp()

/** The associative property. */
case object Associative extends AlgProp(associative = Some(true))

/** The commutative property */
case object Commutative extends AlgProp(commutative = Some(true))

/** The idempotent property. */
case object Idempotent extends AlgProp(idempotent = Some(true))

/** An absorber. */
case class Absorber(atom: BasicAtom) extends AlgProp(absorber = Some(atom))

/** An identity. */
case class Identity(atom: BasicAtom) extends AlgProp(identity = Some(atom))
