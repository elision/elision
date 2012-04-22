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
import sjp.elision.ElisionException

/**
 * Indicate a properties specification is illegal.
 * 
 * @param msg	Human readable message.
 */
class IllegalPropertiesSpecification(msg: String)
extends ElisionException(msg)

class AlgProp(
    val associative: Option[BasicAtom] = None,
    val commutative: Option[BasicAtom] = None,
    val idempotent: Option[BasicAtom] = None,
    val absorber: Option[BasicAtom] = None,
    val identity: Option[BasicAtom] = None) extends BasicAtom with Applicable {
  
  // Type check the Boolean properties.
  private def _isNotBool(opt: Option[BasicAtom]) = opt match {
    case Some(atom) =>
      atom.theType match {
        case ANY => false
        case BOOLEAN => false
        case _ => println(atom.theType); true
      }
    case None => false
  }
  if (_isNotBool(associative))
    throw new IllegalPropertiesSpecification(
        "Associativity is not a Boolean value: " + associative)
  if (_isNotBool(commutative))
    throw new IllegalPropertiesSpecification(
        "Commutativity is not a Boolean value: " + commutative)
  if (_isNotBool(idempotent))
    throw new IllegalPropertiesSpecification(
        "Idempotency is not a Boolean value: " + idempotent)
  
  // If we are not associative, we cannot have idempotency, identities, or
  // absorbers.
  if (!isA(true)) {
    if (isI(false))
      throw new IllegalPropertiesSpecification("Idempotency requires associativity.")
    if (getB(null) != null)
      throw new IllegalPropertiesSpecification("An absorber requires associativity.")
    if (getD(null) != null)
      throw new IllegalPropertiesSpecification("An identity requires associativity.")
  }
  
  /**
   * Fast check for associativity.
   * 
   * @param default	What to return if unspecified.
   */
  def isA(default: Boolean) = associative match {
    case Some(Literal.TRUE) => true
    case Some(Literal.FALSE) => false
    case _ => default
  }
  
  /**
   * Fast check for commutativity.
   * 
   * @param default	What to return if unspecified.
   */
  def isC(default: Boolean) = commutative match {
    case Some(Literal.TRUE) => true
    case Some(Literal.FALSE) => false
    case _ => default
  }
  
  /**
   * Fast check for idempotency.
   * 
   * @param default	What to return if unspecified.
   */
  def isI(default: Boolean) = idempotent match {
    case Some(Literal.TRUE) => true
    case Some(Literal.FALSE) => false
    case _ => default
  }
  
  /**
   * Fast check for absorber.
   * 
   * @param default	What to return if unspecified.
   */
  def getB(default: BasicAtom) = absorber match {
    case Some(atom) => atom
    case _ => default
  }
  
  /**
   * Fast check for identity.
   * 
   * @param default	What to return if unspecified.
   */
  def getD(default: BasicAtom) = absorber match {
    case Some(atom) => atom
    case _ => default
  }
  
  /** The type. */
  val theType = TypeUniverse
  
  private val _plist =
    List(associative, commutative, idempotent, absorber, identity)
  
  /** The depth is equal to the maximum depth, plus one. */
  val depth = _plist.foldLeft(0) {
    (dbi: Int, opt: Option[BasicAtom]) => dbi max (opt match {
      case None => 0
      case Some(atom) => atom.depth
    })
  } + 1
  
  /** This is a term iff all its parts are terms. */
  val isTerm = _plist.foldLeft(true)(_ && _.getOrElse(Literal.TRUE).isTerm)
  
  private val _proplist = {
    var list = List[BasicAtom]()
    def add(opt: Option[BasicAtom]) = opt match {
      case None =>
      case Some(atom) => list ::= atom
    }
    add(associative)
    add(commutative)
    add(idempotent)
    add(absorber)
    add(identity)
    list
  }
  
  def doApply(rhs: BasicAtom) = rhs match {
    case as: AtomSeq => AtomSeq(this, as.atoms)
    case _ => SimpleApply(this, rhs)
  }
  
  /** The constant pool. */
  val constantPool = Some(BasicAtom.buildConstantPool(13, _proplist:_*))
  
  /** This is constant iff all parts are constant. */
  val isConstant = (associative match {
    case None => true
    case Some(atom) => atom.isConstant
  }) && (commutative match {
    case None => true
    case Some(atom) => atom.isConstant
  }) && (idempotent match {
    case None => true
    case Some(atom) => atom.isConstant
  }) && (absorber match {
    case None => true
    case Some(atom) => atom.isConstant
  }) && (identity match {
    case None => true
    case Some(atom) => atom.isConstant
  })
  
  /** The De Bruijn index is the maximum of the parts. */
  val deBruijnIndex =
    _plist.foldLeft(0)(_ max _.getOrElse(Literal.TRUE).deBruijnIndex)
  
  /**
   * Rewrite an optional atom.
   * 
   * @param opt		The optional atom.
   * @param binds	The bindings.
   * @return	The rewritten optional atom.
   */
  private def _rewrite(opt: Option[BasicAtom], binds: Bindings) = opt match {
    case None => (None, false)
    case Some(atom) => {
      val newatom = atom.rewrite(binds)
      (Some(newatom._1), newatom._2)
    }
  }
  
  def rewrite(binds: Bindings) = {
    val assoc = _rewrite(associative, binds)
    val commu = _rewrite(commutative, binds)
    val idemp = _rewrite(idempotent, binds)
    val absor = _rewrite(absorber, binds)
    val ident = _rewrite(identity, binds)
    if (assoc._2 || commu._2 || idemp._2 || absor._2 || ident._2) (this, false)
    else (AlgProp(assoc._1, commu._1, idemp._1, absor._1, ident._1), true)
  }
  
  /**
   * Match two optional atoms against one another.  A match is really only
   * performed iff both are atoms.  If the pattern is unspecified, then the
   * match succeeds.  If the pattern is specified, but the subject is not, then
   * the pattern is matched against Nothing.  If both are specified, they are
   * matched as usual.
   * 
   * @param pat		The pattern.
   * @param sub		The subject.
   * @param binds	The bindings.
   * @return	The outcome.
   */
  private def _match(pat: Option[BasicAtom], sub: Option[BasicAtom],
      binds: Bindings) = pat match {
    case None => Match(binds)
    case Some(pattern) => sub match {
      case None => pattern.tryMatch(ANY, binds)
      case Some(subject) => pattern.tryMatch(subject, binds)
    }
  }
  
  private def _matchAll(plist: List[Option[BasicAtom]],
      slist: List[Option[BasicAtom]], binds: Bindings): Outcome =
    if (plist.length == 0) Match(binds)
    else {
      _match(plist.head, slist.head, binds) match {
        case fail: Fail => fail
        case Match(newbinds) => _matchAll(plist.tail, slist.tail, newbinds)
        case Many(iter) =>
          Many(iter ~> ((newbinds: Bindings) =>
            _matchAll(plist.tail, slist.tail, newbinds)))
      }
    }
  
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) = subject match {
    case ap: AlgProp =>
      _matchAll(
          List(associative, commutative, idempotent, absorber, identity),
          List(ap.associative, ap.commutative, ap.idempotent, ap.absorber, ap.identity),
          binds)
    case _ => Fail("Properties only match other properties.", this, subject)
  }
  
  /**
   * Join two optional atoms together.  The second, if specified, overrides
   * the first.
   * 
   * @param a1	The first atom.
   * @param a2	The second atom.
   * @return	The result.
   */
  private def joinatoms(a1: Option[BasicAtom],
      a2: Option[BasicAtom]) = (a1, a2) match {
    case (_, None) => a1
    case (_, Some(atom)) => a2
  }

  /**
   * Combine this with another property and yield the resulting property.
   *
   * @param other	Another property to consider.
   * @return	A new list property.
   */
  def and(other: AlgProp) = {
    AlgProp(
      joinatoms(associative, other.associative),
      joinatoms(commutative, other.commutative),
      joinatoms(idempotent, other.idempotent),
      joinatoms(absorber, other.absorber),
      joinatoms(identity, other.identity))
  }

  /**
   * Invert a single Boolean option.
   */
  private def invert(opt: Option[BasicAtom]) = opt match {
    case Some(Literal.TRUE) => Some(Literal.FALSE)
    case Some(Literal.FALSE) => Some(Literal.TRUE)
    case _ => opt
  }

  /**
   * Invert the sense of the specified properties.
   *
   * @return	The new list property.
   */
  def unary_! = {
    AlgProp(invert(associative), invert(commutative), invert(idempotent))
  }
  
  override def equals(other: Any) = other match {
    case ap:AlgProp =>
      associative == ap.associative &&
      commutative == ap.commutative &&
      idempotent == ap.idempotent &&
      absorber == ap.absorber &&
      identity == ap.identity
    case _ => false
  }

  override def toString = "AlgProp(" + associative + ", " + commutative + ", " +
  		idempotent + ", " + absorber + ", " + identity + ")"

  /**
   * Generate a parse string representation of the atom.
   * 
   * The short properties string uses abbreviations.
   *  - {{A}} for associative
   *  - {{C}} for commutative
   *  - {{I}} for idempotent
   *  - {{B[}}''atom''{{]}} for absorber ''atom''
   *  - {{D[}}''atom''{{]}} for identity ''atom''
   *  
   * Associativity, commutativity, and idempotency can be negated by prefixing
   * them with an exclamation mark ({{!}}).  Thus {{%A!C}} denotes associativity
   * and non-commutativity.
   * 
   * Other atoms (such as variables) can be specified for associativity,
   * commutativity, and idempotency, by giving the atom in square brackets
   * after the abbreviation.  Thus {{%A[$a]C}} has a variable {{$a}} for
   * associativity, with commutativity true.
   * 
   * @return	The short string.
   */
  def toParseString = "%" + (associative match {
    case Some(Literal.TRUE) => "A"
    case Some(Literal.FALSE) => "!A"
    case Some(atom) => "A[" + atom.toParseString + "]"
    case _ => ""
  }) + (commutative match {
    case Some(Literal.TRUE) => "C"
    case Some(Literal.FALSE) => "!C"
    case Some(atom) => "C[" + atom.toParseString + "]"
    case _ => ""
  }) + (idempotent match {
    case Some(Literal.TRUE) => "I"
    case Some(Literal.FALSE) => "!I"
    case Some(atom) => "I[" + atom.toParseString + "]"
    case _ => ""
  }) + (absorber match {
    case None => ""
    case Some(atom) => "B[" + atom.toParseString + "]"
  }) + (identity match {
    case None => ""
    case Some(atom) => "D[" + atom.toParseString + "]"
  })
}

/**
 * Simplified creation and matching.
 */
object AlgProp {
  def apply(associative: Option[BasicAtom] = None,
      commutative: Option[BasicAtom] = None,
      idempotent: Option[BasicAtom] = None,
      absorber: Option[BasicAtom] = None,
      identity: Option[BasicAtom] = None) =
    new AlgProp(associative, commutative, idempotent, absorber, identity)
}

/** No properties. */
case object NoProps extends AlgProp()

/** The associative property. */
case class Associative(atom: BasicAtom) extends AlgProp(associative = Some(atom))

/** The commutative property */
case class Commutative(atom: BasicAtom) extends AlgProp(commutative = Some(atom))

/** The idempotent property. */
case class Idempotent(atom: BasicAtom) extends AlgProp(idempotent = Some(atom))

/** An absorber. */
case class Absorber(atom: BasicAtom) extends AlgProp(absorber = Some(atom))

/** An identity. */
case class Identity(atom: BasicAtom) extends AlgProp(identity = Some(atom))
