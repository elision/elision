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
======================================================================*/
package ornl.elision.core
import ornl.elision.ElisionException

/**
 * Indicate a properties specification is illegal.
 * 
 * @param msg	Human readable message.
 */
class IllegalPropertiesSpecification(msg: String)
extends ElisionException(msg)

/**
 * Encapsulate the algebraic properties ascribed to some object.
 * 
 * == Properties ==
 * The following properties are supported.
 *  - ''Associativity'' implies that children can be arbitrarily grouped.
 *    For example, string concatenation is associative.
 *  - ''Commutativity'' implies that children can be arbitrarily ordered.
 *    For example, multiplication is commutative.
 *  - ''Idempotency'' implies that repeated children are ignored.  For
 *    example, Boolean '''or''' is idempotent.
 *  - An ''absorber'' is a special atom ''A'' that, when added to the children,
 *    causes the result to evaluate to simply ''A''.  Zero is a multiplicative
 *    absorber.
 *  - An ''identity'' is a special atom ''I'' that can be introduced or
 *    omitted from the child list without changing the value.  Zero is the
 *    additive identity.
 *    
 * == Restrictions ==
 * Some properties require others.  At present idempotency requires
 * associativity.  If an absorber or identity is present, associativity is
 * also required.
 * 
 * == Use ==
 * To use this, make an instance and specify the properties.  There are several
 * child classes that may make this easier.
 *  - [[ornl.elision.core.NoProps]]
 *  - [[ornl.elision.core.Associative]]
 *  - [[ornl.elision.core.Commutative]]
 *  - [[ornl.elision.core.Idempotent]]
 *  - [[ornl.elision.core.Absorber]]
 *  - [[ornl.elision.core.Identity]]
 * These can be combined with `and` and the Boolean-valued properties negated
 * with `!`.  Thus one can write `Associative and !Commutative`.
 * 
 * Properties can be specified, or left unspecified.  The entire properties
 * object can be matched and rewritten.
 * 
 * == Application ==
 * Instances are applicable; applied to a typed list of atoms, they "overwrite"
 * the lists properties.
 * 
 * @param associative		Optional associativity.  Default is none.
 * @param commutative		Optional commutativity.  Default is none.
 * @param idempotent		Optional idempotency.  Default is none.
 * @param absorber			Optional absorber.  Default is none.
 * @param identity			Optional identity.  Default is none.
 */
class AlgProp(
    val associative: Option[BasicAtom] = None,
    val commutative: Option[BasicAtom] = None,
    val idempotent: Option[BasicAtom] = None,
    val absorber: Option[BasicAtom] = None,
    val identity: Option[BasicAtom] = None) extends BasicAtom with Applicable {
  
  // Type check the Boolean properties.
  private def _isNotBool(opt: Option[BasicAtom]) = opt match {
    case Some(ANY) => false
    case Some(atom) =>
      atom.theType match {
        case ANY => false
        case BOOLEAN => false
        case _ => true
      }
    case None => false
  }
  if (_isNotBool(associative))
    throw new IllegalPropertiesSpecification(
        "Associativity value must be a Boolean, but the provided value was: " +
        associative.get.toParseString)
  if (_isNotBool(commutative))
    throw new IllegalPropertiesSpecification(
        "Commutativity value must be a Boolean, but the provided value was: " +
        commutative.get.toParseString)
  if (_isNotBool(idempotent))
    throw new IllegalPropertiesSpecification(
        "Idempotency value must be a Boolean, but the provided value was: " +
        idempotent.get.toParseString)
  
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
  
  private val _plist =
    List(associative, commutative, idempotent, absorber, identity)
  
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
  
  val theType = TypeUniverse
  
  val depth = _plist.foldLeft(0) {
    (dbi: Int, opt: Option[BasicAtom]) => dbi max (opt match {
      case None => 0
      case Some(atom) => atom.depth
    })
  } + 1
  
  val isTerm = _plist.foldLeft(true)(_ && _.getOrElse(Literal.TRUE).isTerm)
  
  val constantPool = Some(BasicAtom.buildConstantPool(13, _proplist:_*))
  
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
  
  val deBruijnIndex =
    _plist.foldLeft(0)(_ max _.getOrElse(Literal.TRUE).deBruijnIndex)
  
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
	
	//////////////////// GUI changes
  /**
   * Apply this to the given atom.  If the provided atom is an atom sequence,
   * then this will override the properties of the atom sequence.
   */
  def doApply(rhs: BasicAtom, bypass: Boolean) = {
	// get the node representing this atom that is being rewritten
	val rwNode = RWTree.addToCurrent("AlgProp doApply: ")
    
	rhs match {
		/* A Note to Maintainers
		 * Remember that the "and" has the properties of the second override those
		 * of the first.
		 */
		case ap: AlgProp => (ap and this)
		case as: AtomSeq => 
			val newAS = AtomSeq(as.props and this, as.atoms)
			RWTree.addTo(rwNode, newAS) //rwNode.addChild(newAS)
			newAS
		case _ => 
			val newSA = SimpleApply(this, rhs)
			RWTree.addTo(rwNode, newSA) //rwNode.addChild(newSA)
			newSA
	}
  }
	//////////////////// end GUI changes
  
  /**
   * Rewrite an optional atom.
   * 
   * @param opt		The optional atom.
   * @param binds	The bindings.
   * @return	The rewritten optional atom.
   */
 /* private def _rewrite(opt: Option[BasicAtom], binds: Bindings) = opt match {
    case None => (None, false)
    case Some(atom) => {
      val newatom = atom.rewrite(binds)
      (Some(newatom._1), newatom._2)
    }
  }*/
  
  //////////////////// GUI changes
  
  /**
   * Rewrite an optional atom.
   * 
   * @param opt		The optional atom.
   * @param binds	The bindings.
   * @return	The rewritten optional atom.
   */
  
  private def _rewrite(opt: Option[BasicAtom], binds: Bindings) = {
	// get the node representing this atom that is being rewritten
	val rwNode = RWTree.current
	
	opt match {
		case None => 
			RWTree.addTo(rwNode,"n/a") //rwNode.addChild("n/a")
			(None, false)
		case Some(atom) => {
            val atomNode = RWTree.addTo(rwNode,atom)
			RWTree.current =  atomNode
            
            val newatom = atom.rewrite(binds)
            RWTree.addTo(atomNode,newatom._1)
            
            (Some(newatom._1), newatom._2)
		}
	}
  }
  
  //////////////////// end GUI changes
  /*
  def rewrite(binds: Bindings): (AlgProp, Boolean) = {
    val assoc = _rewrite(associative, binds)
    val commu = _rewrite(commutative, binds)
    val idemp = _rewrite(idempotent, binds)
    val absor = _rewrite(absorber, binds)
    val ident = _rewrite(identity, binds)
    if (assoc._2 || commu._2 || idemp._2 || absor._2 || ident._2)
      (AlgProp(assoc._1, commu._1, idemp._1, absor._1, ident._1), true)
    else (this, false)
  }
  */
  //////////////////// GUI changes
  
  def rewrite(binds: Bindings): (AlgProp, Boolean) = {
	// get the node representing this atom that is being rewritten
	val rwNode = RWTree.addToCurrent("AlgProp rewrite: ") //RWTree.current.addChild("AlgProp rewrite: ")
	
	RWTree.current = RWTree.addTo(rwNode, "associative: ") //rwNode.addChild("associative: ")
    val assoc = _rewrite(associative, binds)
	
	RWTree.current = RWTree.addTo(rwNode, "commutative: ")
    val commu = _rewrite(commutative, binds)
	
	RWTree.current = RWTree.addTo(rwNode, "idempotent: ")
	val idemp = _rewrite(idempotent, binds)
	
	RWTree.current = RWTree.addTo(rwNode, "absorber: ")
	val absor = _rewrite(absorber, binds)
	
	RWTree.current = RWTree.addTo(rwNode, "identity: ")
	val ident = _rewrite(identity, binds)
    
    if (assoc._2 || commu._2 || idemp._2 || absor._2 || ident._2) {
        val newAlgProp = AlgProp(assoc._1, commu._1, idemp._1, absor._1, ident._1)
        RWTree.addTo(rwNode, newAlgProp) //rwNode.addChild(newAlgProp)
        (newAlgProp, true)
	} else (this, false)
  }
  
  //////////////////// end GUI changes
  
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
  
  /**
   * Match two lists of options using `_match` for each, and return the
   * result.
   * 
   * @param plist		The pattern list.
   * @param slist		The subject list.
   * @param binds		Bindings to honor in any match.
   * @return	The outcome.
   */
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
   * Properties in the second override properties in the first, if they
   * are specified.
   *
   * @param other	Another properties list to consider.
   * @return	A new algebraic properties list.
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
   * 
   * @param opt		An optional atom.
   * @return	The atom, with its sense inverted.
   */
  private def invert(opt: Option[BasicAtom]) = opt match {
    case Some(Literal.TRUE) => Some(Literal.FALSE)
    case Some(Literal.FALSE) => Some(Literal.TRUE)
    case _ => opt
  }

  /**
   * Invert the sense of the specified properties.
   *
   * @return	The new algebraic properties list.
   */
  def unary_! = {
    AlgProp(invert(associative), invert(commutative), invert(idempotent))
  }
  
  /**
   * Determine if this atom is equal to another atom.
   * 
   * @param other	The other atom.
   * @return	True iff the atoms are equal.
   */
  override def equals(other: Any) = other match {
    case ap:AlgProp =>
      associative == ap.associative &&
      commutative == ap.commutative &&
      idempotent == ap.idempotent &&
      absorber == ap.absorber &&
      identity == ap.identity
    case _ => false
  }

  /**
   * Generate a descriptive string.
   * 
   * @return	The string.
   */
  def toHumaneString = {
    var list = List[String]()
    associative match {
	    case Some(Literal.TRUE) => list :+= "associative"
	    case Some(Literal.FALSE) => list :+= "not associative"
	    case Some(atom) => list :+= "associative=[" + atom.toParseString + "]"
	    case _ =>
	  }
    commutative match {
	    case Some(Literal.TRUE) => list :+= "commutative"
	    case Some(Literal.FALSE) => list :+= "not commutative"
	    case Some(atom) => list :+= "commutative=[" + atom.toParseString + "]"
	    case _ =>
	  }
    idempotent match {
	    case Some(Literal.TRUE) => list :+= "idempotent"
	    case Some(Literal.FALSE) => list :+= "not idempotent"
	    case Some(atom) => list :+= "idempotent=[" + atom.toParseString + "]"
	    case _ =>
	  }
    absorber match {
	    case None =>
	    case Some(atom) => list :+= "absorber=[" + atom.toParseString + "]"
	  }
    identity match {
	    case None =>
	    case Some(atom) => list :+= "identity=[" + atom.toParseString + "]"
	  }
    if (list.length == 0) "no properties"
    else list.mkString(" and ")
  }

  /**
   * Generate a parse string representation of the atom.
   * 
   * The short properties string uses abbreviations.
   *  - `A` for associative
   *  - `C` for commutative
   *  - `I` for idempotent
   *  - `B[`''atom''`]` for absorber ''atom''
   *  - `D[`''atom''`]` for identity ''atom''
   *  
   * Associativity, commutativity, and idempotency can be negated by prefixing
   * them with an exclamation mark (`!`).  Thus `%A!C` denotes associativity
   * and non-commutativity.
   * 
   * Other atoms (such as variables) can be specified for associativity,
   * commutativity, and idempotency, by giving the atom in square brackets
   * after the abbreviation.  Thus `%A[\$a]C` has a variable `\$a` for
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
 * Simplified creation and matching for algebraic properties objects.
 */
object AlgProp {
  /**
   * Create an algebraic properties object.
   * 
	 * @param associative		Optional associativity.  Default is none.
	 * @param commutative		Optional commutativity.  Default is none.
	 * @param idempotent		Optional idempotency.  Default is none.
	 * @param absorber			Optional absorber.  Default is none.
	 * @param identity			Optional identity.  Default is none.
	 * @return	The new algebraic properties object.
   */
  def apply(associative: Option[BasicAtom] = None,
      commutative: Option[BasicAtom] = None,
      idempotent: Option[BasicAtom] = None,
      absorber: Option[BasicAtom] = None,
      identity: Option[BasicAtom] = None) = {
    // Having the value ANY is really the same as being unspecified, so
    // we correct that now.
    def _adjust(opt: Option[BasicAtom]) = opt match {
      case Some(ANY) => None
      case _ => opt
    }
    new AlgProp(_adjust(associative), _adjust(commutative), _adjust(idempotent),
        _adjust(absorber), _adjust(identity))
  }
  
  /**
   * Pull apart an algebraic properties object.
   * 
   * @param ap	The algebraic properties object.
   * @return	Associativity, commutativity, idempotency, absorber, and identity.
   */
  def unapply(ap: AlgProp) = Some((ap.associative, ap.commutative,
      ap.idempotent, ap.absorber, ap.identity))
}

/** No properties. */
case object NoProps extends AlgProp()

/** The associative property. */
case class Associative(atom: BasicAtom) extends AlgProp(associative = Some(atom))

/** The commutative property */
case class Commutative(atom: BasicAtom) extends AlgProp(commutative = Some(atom))

/** The idempotent property. */
case class Idempotent(atom: BasicAtom) extends AlgProp(idempotent = Some(atom))

/**
 * An absorber.
 * 
 * @param atom	The absorber atom.
 */
case class Absorber(atom: BasicAtom) extends AlgProp(absorber = Some(atom))

/**
 * An identity.
 * 
 * @param atom	The identity atom.
 */
case class Identity(atom: BasicAtom) extends AlgProp(identity = Some(atom))
