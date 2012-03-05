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

/** Simple operator prototype creation and pattern matching. */
object Proto {
  /**
   * Make a new operator prototype.
   * 
   * @param name			The operator name.
   * @param typ				The type of a fully-applied operator.
   * @param arguments	The formal parameters.
   * @return	The new operator prototype.
   */
  def apply(name: String, typ: BasicAtom, parameters: Variable*) =
    OperatorPrototype(name, parameters.toList, typ)
    
  /**
   * Deconstruct an operator prototype into its components for a pattern match.
   * This works with up to five parameters.
   * 
   * @param op		The operator prototype.
   * @return	The components of the operator prototype, organized by operator
   * 					name, then operator type, and then each of the formal parameters.
   */
  def unapply(op: OperatorPrototype) = op.pars match {
    case List() => Some(op.name, op.typ)
    case List(v1) => Some(op.name, op.typ, v1)
    case List(v1, v2) => Some(op.name, op.typ, v1, v2)
    case List(v1, v2, v3) => Some(op.name, op.typ, v1, v2, v3)
    case List(v1, v2, v3, v4) => Some(op.name, op.typ, v1, v2, v3, v4)
    case List(v1, v2, v3, v4, v5) => Some(op.name, op.typ, v1, v2, v3, v4, v5)
    case _ => None
  }
}

/** Operator properties. */
sealed abstract class OpProperty

/** Indicate that an operator is associative. */
case class Associative() extends OpProperty

/** Indicate that an operator is commutative. */
case class Commutative() extends OpProperty

/** Indicate that an operator is idempotent. */
case class Idempotent() extends OpProperty

/**
 * Indicate that an operator has an identity.
 * 
 * @param id	The identity.
 */
case class Identity(id: BasicAtom) extends OpProperty

/**
 * Indicate that an operator has an absorber.
 * 
 * @param ab	The absorber.
 */
case class Absorber(ab: BasicAtom) extends OpProperty

/** Simple operator properties creation and pattern matching. */
object Prop {
  /**
   * Make a new operator properties object by passing the individual properties.
   * 
   * @param properties	The operator properties.
   * @return	The new operator properties object.
   */
  def apply(properties: OpProperty*) = {
    var (assoc, comm, idem) = (false, false, false)
    var absorber: Option[BasicAtom] = None
    var identity: Option[BasicAtom] = None
    for (prop <- properties) prop match {
      case Associative() => assoc = true
      case Commutative() => comm = true
      case Idempotent() => idem = true
      case Identity(id) => identity = Some(id)
      case Absorber(ab) => absorber = Some(ab)
    }
    new OperatorProperties(assoc, comm, idem, absorber, identity)
  }
  
  /**
   * Deconstruct an operator properties object into its components.
   * 
   * @param	properties	The operator properties object.
   * @return	The properties, in order, as associativity, commutativity,
   * 					idempotence, any absorber, and any identity.
   */
  def unapply(properties: OperatorProperties) =
    Some(properties.associative, properties.commutative, properties.idempotent,
        properties.absorber, properties.identity)
}

/** Simple operator application creation and pattern matching. */
object Oper {
  /**
   * Make a new operator application.
   * 
   * @param lib		The operator library.
   * @param name	The operator name.
   * @param arg		The arguments.
   * @return	The new operator application.
   */
  def apply(lib: OperatorLibrary, name: String, arg: BasicAtom*) =
    Apply(lib(name), AtomList(arg.toList))
    
  /**
   * Make a new operator application.
   * 
   * @param op		The operator.
   * @param arg		The arguments.
   * @return	The new operator application.
   */
  def apply(op: Operator, arg: BasicAtom*) =
    Apply(op, AtomList(arg.toList))
    
  /**
   * Deconstruct an operator application into its components.  This works with
   * five or fewer arguments.
   * 
   * @param apply	The application.
   * @return	The application parts, consisting of the name, followed by any
   * 					arguments.
   */
  def unapply(apply: Apply) = apply match {
    case Apply(op:Operator, AtomList(list, _)) => list match {
      case Seq() => Some(op.name)
      case Seq(v1) => Some(op.name, v1)
      case Seq(v1, v2) => Some(op.name, v1, v2)
      case Seq(v1, v2, v3) => Some(op.name, v1, v2, v3)
      case Seq(v1, v2, v3, v4) => Some(op.name, v1, v2, v3, v4)
      case Seq(v1, v2, v3, v4, v5) => Some(op.name, v1, v2, v3, v4, v5)
      case _ => None
    }
    case _ => None
  }
  
  /**
   * Extract the arguments from the apply, and return those arguments.
   * 
   * @param apply	The operator application.
   * @return	The arguments from the application.
   */
  def unapplySeq(apply: Apply) = apply match {
    case Apply(_, AtomList(list, _)) => list
  }
}
