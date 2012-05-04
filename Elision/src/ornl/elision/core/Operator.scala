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

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import ornl.elision.ElisionException

/**
 * An incorrect argument list was supplied to an operator.
 * 
 * @param msg	The human-readable message describing the problem.
 */
class ArgumentListException(msg: String) extends ElisionException(msg)

/**
 * Encapsulate an operator.
 * 
 * This is the common base class for all operator classes.
 * 
 * == Purpose ==
 * An operator, by itself, is simply a function that maps from some
 * domain to some codomain.
 * 
 * == Use ==
 * The companion object provides methods to create operators.
 * 
 * @param sfh					The special form holder.
 * @param name				The operator name.
 * @param typ					The type of the fully-applied operator.
 * @param definition	A definition for the operator.
 */
abstract class Operator(sfh: SpecialFormHolder,
    val name: String, val typ: BasicAtom, definition: AtomSeq)
    extends SpecialForm(sfh.tag, sfh.content) with Applicable {
  def apply(atoms: BasicAtom*): BasicAtom
}

/**
 * Provide construction and matching for operators.
 */
object Operator {
  /** Tag for this special form. */
  val tag = Literal('operator)
  
  /**
   * Construct an operator from the provided special form data.
   * 
   * @param sfh		The parsed special form data.
   * @return	An operator.
   */
  def apply(sfh: SpecialFormHolder): Operator = {
    val bh = sfh.requireBindings
    bh.check(Map("name"->true, "cases"->false, "params"->false, "type"->false))
    if (bh.either("cases", "params") == "cases") {
      CaseOperator(sfh)
    } else {
      TypedSymbolicOperator(sfh)
    }
  }
  
  /**
   * Extract the parts of an operator.
   * 
   * @param op		The operator.
   * @return	The triple of name, type, and definition.
   */
  def unapply(op: Operator) = op match {
    case so: SymbolicOperator => Some((so.name, so.theType, so.params))
    case co: CaseOperator => Some((co.name, co.theType, co.cases))
  }
}

/**
 * Encapsulate a reference to an operator.
 * 
 * == Purpose ==
 * Operators are just atoms, so they can be matched and rewritten.  This is
 * not always desirable; we want the operator to remain fixed.  This class
 * provides a level of indirection.
 * 
 * @param operator	The referenced operator.
 */
class OperatorRef(val operator: Operator) extends BasicAtom with Applicable {
  val depth = 0
  val deBruijnIndex = 0
  val constantPool = None
  val isTerm = true
  val isConstant = true
  val theType = OPREF
  /** The operator name. */
  val name = operator.name
  
  /**
   * Apply the referenced operator to the given atom.
   * 
   * @param atom		The atom.
   * @param bypass	Whether to bypass native handlers.
   * @return	The result of applying the referenced operator to the given atom.
   */
  def doApply(atom: BasicAtom, bypass: Boolean) = operator.doApply(atom, bypass)
  
  def toParseString = toESymbol(operator.name) + ":OPREF"
  
  /**
   * Operator references cannot be rewritten.  This is actually why they exist!
   */
  def rewrite(binds: Bindings) = (this, false)
  
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings, hints: Option[Any]) =
    if (subject == this) Match(binds)
    else subject match {
      case OperatorRef(oop) if (oop == operator) => Match(binds)
      case oop: Operator if (oop == operator) => Match(binds)
      case _ => Fail("Operator reference does not match subject.", this, subject)
    }
  
  /**
   * Apply the referenced operator to a sequence of atoms.
   * 
   * @param atoms		The arguments.
   * @return	The result of applying the referenced operator to the sequence
   * 					of atoms.
   */
  def apply(atoms: BasicAtom*) = operator(atoms:_*)
  
  /**
   * Operator references are equal iff the referenced operators are equal.
   */
  override def equals(other: Any) = other match {
    case OperatorRef(oop) if (oop == operator) => true
    //case oop: Operator if (oop == operator) => true
    case _ => false
  }
  
  override lazy val hashCode = 83 * operator.hashCode
}

/**
 * Make and match operator references.
 */
object OperatorRef {
  /**
   * Extract the operator from the reference.
   * 
   * @param ref		The operator reference.
   * @return	The referenced operator.
   */
  def unapply(ref: OperatorRef) = Some(ref.operator)
  /**
   * Make a reference for an operator.
   * 
   * @param op	The operator.
   * @return	A reference to the operator.
   */
  def apply(op: Operator) = new OperatorRef(op)
}

/**
 * Construction and matching of macros (case operators).
 */
object CaseOperator {
  /**
   * Make a case operator from the given special form data.
   * 
   * @param sfh	The special form data.
   * @return	The case operator.
   */
  def apply(sfh: SpecialFormHolder): CaseOperator = {
    val bh = sfh.requireBindings
    bh.check(Map("name"->true, "cases"->true, "type"->false))
    val name = bh.fetchAs[SymbolLiteral]("name").value.name
    val cases = bh.fetchAs[AtomSeq]("cases")
    val typ = bh.fetchAs[BasicAtom]("type", Some(ANY))
    return new CaseOperator(sfh, name, typ, cases)
  }
  
  /**
   * Make a case operator from the components.
   * 
   * @param name			Operator name.
   * @param typ				The operator type (may be `ANY`).
   * @param cases			The cases, as a sequence of atoms.
   * @return	The new case operator.
   */
  def apply(name: String, typ: BasicAtom, cases: AtomSeq): CaseOperator = {
    val nameS = Literal(Symbol(name))
    val binds = Bindings() + ("name"->nameS) + ("cases"->cases) + ("type"->typ)
    val sfh = new SpecialFormHolder(Operator.tag, binds)
    return new CaseOperator(sfh, name, typ, cases)
  }
  
  /**
   * Extract the parts of a case operator.
   * 
   * @param co	The case operator.
   * @return	A triple of the name, type, and cases.
   */
  def unapply(co: CaseOperator) = Some((co.name, co.theType, co.cases))
}

/**
 * Encapsulate a case operator.
 * 
 * == Purpose ==
 * A case operator is actually a kind of macro.  Its definition consists of
 * a sequence of atoms.  When applied to some atom ''A'', it proceeds as
 * follows, considering each atom in its definition, in order.
 *  - If the atom is a rewriter, apply it and, if the success flag is true,
 *    the value is the result.  Otherwise continue.
 *  - If the atom is an applicable, apply it.  The value is the result of
 *    the application.
 *  - If the atom is neither a rewriter nor an applicable, just return that
 *    atom as the result.
 * If the end of the list is reached and no value is determined, then an
 * error is generated (an `ArgumentListException`).
 * 
 * @param sfh				Special form data.
 * @param name			The operator name.
 * @param typ				The operator type.
 * @param cases			The definition.
 */
class CaseOperator private (sfh: SpecialFormHolder,
    name: String, typ: BasicAtom, val cases: AtomSeq)
		extends Operator(sfh, name, typ, cases) {
  /** The type of the operator is the provided type. */
  override val theType = typ
  
  /**
   * Apply this operator to the given arguments.
   * 
   * @param atoms		The arguments.
   * @return	The result of applying this operator to the given argument list.
   */
  def apply(atoms: BasicAtom*) =
    doApply(AtomSeq(NoProps, atoms.toIndexedSeq[BasicAtom]))
  
  def doApply(args: BasicAtom, bypass: Boolean) = {
    // Traverse the list of cases and try to find a case that the arguments
    // match.  Every case should be a rewritable, an applicable, or an atom.
    // If a rewritable, apply it and if it succeeds, choose the result.
    // If an applicable, apply it.  If any other atom, choose that atom.
    var result: Option[BasicAtom] = None
    val done = cases.exists { _ match {
      case rew: Rewriter =>
        val pair = rew.doRewrite(args)
        result = Some(pair._1)
        pair._2
      case app: Applicable =>
        result = Some(app.doApply(args, bypass))
        true
      case atom =>
        result = Some(atom)
        true
    }}
    // If nothing worked, then we need to generate an error since the operator
    // was incorrectly applied.
    if (!done)
      throw new ArgumentListException("Applied the operator " +
          toESymbol(name) + " to an incorrect argument list: " +
          args.toParseString)
    // If the result turned out to be ANY, then just construct a simple
    // apply for this operator.
    result.get match {
      case ANY => SimpleApply(OperatorRef(this), args)
      case other => other
    }
  }
}

/**
 * Construction and matching of typed symbolic operators.
 */
object TypedSymbolicOperator {
  /**
   * Make a typed symbolic operator from the provided special form data.
   * 
   * @param sfh		The parsed special form data.
   * @return	The typed symbolic operator.
   */
  def apply(sfh: SpecialFormHolder): TypedSymbolicOperator = {
    val bh = sfh.requireBindings
    bh.check(Map("name"->true, "params"->true, "type"->false))
    val name = bh.fetchAs[SymbolLiteral]("name").value.name
    val params = bh.fetchAs[AtomSeq]("params")
    val typ = bh.fetchAs[BasicAtom]("type", Some(ANY))
    return new TypedSymbolicOperator(sfh, name, typ, params)
  }
  
  /**
   * Make a typed symbolic operator from the provided parts.
   * 
   * @param name			The operator name.
   * @param typ				The type of the fully-applied operator.
   * @param params		The operator parameters.
   * @return	The typed symbolic operator.
   */
  def apply(name: String, typ: BasicAtom, params: AtomSeq): TypedSymbolicOperator = {
    val nameS = Literal(Symbol(name))
    val binds = Bindings() + ("name"->nameS) + ("params"->params) + ("type"->typ)
    val sfh = new SpecialFormHolder(Operator.tag, binds)
    return new TypedSymbolicOperator(sfh, name, typ, params)
  }
  
  /**
   * Extract the parts of a typed symbolic operator.
   * 
   * @param so	The operator.
   * @return	The triple of name, computed type, and parameters.
   */
  def unapply(so: TypedSymbolicOperator) = Some((so.name, so.theType, so.params))
}

/**
 * Encapsulate a typed symbolic operator.
 * 
 * == Purpose ==
 * A ''typed'' symbolic operator computes its type based on the types of its
 * parameters and the provided "fully applied" type.  The result has the form
 * of a mapping from a domain to a co-domain.
 * 
 * @param sfh		The parsed special form data.
 * @param name			The operator name.
 * @param typ				The type of the fully-applied operator.
 * @param params		The operator parameters.
 */
class TypedSymbolicOperator private (sfh: SpecialFormHolder,
    name: String, typ: BasicAtom, params: AtomSeq)
		extends SymbolicOperator(sfh, name, typ, params) {
	/**
   * The type of an operator is a mapping from the operator domain to the
   * operator codomain.
   */
	override val theType = SymbolicOperator.makeOperatorType(params, typ)
}

/**
 * Construction and matching of symbolic operators.
 */
object SymbolicOperator {
  /**
   * Make a symbolic operator from the provided parts.
   * 
   * @param name			The operator name.
   * @param typ				The type of the fully-applied operator.
   * @param params		The operator parameters.
   * @return	The typed symbolic operator.
   */
  def apply(name: String, typ: BasicAtom, params: AtomSeq): SymbolicOperator = {
    val nameS = Literal(Symbol(name))
    val binds = Bindings() + ("name"->nameS) + ("params"->params) + ("type"->typ)
    val sfh = new SpecialFormHolder(Operator.tag, binds)
    return new SymbolicOperator(sfh, name, typ, params)
  }
  
  /**
   * Extract the parts of a symbolic operator.
   * 
   * @param so	The operator.
   * @return	The triple of name, computed type, and parameters.
   */
  def unapply(so: SymbolicOperator) = Some((so.name, so.theType, so.params))

  /** The well-known MAP operator. */
  val MAP = OperatorRef(SymbolicOperator("MAP", ANY, AtomSeq(NoProps, 'domain, 'codomain)))
  /** The well-known cross product operator. */
  val xx = OperatorRef(SymbolicOperator("xx", ANY, AtomSeq(Associative(true), 'x, 'y)))
  
  /**
   * Compute an operator type.
   * 
   * @param params	The parameters.
   * @param typ			The type of the fully-applied operator.
   * @return	The type for the operator.
   */
  def makeOperatorType(params: AtomSeq, typ: BasicAtom) =
  params.length match {
    case 0 => MAP(NONE, typ)
    case 1 => MAP(params(0).theType, typ)
    case _ => MAP(xx(params.map(_.theType):_*), typ)
  }
}

/**
 * Encapsulate a symbolic operator.
 * 
 * == Purpose ==
 * An (untyped) symbolic operator is a rudimentary form of operator used only
 * for special "primitive" operators that are themselves used to specify the
 * types of operators.
 * 
 * @param sfh				The parsed special form data.
 * @param name			The operator name.
 * @param typ				The type of the fully-applied operator.
 * @param params		The operator parameters.
 */
protected class SymbolicOperator protected (sfh: SpecialFormHolder,
    name: String, typ: BasicAtom, val params: AtomSeq)
		extends Operator(sfh, name, typ, params) {
	override val theType: BasicAtom = ANY
  
  /** The native handler, if one is declared. */
  protected[core]
  var handler: Option[(SymbolicOperator,AtomSeq,Bindings) => BasicAtom] = None
	
  /**
   * Apply this operator to the given arguments.
   * 
   * @param atoms		The arguments.
   * @return	The result of applying this operator to the given argument list.
   */
  def apply(args: BasicAtom*): BasicAtom = {
    // Make an atom list from the arguments.
    val seq = AtomSeq(NoProps, args:_*)
    doApply(seq, false)
  }
  
  def doApply(rhs: BasicAtom, bypass: Boolean): BasicAtom = {
    rhs match {
      case args: AtomSeq =>
		    // There are special cases to handle here.  First, if the argument list
		    // is empty, but there is an identity, return it.
		    if (args.length == 0) {
		      params.identity match {
		        case Some(ident) =>
		          // Return the identity.
		          return ident
		        case None =>
		          // No identity.  Proceed with caution.
		      }
		    }
		    
		    // If the argument list is associative and we have a single element, then
		    // that element must match the type of the operator, and we return it.
		    if (args.length == 1) {
		      if (params.associative) {
		        // Get the atom.
		        val atom = args(0)
		        // Match the type of the atom against the type of the parameters.
		        val param = params(0)
		        param.tryMatch(atom) match {
		          case Fail(reason, index) =>
		            // The argument is invalid.  Reject!
					      throw new ArgumentListException("Incorrect argument for operator " +
					          toESymbol(name) + " at position 0: " + atom.toParseString)
		          case mat: Match =>
		            // The argument matches.
		            return atom
		          case many: Many =>
		            // The argument matches.
		            return atom
		        }
		      }
		    }
		    
		    // If the operator is associative, we pad the parameter list to get faster
		    // matching.  Otherwise we just match as-is.  In any case, when we are done
		    // we can just use the sequence matcher.
		    val newparams = if (params.associative) {
		      var newatoms: OmitSeq[BasicAtom] = EmptySeq
		      val atom = params(0)
		      for (index <- 0 until args.length) {
		        var param = Variable(atom.theType, ""+index)
		        newatoms = param +: newatoms
		      } // Build new parameter list.
		      newatoms
		    } else {
		      params.atoms
		    }
		    
		    // Handle actual operator application.
		    def handleApply(binds: Bindings): BasicAtom = {
		      // Re-package the arguments with the correct properties.
		      val newargs = AtomSeq(params.props, args.atoms)
		      // See if we are bypassing the native handler.
		      if (!bypass) {
		        // Run any native handler.
		        if (handler.isDefined) return handler.get(this, newargs, binds)
		      }
		      // No native handler.
		      return OpApply(OperatorRef(this), newargs, binds)
		    }
		    
		    // We've run out of special cases to handle.  Now just try to match the
		    // arguments against the parameters.
		    SequenceMatcher.tryMatch(newparams, args) match {
		      case Fail(reason, index) =>
			      throw new ArgumentListException("Incorrect argument for operator " +
			          toESymbol(name) + " at position " + index + ": " +
			          args(0).toParseString)
		      case Match(binds) =>
		        // The argument list matches.
		        return handleApply(binds)
		      case Many(iter) =>
		        // The argument list matches.
		        return handleApply(iter.next)
		    }
		    
      case _ => SimpleApply(this, rhs)
    }
  }
}
