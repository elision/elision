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
 * @param description	An optional short description for the operator.
 * @param detail			Optional detailed help for the operator.
 */
abstract class Operator(sfh: SpecialFormHolder,
    val name: String, val typ: BasicAtom, definition: AtomSeq,
    val description: String, val detail: String)
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
    bh.check(Map("name"->true, "cases"->false, "params"->false, "type"->false,
        "description"->false, "detail"->false))
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
  
  override def toString = "OperatorRef(" + operator.toString + ")"
  
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
    bh.check(Map("name"->true, "cases"->true, "type"->false,
        "description"->false, "detail"->false))
    val name = bh.fetchAs[SymbolLiteral]("name").value.name
    val cases = bh.fetchAs[AtomSeq]("cases")
    val typ = bh.fetchAs[BasicAtom]("type", Some(ANY))
    val description = bh.fetchAs[StringLiteral]("description", Some("No description."))
    val detail = bh.fetchAs[StringLiteral]("detail", Some("No detail."))
    return new CaseOperator(sfh, name, typ, cases, description, detail)
  }
  
  /**
   * Make a case operator from the components.
   * 
   * @param name					Operator name.
   * @param typ						The operator type (may be `ANY`).
   * @param cases					The cases, as a sequence of atoms.
	 * @param description		An optional short description for the operator.
	 * @param detail				Optional detailed help for the operator.
   * @return	The new case operator.
   */
  def apply(name: String, typ: BasicAtom, cases: AtomSeq,
      description: String, detail: String): CaseOperator = {
    val nameS = Literal(Symbol(name))
    val binds = Bindings() + ("name"->nameS) + ("cases"->cases) +
    		("type"->typ) + ("description"->Literal(description)) +
    		("detail"->Literal(detail))
    val sfh = new SpecialFormHolder(Operator.tag, binds)
    return new CaseOperator(sfh, name, typ, cases, description, detail)
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
 * @param sfh						Special form data.
 * @param name					The operator name.
 * @param typ						The operator type.
 * @param cases					The definition.
 * @param description		An optional short description for the operator.
 * @param detail				Optional detailed help for the operator.
 */
class CaseOperator private (sfh: SpecialFormHolder,
    name: String, typ: BasicAtom, val cases: AtomSeq,
    description: String, detail: String)
		extends Operator(sfh, name, typ, cases, description, detail) {
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
        val pair = rew.doRewrite(args, Some(this))
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
      case ANY => args match {
        case as:AtomSeq => OpApply(OperatorRef(this), as, Bindings())
        case _ => SimpleApply(OperatorRef(this), args)
      }
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
    bh.check(Map("name"->true, "params"->true, "type"->false,
        "description"->false, "detail"->false))
    val name = bh.fetchAs[SymbolLiteral]("name").value.name
    val params = bh.fetchAs[AtomSeq]("params")
    val typ = bh.fetchAs[BasicAtom]("type", Some(ANY))
    val description = bh.fetchAs[StringLiteral]("description", Some("No description."))
    val detail = bh.fetchAs[StringLiteral]("detail", Some("No detail."))
    return new TypedSymbolicOperator(sfh, name, typ, params, description, detail)
  }
  
  /**
   * Make a typed symbolic operator from the provided parts.
   * 
   * @param name					The operator name.
   * @param typ						The type of the fully-applied operator.
   * @param params				The operator parameters.
	 * @param description		An optional short description for the operator.
	 * @param detail				Optional detailed help for the operator.
   * @return	The typed symbolic operator.
   */
  def apply(name: String, typ: BasicAtom, params: AtomSeq,
      description: String, detail: String): TypedSymbolicOperator = {
    val nameS = Literal(Symbol(name))
    val binds = Bindings() + ("name"->nameS) + ("params"->params) +
    		("type"->typ) + ("description"->Literal(description)) +
    		("detail"->Literal(detail))
    val sfh = new SpecialFormHolder(Operator.tag, binds)
    return new TypedSymbolicOperator(sfh, name, typ, params, description, detail)
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
    name: String, typ: BasicAtom, params: AtomSeq,
    description: String, detail: String)
		extends SymbolicOperator(sfh, name, typ, params, description, detail) {
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
   * @param name					The operator name.
   * @param typ						The type of the fully-applied operator.
   * @param params				The operator parameters.
	 * @param description		An optional short description for the operator.
	 * @param detail				Optional detailed help for the operator.
   * @return	The typed symbolic operator.
   */
  def apply(name: String, typ: BasicAtom, params: AtomSeq,
      description: String, detail: String): SymbolicOperator = {
    val nameS = Literal(Symbol(name))
    val binds = Bindings() + ("name"->nameS) + ("params"->params) +
    		("type"->typ) + ("description"->Literal(description)) +
    		("detail"->Literal(detail))
    val sfh = new SpecialFormHolder(Operator.tag, binds)
    return new SymbolicOperator(sfh, name, typ, params, description, detail)
  }
  
  /**
   * Extract the parts of a symbolic operator.
   * 
   * @param so	The operator.
   * @return	The triple of name, computed type, and parameters.
   */
  def unapply(so: SymbolicOperator) = Some((so.name, so.theType, so.params))

  /** The well-known MAP operator. */
  val MAP = OperatorRef(
      SymbolicOperator("MAP", ANY, AtomSeq(NoProps, 'domain, 'codomain),
          "Mapping constructor.",
          """|This operator is used to construct types for operators.  It
             |indicates a mapping from one type (the domain) to another type
             |(the codomain).
          """.stripMargin))
  /** The well-known cross product operator. */
  val xx = OperatorRef(
      SymbolicOperator("xx", ANY, AtomSeq(Associative(true), 'x, 'y),
          "Cross product.",
          """|This operator is used to construct types for operators.  It
             |indicates the cross product of two atoms (typically types).
             |These originate from the types of the parameters of an operator.
          """.stripMargin))
  /** The well-known list operator. */
  val LIST = OperatorRef(
      SymbolicOperator("LIST", ANY, AtomSeq(NoProps, 'type),
          "List type constructor.",
          """|This operator is used to indicate the type of a list.  It
             |takes a single argument that is the type of the atoms in
             |the list.  For heterogeneous lists this will be ANY.
          """.stripMargin))
  
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
    name: String, typ: BasicAtom, val params: AtomSeq,
    description: String, detail: String)
		extends Operator(sfh, name, typ, params, description, detail) {
	override val theType: BasicAtom = ANY
	
	// Check the properties.
	_check()
  
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
	
	/**
	 * Check the parameters against the properties.  If any problems are detected,
	 * then an exception is thrown (`ArgumentListException`). 
	 */
	def _check() {
	  /**
	   * Define a little method to require that all parameters have the same
	   * type.
	   * 
	   * @return	True if all parameters have the same type, and false if not.
	   */
	  def paramTypeCheck = {
	    val aType = params(0).theType
	    params.forall(_.theType == aType)
	  }
	  
	  // Check the properties and make sure everything is in accordance with
	  // them.
	  if (params.props.isA(false)) {
	    // There must be exactly two parameters.
	    if (params.length != 2) {
	      throw new ArgumentListException("The operator " + toESymbol(name) +
	          " is marked as associative, but does not have exactly two " +
	          "parameters, as required: " + params.toParseString)
	    }
	    // All parameter types must be the same.
	    if (!paramTypeCheck) {
	      throw new ArgumentListException("The operator " + toESymbol(name) +
	          " is marked as associative, but all parameters do not hae the " +
	          "same type, as required: " + params.toParseString)
	    }
	    // The fully-applied type must be the same as the parameter type.
	    if (params(0).theType != typ) {
	      throw new ArgumentListException("The operator " + toESymbol(name) +
	          " is marked as associative, but the parameter type (" +
	          params(0).theType.toParseString +
	          ") is not the same as the fully-applied type (" +
	          typ.toParseString + ").")
	    }
	  } else {
	    // The operator is not associative, so it must not have an identity,
	    // absorber, or be idempotent.
	    if (params.props.isI(false)) {
	      throw new ArgumentListException("The operator " + toESymbol(name) +
	          " is marked as idempotent, but it not marked as associative, as" +
	          " required.")
	    }
	    if (params.props.identity.isDefined) {
	      throw new ArgumentListException("The operator " + toESymbol(name) +
	          " is declared to have an identity, but it not marked as " +
	          "associative, as required.")
	    }
	    if (params.props.absorber.isDefined) {
	      throw new ArgumentListException("The operator " + toESymbol(name) +
	          " is declared to have an absorber, but it not marked as " +
	          "associative, as required.")
	    }
	  }
	  if (params.props.isC(false)) {
	    // There must be at least two parameters.
	    if (params.length < 2) {
	      throw new ArgumentListException("The operator " + toESymbol(name) +
	          " is marked as commutative, but does not have at least two " +
	          "parameters, as required: " + params.toParseString)
	    }
	    // All parameter types must be the same.
	    if (!paramTypeCheck) {
	      throw new ArgumentListException("The operator " + toESymbol(name) +
	          " is marked as commutative, but all parameters do not hae the " +
	          "same type, as required: " + params.toParseString)
	    }
	  }
	  // Any identity must match the parameter type.  We just try to match
	  // the first parameter's type.
	  if (params.props.identity.isDefined) {
	    params(0).theType.tryMatch(params.props.identity.get.theType) match {
	      case Fail(reason, index) =>
		      throw new ArgumentListException("The operator " + toESymbol(name) +
		          " has an identity whose type (" +
		          params.props.identity.get.theType.toParseString +
		          ") does not match the parameter type (" +
		          params(0).theType.toParseString + ").")
	      case _ =>
	    }
	  }
	  // Any absorber must match the parameter type.  We just try to match
	  // the first parameter's type.
	  if (params.props.absorber.isDefined) {
	    params(0).theType.tryMatch(params.props.absorber.get.theType) match {
	      case Fail(reason, index) =>
		      throw new ArgumentListException("The operator " + toESymbol(name) +
		          " has an absorber whose type (" +
		          params.props.absorber.get.theType.toParseString +
		          ") does not match the parameter type (" +
		          params(0).theType.toParseString + ").")
	      case _ =>
	    }
	  }
	}
  
  def doApply(rhs: BasicAtom, bypass: Boolean): BasicAtom = {
    rhs match {
      case args: AtomSeq =>
        // Things have to happen in the correct order here.  First increase
        // the argument list by flattening associative applications.  Second
        // we reduce by looking for identities, etc.  Third we check for an
        // empty argument list.
        
        // Save the properties for fast access.
        val props = params.props
        val assoc = props.isA(false)
        val commu = props.isC(false)
        val idemp = props.isI(false)
        val absor = props.absorber.isDefined
        val ident = props.identity.isDefined
        
        // Run through the arguments and watch for the absorber, omit
        // identities, and flatten associative lists.
	      var newseq = OmitSeq[BasicAtom]()
	      for (atom <- args) {
	        // If there is an absorber and we find it, we are done.
	        if (absor && props.absorber.get == atom) {
	          // Found the absorber.  Nothing else to do.
	          return props.absorber.get
	        }
	        // Skip the identity.
	        if (!ident || props.identity.get != atom) {
	          if (assoc) atom match {
	            case OpApply(opref, args, binds) if opref.operator == this => 
	              // Add the arguments directly to this list.  We can assume this
	              // list has already been processed, so no deeper checking is
	              // needed.  This flattens the associative lists.
	              newseq ++= args
	            case _ =>
	              // Add this atom to the list.
	              newseq :+= atom
	          } else {
	            newseq :+= atom
	          }
	        }
	      } // Loop over atoms.
		    
		    // Handle actual operator application.
		    def handleApply(binds: Bindings): BasicAtom = {
		      // Re-package the arguments with the correct properties.
		      val newargs = AtomSeq(params.props, newseq)
		      // See if we are bypassing the native handler.
		      if (!bypass) {
		        // Run any native handler.
		        if (handler.isDefined) return handler.get(this, newargs, binds)
		      }
		      // No native handler.
		      return OpApply(OperatorRef(this), newargs, binds)
		    }
        
		    // There are special cases to handle here.  First, if the argument list
		    // is empty, but there is an identity, return it.
		    if (newseq.length == 0) {
		      params.identity match {
		        case Some(ident) =>
		          // Return the identity.
		          return ident
		        case None =>
		          // No identity.  We're done.
		          return handleApply(Bindings())
//		          return OpApply(OperatorRef(this), AtomSeq(params.props, newseq),
//		              Bindings())
		      }
		    }
		    
		    // If the argument list is associative, we have an identity, and we
		    // have a single element, then that element must match the type of
		    // the operator, and we return it.  Why is this the rule?  We want
		    // to use associative operators to mimic "var args", but don't want
		    // them to "collapse" when there is just one argument.  That is, we
		    // don't want f(x)->x when we just want a var args f.  But if we give
		    // f an identity, it is probably a mathematical operator of some kind,
		    // and we probably do want f(x)->x.  So, for now, that's the rule.
		    // For greater control, you have to use a case operator.
		    if (newseq.length == 1) {
		      if (params.associative && params.identity.isDefined) {
		        // Get the atom.
		        val atom = newseq(0)
		        // Match the type of the atom against the type of the parameters.
		        val param = params(0)
		        param.tryMatch(atom) match {
		          case Fail(reason, index) =>
		            // The argument is invalid.  Reject!
					      throw new ArgumentListException("Incorrect argument " +
					      		"for operator " + toESymbol(name) + " at position 0: " +
					      		atom.toParseString + ".  " + reason())
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
		      for (index <- 0 until newseq.length) {
		        var param = Variable(atom.theType, ""+index)
		        newatoms = param +: newatoms
		      } // Build new parameter list.
		      newatoms
		    } else {
		      params.atoms
		    }
		    
		    // We've run out of special cases to handle.  Now just try to match the
		    // arguments against the parameters.
		    SequenceMatcher.tryMatch(newparams, newseq) match {
		      case Fail(reason, index) =>
			      throw new ArgumentListException("Incorrect argument for operator " +
			          toESymbol(name) + " at position " + index + ": " +
			          newseq(index).toParseString + ".  " + reason())
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
