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

import scala.collection.mutable.ListBuffer

/**
 * The common root for all application atoms.
 * 
 * An ''apply'' takes two atoms and applies the first (as an operator,
 * rewriter, etc.) to the second (as the argument).  Elements common across
 * all types are found here.
 * 
 * @param op		The left-hand element of the apply (operator).
 * @param arg		The right-hand element of the apply (argument).
 */
abstract class Apply(val op: BasicAtom, val arg: BasicAtom) extends BasicAtom {
  /** The apply is constant iff its parts are. */
  val isConstant = op.isConstant && arg.isConstant
  
  /** The constant pool aggregated from the children. */
  val constantPool = Some(BasicAtom.buildConstantPool(2, op, arg))
  
  /** The depth is one more than the depth of the children. */
  val depth = (op.depth max arg.depth) + 1
  
  /** The De Bruijn index computed from the children. */
  val deBruijnIndex = op.deBruijnIndex max arg.deBruijnIndex
  
  /** The hash code for this apply. */
  override lazy val hashCode = op.hashCode * 31 + arg.hashCode
  
  /**
   * Rewrite this apply with the bindings.  Each part is rewritten, and then
   * a new apply is generated.
   */
  def rewrite(binds: Bindings) = {
    val (nop, nof) = op.rewrite(binds)
    val (narg, naf) = arg.rewrite(binds)
    if (nof || naf) (Apply(nop, narg), true) else (this, false)
  }
  
  /**
   * By default applications match iff their parts match.  The trick here is
   * that the argument lists have to know what the top-level operator is in
   * order to successfully associatively match.
   */
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) =
    // Only applies match other applies.
    subject match {
      case Apply(oop, oarg) => {
        // Try to match the operators, and then the arguments.  If both match,
        // then this matches.  If not, then this does not match.
        op.tryMatch(oop, binds, hints) match {
          case fail: Fail =>
            Fail("Operators do not match.", this, subject, Some(fail))
          case Match(newbinds) =>
            // The operators match.  Now try to match the arguments.
            arg match {
              case al:AtomList => al.tryMatch(oarg, newbinds, Some(op))
              case _ => arg.tryMatch(oarg, newbinds, hints)
            }
            
          case Many(matches) =>
            // The operators match in multiple ways.  This seems unlikely, but
            // we consider it here anyway.
            Many(MatchIterator(arg.tryMatch(oarg, _, hints), matches))
        }
      }
      case _ => Fail("Applications only match other applications.",
          this, subject)
    }
}

/**
 * Provide construction and extraction for an ''apply''.  Deferred applications
 * are handled elsewhere.  See [[sjp.elision.core.DeferApply]].
 */
object Apply {
  /**
   * Extract the components of an apply and return them.
   * 
   * @param apply	The apply.
   * @return	A pair consisting of the operator and argument, in order.
   */
  def unapply(apply: Apply) = Some(apply.op, apply.arg)
  
  /**
   * Construct an application.  We key off the left hand side (the operator)
   * to decide how to handle this.
   * 
   * @param op			The lhs of the apply, typically an operator.
   * @param arg			The rhs of the apply, typically an argument.
   * @param bypass	If true, bypass native operator handler invocations.
   * @return	The basic atom resulting from the application.
   */
  def apply(op: BasicAtom, arg: BasicAtom, bypass: Boolean = false): BasicAtom = {
    op match {
      case oper:Operator =>
        // The lhs is an operator; this is a highly likely case.
      	opApply(oper, arg, bypass)
	    case app:Applicable =>
	      // The lhs is applicable; invoke its apply method.  This will return
	      // some atom, and that atom is the overall result.
	      app.doApply(arg)
	    case rew:Rewriter =>
	      // The lhs is a rewriter; invoke its rewrite method.  This will return
	      // a pair.  We need to convert the pair to a binding.
	      val (r_atom, r_flag) = rew.doRewrite(arg)
	      BindingsAtom(Bindings() +
	          ("atom" -> r_atom) +
	          ("flag" -> (if (r_flag) Literal.TRUE else Literal.FALSE)))
	    case _ =>
	      // The lhs is something else.  It may be a variable or some other
	      // expression that we have yet to evaluate.  Just build a simple
	      // apply of the lhs and rhs.
	      SimpleApply(op, arg)
    }
  }
  
  /**
   * Apply an operator to the given argument.  This is the correct way to
   * apply an operator.  The argument can be any atom, but if the argument
   * is an atom list, special things are done.
   * 
   * @param op	The operator.
   * @param arg	The argument.
   * @return	A new atom.
   */
  private def opApply(op: Operator, arg: BasicAtom,
      bypass: Boolean): BasicAtom = arg match {
    /*
     * How Operator Applications Get Created
     * 
     * Okay, so you come here to create an operator.  If the argument is not
     * an atom list, then the application is immediately created and returned.
     * No native handler or immediate rewrite is applied.
     * 
     * Otherwise you have to do some work.  In order to do this properly, you
     * have to know what the operator is, have its prototype and properties,
     * and do some processing.
     * 
     * This is how an operator application is handled.
		 * 
		 *  # If the argument is not an atom list, then the application is immediately
		 *    created and returned.  No native handlers are invoked and no immediate
		 *    rewrites are applied.
		 *  # If the argument is an atom list, then the atom list properties are
		 *    checked.  If they are set, they must match the operator's properties,
		 *    or an ArgumentListException is thrown.
		 *  # A new argument list is allocated with the same properties as the operator.
		 *  # Each argument in the original list is considered, and the following is
		 *    done.
		 *  # The arguments are matched against the parameter list.  If the match fails,
		 *    then an ArgumentListException is thrown.
		 *  # Any instances of an identity are discarded.
		 *  # If an absorber is found, it is immediately returned as the result.
		 *  # If no arguments are left, and the operator has an identity, then the
		 *    identity is returned as the result.
		 *  # If the operator is a native operator, and a closure has been registered,
		 *    then the new argument list is passed to the closure.  Note that this means
		 *    that the closure might not get invoked; for instance, if an absorber is
		 *    found.  The result of the closure is returned as the result.
		 *  # If no closure is present for a native operator, or if the operator is a
		 *    symbolic operator, then the operator prototype is rewritten with the
		 *    bindings from the parameter match.  The result is returned.
		 *  # Finally, for an immediate operator the bindings are applied to the
		 *    replacement, and the result is returned.
     */
    
    case oldlist:AtomList =>
      // We need to apply the operator to any child atom list.
      val al = AtomList(oldlist.atoms.map{
        _ match {
          case sublist: AtomList =>
            // Recursively apply the operator.
            apply(op, sublist)
          case atom: BasicAtom =>
            // Other elements do not get modified.
            atom 
        }
      }, oldlist.props)
      
      // What we do next depends on the type of operator definition.  The
      // native and symbolic definitions specify properties; the immediate
      // definition does not.
      // 
      // The following will compute the correct atom for the apply.
      op.opdef match {
        case NativeOperatorDefinition(_, props) =>
          // This is a native operator.
          val (assoc, comm) = al.props.getOrElse(props.associative,
              props.commutative)
          checkProto(op, props, al, assoc, comm, bypass)
        case SymbolicOperatorDefinition(_, props) =>
          // This is a symbolic operator.
          val (assoc, comm) = al.props.getOrElse(props.associative,
              props.commutative)
          checkProto(op, props, al, assoc, comm,bypass)
        case ImmediateOperatorDefinition(_, body) =>
          // Match the arguments against the formal parameters.  This will
          // allow us to rewrite the body with the resulting bindings to
          // obtain the final result.
          val bind = SequenceMatcher.tryMatch(op.opdef.proto.pars, al.atoms) match {
            case Fail(reason, index) =>
            	throw new ArgumentListException("Incorrect argument list at " +
            			"position " + index + ": " + reason())
            case Match(bnd) => bnd
            case Many(matches) => matches.next()
          }
          body.rewrite(bind)._1
      }
      
    case _ =>
      // If not an atom list, immediately make and return an apply.  Note that
      // we cannot use any native handler here.
      SimpleApply(op, arg)
  }
  
  /**
   * Check an argument atom list against the prototype for the operator.  If
   * they do not match, throw an exception.
   * 
   * The argument list is processed to flatten associative applications, and
   * to remove identities and look for absorbers.
   * 
   * Finally, the remaining arguments (if any) are checked to see if they
   * match the formal parameters.
   * 
   * @param op			The operator.
   * @param props		The operator properties.
   * @param al			The original atom list.
   * @param assoc		True iff associative.
   * @param comm		True iff commutative.
   * @param bypass	If true, bypass native constructor invocation.
   * @return	An argument list modified according to the specified properties.
   * @throws	ArgumentListException
   * 					The properties do not match.
   */
  private def checkProto(op: Operator, props: OperatorProperties, al: AtomList,
      assoc: Boolean, comm: Boolean, bypass: Boolean): BasicAtom = {
    // Check the properties and throw an exception if they do not match.
    if (props.associative && !assoc)
      throw new ArgumentListException("Non-associative argument list " +
      		"passed to associative operator.")
    else if (props.commutative && !comm)
      throw new ArgumentListException("Non-commutative argument list " +
      		"passed to commutative operator.")
    else if (assoc && !props.associative)
      throw new ArgumentListException("Associative argument list passed " +
      		"to non-associative operator.")
    else if (comm && !props.commutative)
      throw new ArgumentListException("Commutative argument list passed " +
      		"to non-commutative operator.")
    
    // If the operator is not associative, then we cannot change the number
    // of elements.  The only remaining item is to match the arguments against
    // the formal parameters.
    if (!assoc) {
      // To check the argument list, we match the prototype argument list
      // against the provided argument list.  If they match, then all is
      // well.  If they do not match, then we immediately throw an exception.
      // We need to capture and save the bindings of parameter to argument.
      val pabind = SequenceMatcher.tryMatch(op.opdef.proto.pars, al.atoms) match {
        case fail:Fail =>
          // The argument list does not match the formal parameters.
          throw new ArgumentListException(
              "Incorrect argument list at position " + fail.index + ": " +
              fail.theReason )
        case Match(binds) => binds
        case Many(iter) => iter.next
      }
      
      // The argument list matches.  Make and return the apply.
      if (bypass) {
        OpApply(op, al, pabind)
      } else { 
	      return op.handler match {
	        case Some(closure) => closure(op, al, pabind)
	        case None => OpApply(op, al, pabind)
	      }
      }
    }
    
    // After this point we are dealing with an associative operator.
    
    // Now check for identities and absorbers, and flatten any associative
    // applications.
    var newlist = IndexedSeq[BasicAtom]()
    val absorber = props.absorber.getOrElse(null)
    val identity = props.identity.getOrElse(null)
    for (atom <- al.atoms) {
		  // If this atom is an application of the same operator, then flatten
		  // the argument list.  This is the "parenthesized" list case:
		  // op(x,op(y,z)) becomes op(x,y,z).
      //
      // We also look for identities (which we skip) and absorbers.
		  atom match {
		    case Apply(oop, AtomList(args,_)) if oop == op =>
		      // Add the arguments directly to this list.  We can assume it has
		      // already been processed, so no deeper checking is needed.
		      newlist ++= args
		    case atom:BasicAtom if atom == absorber =>
		    	// This atom is an absorber.  We are done.
		      return absorber
		    case atom:BasicAtom if atom == identity =>
		      // Skip identities.
		    case _ =>
		    	// This atom is not the identity or an absorber.  Add it to the list.
		      newlist :+= atom
		  }
    }
    
    // If the list is empty, and there is an identity, then the identity is
    // the result.  Otherwise the answer is a new list.
    if (newlist.isEmpty && identity != null) return identity
    
    // If the list is a singleton, and there is an identity, then the answer
    // is the single element, whatever it is.
    if (newlist.length == 1 && identity != null) return newlist(0)
    
    // There are still arguments left.  These arguments must match the formal
    // parameters.  These two lists might be different sizes, however.  If the
    // argument list is shorter than the parameter list, it is doomed and we
    // give up.
    val pars = op.opdef.proto.pars
    val plen = pars.length
    val nlen = newlist.length
    if (nlen < plen) throw new ArgumentListException("Too few arguments (" +
        nlen + ") to operator " + op +
        " after processing.  Argument list is now: " +
        newlist.mkParseString("(",", ",")") + ".")

    // If there are equal numbers of parameters and arguments, great!  We can
    // just match normally.
    //
    // In the case that there are more arguments than parameters, we have to
    // do some modification to the parameter list.  We make a new list by
    // repeating the last parameter as many times as necessary, and then
    // performing the match on the new lists.
    var parameters = OmitSeq[BasicAtom]()
    parameters ++ pars
    val last = pars.last
    var count = 1
    while (parameters.length < newlist.length) {
      parameters :+= Variable(last.theType, "::"+count)
      count += 1
    }
    val pabind = SequenceMatcher.tryMatch(parameters, newlist) match {
      case fail:Fail =>
        // The argument list does not match the formal parameters.
        throw new ArgumentListException(
            "Incorrect argument list at position " + fail.index + ": " +
            fail.theReason )
      case Match(bind) => bind
      case Many(iter) => iter.next
    }

    // The argument list matches.  Make and return the apply.
    if (bypass) {
      return OpApply(op, AtomList(newlist, Some((assoc, comm))), pabind)
    } else { 
	    return op.handler match {
	      case Some(closure) =>
	        closure(op, AtomList(newlist, Some((assoc, comm))), pabind)
	      case None =>
	        OpApply(op, AtomList(newlist, Some((assoc, comm))), pabind)
	    }
    }
  }
}

/**
 * Provide a ''deferred apply''.  This is an apply that is only evaluated if it
 * is successfully rewritten.
 * 
 * == Structure and Syntax ==
 * Deferred applications are similar to regular applications, except that 
 * they are not evaluated until they are first successfully rewritten.  This
 * triggers constructing a regular apply.
 * 
 * The syntax for a deferred apply is identical to that of a regular apply,
 * except that two dots are used.
 * {{{
 * add:OPTYPE .. %?(\$x,\$y)
 * }}}
 * This will be transformed into a regular apply if either (or both) of the
 * variables are rewritten.
 * 
 * == Type ==
 * The type of a deferred apply is taken from the operator.  Since no final
 * bindings have happened, the type may be a variable, complicating matching.
 * 
 * == Equality and Matching ==
 * See the `equals` and `tryMatchWithoutTypes` methods.
 * 
 * @param op		The operator.
 * @param arg		The argument.
 */
case class DeferApply(override val op: BasicAtom, override val arg: BasicAtom)
extends Apply(op, arg) {
  /**
   * The type is taken from the operator.  Until we have evaluated the apply
   * we do not know the real type.  This can be a problem for matching.
   */
  val theType = op.theType
  
  /**
   * Applications are equal to each other iff their parts are equal.
   */
  override def equals(other: Any) = other match {
    case DeferApply(oop, oarg) => op.equals(oop) && arg.equals(oarg)
    case _ => false
  }
  
  /** The parse string for this deferred apply. */
  def toParseString = "(" + op.toParseString + ")..(" + arg.toParseString + ")"
}

/**
 * An ''operator apply''.  This is the common case of applying a known operator
 * to some argument list.
 * 
 * @param op			The operator.
 * @param arg			The argument list.
 * @param pabinds	The bindings from parameter name to argument.  Note that
 * 								if the operator is associative, some parameters may be
 * 								synthetic!
 */
case class OpApply(override val op: Operator, override val arg: AtomList,
    pabinds: Bindings) extends Apply(op, arg) {
  /**
   * Compute the type from the type specified by the operator, and the bindings
   * provided during parameter matching.  This allows rewriting otherwise
   * abstract type information to get a proper type.
   */
  val theType = op.opdef.proto.typ.rewrite(pabinds)._1
  
  /**
   * Applications are equal to each other iff their parts are equal.
   */
  override def equals(other: Any) = other match {
    case OpApply(oop, oarg, _) => op.equals(oop) && arg.equals(oarg)
    case _ => false
  }
  
  /**
   * Generate a parseable string.
   */
  def toParseString = toESymbol(op.name) + "(" + arg.toNakedString + ")"
}

case class SimpleApply(override val op: BasicAtom, override val arg: BasicAtom)
extends Apply(op, arg) {
  /**
   * We take the type from the operator.  This may be an incomplete type, but
   * we cannot rewrite it yet because we don't know the full bindings.  This
   * might cause trouble with matching.
   */
  val theType = op.theType
  
  /**
   * Applications are equal to each other iff their parts are equal.
   */
  override def equals(other: Any) = other match {
    case SimpleApply(oop, oarg) => op.equals(oop) && arg.equals(oarg)
    case _ => false
  }
  
  /**
   * Generate a parseable string.
   */
  def toParseString = "(" + op.toParseString + "." + arg.toParseString + ")"
}
