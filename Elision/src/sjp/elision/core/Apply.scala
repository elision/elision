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
 * Provide a deferred apply.  This is an apply that is only evaluated if it
 * is successfully rewritten.
 * 
 * @param op		The operator.
 * @param arg		The argument.
 */
case class DeferApply(op: BasicAtom, arg: BasicAtom) extends BasicAtom {
  val theType = op.theType
  val isConstant = op.isConstant && arg.isConstant
  val depth = (op.depth max arg.depth) + 1
  val deBruijnIndex = op.deBruijnIndex max arg.deBruijnIndex
  def toParseString = "(" + op.toParseString + ")..(" + arg.toParseString + ")"
  override lazy val hashCode = op.hashCode * 31 + arg.hashCode
  override def equals(other: Any) = other match {
    case DeferApply(oop, oarg) => op.equals(oop) && arg.equals(oarg)
    case _ => false
  }
  def rewrite(binds: Bindings) = {
    val (nop, nof) = op.rewrite(binds)
    val (narg, naf) = arg.rewrite(binds)
    if (nof || naf) (Apply(nop, narg), true) else (this, false)
  }
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) = subject match {
    case DeferApply(oop, oarg) =>
      SequenceMatcher.tryMatch(Seq(op, arg), Seq(oop, oarg), binds)
    case _ => Fail("Deferred apply does not match.", this, subject)
  }
}

/**
 * An apply represents applying an operator to an argument.
 * 
 * ==Structure and Syntax==
 * There are two forms of apply.
 * 
 * The usual form in which an operator is applied to a list of arguments that
 * match formal parameters, written with the operator identified by a symbol,
 * and the arguments in parentheses juxtaposed with the operator.
 *  - `max(5,9)`
 *  
 * The second form is the more general form, where some atom is treated as
 * an operator, and applied to another atom.  These atoms may be anything.
 * The application is written by interposing a dot (.) between the two atoms.
 *  - `max.%(5,9)`
 *  - `\$op.\$arg`
 * 
 * The second form is right associative.
 *  - `x.y.z` = `x.(y.x)`
 *  
 * ==Type==
 * The type is taken from the operator.
 *  
 * ==Equality and Matching==
 * Two applies are equal iff their operator and argument are equal.
 * 
 * Two applies match if the respective operators and arguments match.
 * 
 * @param op		The operator.
 * @param arg		The argument.
 */
class Apply private (val op: BasicAtom, val arg: BasicAtom)
extends BasicAtom {
  /** The type is taken from the operator. */
  val theType = op match {
    case Operator(NativeOperatorDefinition(proto, _)) => proto.typ
    case Operator(SymbolicOperatorDefinition(proto, _)) => proto.typ
    case Operator(ImmediateOperatorDefinition(proto, _)) => proto.typ
    case _ => op.theType
  }
  
  /** An apply is constant iff both the operator and argument are constant. */
  val isConstant = op.isConstant && arg.isConstant
  
  /** The De Bruijn index is just the maximum of the operator and body. */
  val deBruijnIndex = op.deBruijnIndex.max(arg.deBruijnIndex)
  
  /** The depth is the maximum of the operator and body, plus one. */
  val depth = (op.depth max arg.depth) + 1

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    // Only applies match other applies.
    subject match {
      case Apply(oop, oarg) => {
        // Try to match the operators, and then the arguments.  If both match,
        // then this matches.  If not, then this does not match.
        op.tryMatch(oop, binds) match {
          case fail: Fail =>
            Fail("Operators do not match.", this, subject, Some(fail))
          case Match(newbinds) =>
            // The operators match.  Now try to match the arguments.
            arg.tryMatch(oarg, newbinds)
          case Many(matches) =>
            // The operators match in multiple ways.  This seems unlikely, but
            // we consider it here anyway.
            Many(new MatchIterator(arg.tryMatch(oarg, _), matches))
        }
      }
      case _ => Fail("Applications only match other applications.",
          this, subject)
    }

  def rewrite(binds: Bindings) = {
    val (newop, opchanged) = op.rewrite(binds)
    val (newarg, argchanged) = arg.rewrite(binds)
    if (opchanged || argchanged) (Apply(newop, newarg), true)
    else (this, false)
  }

  def toParseString =
    // If the operator is an actual operator instance, and if the argument is
    // an atom list, then we generate the "friendly" version of the apply.
    // Otherwise we generate the more general form of the apply.
    op match {
	    case actual:Operator =>
	      arg match {
	        case al:AtomList =>
	          toESymbol(actual.name) + "(" + al.toNakedString + ")"
	        case _ =>
	          "(" + op.toParseString + "." + arg.toParseString + ")"
	      }
	    case _ =>
	      "(" + op.toParseString + "." + arg.toParseString + ")"
	  }
  
  override def toString = "Apply(" + op.toString + ", " + arg.toString + ")"
  
  override lazy val hashCode = op.hashCode * 31 + arg.hashCode
  
  override def equals(other: Any) = other match {
    case Apply(oop, oarg) =>
      oop == op && oarg == arg
    case _ => false
  }
}

/**
 * Provide additional constructors for an apply.
 */
object Apply {
  /**
   * Construct an operator application, handling special cases.  This is the
   * correct place to come to apply "something" to "something else." Specific
   * cases handled are:
   * * Applying an operator to an argument of any kind.
   * * Currying a lambda.
   * * Applying a rewrite rule to an atom.
   * * Extracting a value from a binding set.
   * 
   * @param op			The operator.
   * @param arg			The argument.
   */
  def apply(op: BasicAtom, arg: BasicAtom): BasicAtom = {
    //println("Building an apply:")
    //println("  op -> " + op)
    //println(" arg -> " + arg)
	  op match {
	    case oper:Operator =>
	      // Applying an operator to an argument is a special, but probably usual,
	      // case.
	      opApply(oper, arg)
	    case Lambda(_, lvar, body) =>
	      // Curry the lambda body by binding the variable to the argument and then
	      // rewriting the body.
	      body.rewrite((new Bindings) + (lvar.name -> arg))._1
	    case strat:Strategy =>
	      // Try to apply the strategy.  Whatever we get back is the result.
	      strat.apply(arg, new Bindings())._1
	    case rule:RewriteRule =>
	      // Try to apply the rewrite rule.  Whatever we get back is the result.
	      //println("Rewriting with rule.")
	      rule.tryRewrite(arg)._1
	    case binds:BindingsAtom =>
	      // Try to rewrite the argument using the bindings and whatever we get
	      // back is the result.
	      arg.rewrite(binds)._1
	    case _ =>
	      //println("Rewriting vanilla.")
	      new Apply(op, arg)
	  } 
  }
  
  /**
   * Unpack an operator application.
   * @param apply	The application.
   * @return	The pair of operator and argument.
   */
  def unapply(apply: Apply) = Some(apply.op, apply.arg)
  
  /**
   * Apply an operator to the given argument.  This is the correct way to
   * apply an operator.  The argument can be any atom.
   * 
   * @param op	The operator.
   * @param arg	The argument.
   * @return	A new atom.
   */
  private def opApply(op: Operator, arg: BasicAtom): BasicAtom = arg match {
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
    
    case al:AtomList =>
      // What we do next depends on the type of operator definition.  The
      // native and symbolic definitions specify properties; the immediate
      // definition does not.
      // 
      // The following will compute the correct atom for the apply.
      op.opdef match {
        case NativeOperatorDefinition(_, props) =>
          val (assoc, comm) = al.props.getOrElse(props.assoc, props.comm)
          checkProto(op, props, al, assoc, comm)
        case SymbolicOperatorDefinition(_, props) =>
          val (assoc, comm) = al.props.getOrElse(props.assoc, props.comm)
          checkProto(op, props, al, assoc, comm)
        case ImmediateOperatorDefinition(_, body) =>
          // Match the arguments against the formal parameters.
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
      new Apply(op, arg)
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
   * @return	An argument list modified according to the specified properties.
   * @throws	ArgumentListException
   * 					The properties do not match.
   */
  private def checkProto(op: Operator, props: OperatorProperties, al: AtomList,
      assoc: Boolean, comm: Boolean): BasicAtom = {
    // Check the properties and throw an exception if they do not match.
    if (props.assoc && !assoc)
      throw new ArgumentListException("Non-associative argument list " +
      		"passed to associative operator.")
    else if (props.comm && !comm)
      throw new ArgumentListException("Non-commutative argument list " +
      		"passed to commutative operator.")
    else if (assoc && !props.assoc)
      throw new ArgumentListException("Associative argument list passed " +
      		"to non-associative operator.")
    else if (comm && !props.comm)
      throw new ArgumentListException("Commutative argument list passed " +
      		"to non-commutative operator.")
    
    // If the operator is not associative, then we cannot change the number
    // of elements.  The only remaining item is to match the arguments against
    // the formal parameters.
    if (!assoc) {
      // To check the argument list, we match the prototype argument list
      // against the provided argument list.  If they match, then all is
      // well.  If they do not match, then we immediately throw an exception.
      SequenceMatcher.tryMatch(op.opdef.proto.pars, al.atoms) match {
        case fail:Fail =>
          // The argument list does not match the formal parameters.
          throw new ArgumentListException(
              "Incorrect argument list at position " + fail.index + ": " +
              fail.theReason )
        case _ =>
          // The argument list matches.  Make and return the apply.
          return op.handler match {
            case Some(closure) => closure(op.name, al)
            case None => new Apply(op, al)
          }
      }
    }
    
    // After this point we are dealing with an associative operator.
    
    // Now check for identities and absorbers, and flatten any associative
    // applications.
    val newlist = ListBuffer[BasicAtom]()
    val absorber = props.absorber.getOrElse(null)
    val identity = props.identity.getOrElse(null)
    for (atom <- al.atoms) {
		  // If this atom is an application of the same operator, then flatten
		  // the argument list.  This is the "parenthesized" list case:
		  // op(x,op(y,z)) becomes op(x,y,z).
      //
      // We also look for identities (which we skip) and absorbers.
		  atom match {
		    case Apply(op, AtomList(args,_)) if op == this =>
		      // Add the arguments directly to this list.  We can assume it has
		      // already been processed, so no deeper checking is needed.
		      newlist ++ args
		    case atom:BasicAtom if atom == absorber =>
		    	// This atom is an absorber.  We are done.
		      return absorber
		    case atom:BasicAtom if atom == identity =>
		      // Skip identities.
		    case _ =>
		    	// This atom is not the identity or an absorber.  Add it to the list.
		      newlist += atom
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
    val parameters = ListBuffer[BasicAtom]()
    parameters ++ pars
    val last = pars.last
    var count = 1
    while (parameters.length < newlist.length) {
      parameters append Variable(last.theType, "::"+count)
      count += 1
    }
    SequenceMatcher.tryMatch(parameters, newlist) match {
      case fail:Fail =>
        // The argument list does not match the formal parameters.
        throw new ArgumentListException(
            "Incorrect argument list at position " + fail.index + ": " +
            fail.theReason )
      case _ =>
        // The argument list matches.  Make and return the apply.
        return op.handler match {
          case Some(closure) =>
            closure(op.name, AtomList(newlist, Some((assoc, comm))))
          case None => new Apply(op, AtomList(newlist, Some((assoc, comm))))
        }
    }
  }
}
 
object Op {
  /**
   * Pull an apply apart.  This only works in the case of an operator applied
   * to an atom list.
   * 
   * @param apply	The apply to dissect.
   * @return	The parts, as a sequence, starting with the operator name.
   */
  def unapplySeq(apply: Apply): Option[Seq[BasicAtom]] =
    apply match {
    case Apply(op:Operator, AtomList(atoms,_)) =>
      Some(Literal(STRING,op.name) +: atoms)
    case _ => None
  }
}
