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

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

/**
 * An incorrect argument list was supplied to an operator.
 * @param msg	The human-readable message describing the problem.
 */
class ArgumentListException(msg: String) extends Exception(msg)

/**
 * Encapsulate an operator.
 * 
 * ==Structure and Syntax==
 * 
 * ==Type==
 * 
 * ==Equality and Matching==
 * 
 * @param opdef	The operator definition.
 */
case class Operator(opdef: OperatorDefinition) extends BasicAtom {
  val theType = OPTYPE
  val isConstant = opdef.isConstant
  
  /** The native handler, if one is declared. */
  protected[core] var handler: (String,AtomList) => BasicAtom =
    (_, list:AtomList) => Apply(this, list)
  
  /** Provide quick access to the operator name. */
  lazy val name = opdef.proto.name
  
  val deBrujinIndex = 0

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    subject match {
      case Operator(oopdef) if opdef == oopdef => Match(binds)
      case _ => Fail("Operators do not match.", this, subject)
    }

  def rewrite(binds: Bindings) = (this, false)

  def toParseString = toESymbol(name) + ":OPTYPE"
  
  /**
   * Apply this operator to the given argument.  This is the correct way to
   * apply an operator.  The argument can be any atom.
   * @param arg	The argument.
   * @return	A new atom.
   */
  def apply(arg: BasicAtom): BasicAtom = arg match {
    /*
     * How Operator Applications Get Created
     * 
     * Okay, so you come here to create an operator.  If the argument is not
     * an atom list, then the application is immediately created and returned.
     * No native handler or immediate rewrite is applied.
     * 
     * Otherwise you have to do some work.  In order to do this properly, you
     * have to know what the operator is, have its prototype and properties,
     * and do some processing.  All those ingredients are present here, so
     * this is where the magic happens.
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
      opdef match {
        case NativeOperatorDefinition(_, props) =>
          val (assoc, comm) = al.props.getOrElse(props.assoc, props.comm)
          checkProto(props, al, assoc, comm)
        case SymbolicOperatorDefinition(_, props) =>
          val (assoc, comm) = al.props.getOrElse(props.assoc, props.comm)
          checkProto(props, al, assoc, comm)
        case ImmediateOperatorDefinition(_, body) =>
          // Match the arguments against the formal parameters.
          val bind = SequenceMatcher.tryMatch(opdef.proto.pars, al.atoms) match {
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
      Apply(this, arg)
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
   * @param props		The operator properties.
   * @param al			The original atom list.
   * @param assoc		True iff associative.
   * @param comm		True iff commutative.
   * @return	An argument list modified according to the specified properties.
   * @throws	ArgumentListException
   * 					The properties do not match.
   */
  private def checkProto(props: OperatorProperties, al: AtomList,
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
      SequenceMatcher.tryMatch(opdef.proto.pars, al.atoms) match {
        case fail:Fail =>
          // The argument list does not match the formal parameters.
          throw new ArgumentListException(
              "Incorrect argument list at position " + fail.index + ": " +
              fail.theReason )
        case _ =>
          // The argument list matches.  Make and return the apply.
          return handler(name, al)
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
    val pars = opdef.proto.pars
    val plen = pars.length
    val nlen = newlist.length
    if (nlen < plen) throw new ArgumentListException("Too few arguments (" +
        nlen + ") to operator " + this +
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
      parameters append Variable(last.theType, last.name+count)
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
        return handler(name, AtomList(newlist, Some((assoc, comm))))
    }
  }
  
  override lazy val hashCode = opdef.hashCode
  
  override def equals(other: Any) = other match {
    case op:Operator => opdef == op.opdef
    case _ => false
  }
}
