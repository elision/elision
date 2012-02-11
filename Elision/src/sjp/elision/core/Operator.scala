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
 * A type for all operators.
 */
object OPTYPE extends RootType {
  val theType = TypeUniverse
  
  val isConstant = true
  
  def toParseString = "OPTYPE"
    
  override def toString = "OPTYPE"
    
  override lazy val hashCode = toParseString.hashCode
}

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
  def apply(arg: BasicAtom) = arg match {
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
      // See if the atom list provides properties.
      al.props match {
        case None => // No properties specified; nothing to check.
        case Some((assoc,comm)) =>
          // The properties must match.  We also check for absorbers and
          // identities here.  We only do this for native and symbolic
          // definitions.
          opdef match {
            case NativeOperatorDefinition(_, props) =>
              checkProps(props, al, assoc, comm)
            case SymbolicOperatorDefinition(_, props) =>
              checkProps(props, al, assoc, comm)
            case _ =>
          }
      }
      Apply(this, arg)
    // If not an atom list, immediately make and return an apply.
    case _ => Apply(this, arg)
  }
  
  /**
   * Check the given properties against the operator definition.  If they do
   * not match, throw an exception.
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
  private def checkProps(props: OperatorProperties, al: AtomList,
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
      SequenceMatcher.tryMatch(opdef.proto.pars, al.atoms) match {
        case fail:Fail =>
          throw new ArgumentListException("Incorrect argument list: " + fail)
        case _ => Apply(this, al)
      }
    }
    
    // Now check for identities and absorbers, and flatten any associative
    // applications.
    val newlist = ListBuffer[BasicAtom]()
    val absorber = props.absorber.getOrElse(null)
    val identity = props.identity.getOrElse(null)
    for (atom <- al.atoms) {
		  // See if this atom is an absorber.  If so, we are done.
		  if (atom == absorber) return absorber
		  
		  // If this atom is an application of the same operator, then flatten
		  // the argument list.
		  atom match {
		    case Apply(op, AtomList(args,_)) if op == this =>
		      // Add the arguments directly to this list.  We can assume it has
		      // already been processed, so no deeper checking is needed.
		      newlist ++ args
		    case _ =>
		  }
		  
		  // See if this atom is not the identity.  If so, add it to the list.
		  if (atom != identity) newlist += atom
    }
    
    // If the list is empty, and there is an identity, then the identity is
    // the result.  Otherwise the answer is a new list.
    if (newlist.isEmpty && identity != null) return identity
    
    // There are still arguments left.  These arguments must match the formal
    // parameters.  For this to work, we need the argument lists to be the same
    // size.
    AtomList(newlist, Some((assoc, comm)))
  }
  
  override lazy val hashCode = opdef.hashCode
}
