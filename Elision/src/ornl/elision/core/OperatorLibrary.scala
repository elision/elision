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
import scala.collection.mutable.{Map => MMap}
import scala.collection.immutable.List
import ornl.elision.util.ElisionException
import ornl.elision.util.Loc

/**
 * Indicate an attempt to re-define an already-known operator.
 * 
 * @param loc   The location of the new (replacement) operator.
 * @param msg		A human readable message.
 */
class OperatorRedefinitionException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * Indicate an attempt to improperly define an operator.
 * 
 * @param loc   The location of the bad operator definition.
 * @param msg		A human readable message.
 */
class OperatorDefinitionException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * A requested operator was not found, and could not be created.
 * 
 * @param loc   The location of the bad operator reference.
 * @param msg		A human readable message.
 */
class UndefinedOperatorException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * An operator library holds information about the known operators.  Operators
 * are managed by name, and two operators in the same library cannot have the
 * same name (but there can be multiple libraries).
 * 
 * == Native Operators ==
 * Handlers for native operators come here to register themselves.  Pass a
 * closure of the proper form (see `register`).
 * 
 * @param allowRedefinition	If true, allow redefinition of operators (madness)
 * 													as described above.  A warning is always generated!
 * 													This is true by default.
 */
class OperatorLibrary(val allowRedefinition: Boolean = true)
extends Fickle with Mutable {

  /**
   * The mapping from operator name to operator.  This holds the mapping as it
   * changes.
   */
 	private var _nameToOperator = MMap[String, OperatorRef]()
 	
 	// NOTE: We prepend to this list for performance. When reading for insertion
 	// order, be sure to use a reverseIterator
 	private var _opRefList = List[OperatorRef]()
 	
  /** Makes a copy of this operator library. */
  override def clone : OperatorLibrary = {
    val clone = new OperatorLibrary(this.allowRedefinition)
    
    clone._nameToOperator.clear
    for(mapping <- this._nameToOperator) {
        clone._nameToOperator += mapping
    }

    clone._opRefList = _opRefList
    clone
  }
    
 	/**
 	 * Turn the operator library into a sequence of newline-terminated strings
 	 * that are parseable by AtomParser.
 	 * 
 	 * @return	A parseable version of this instance.
 	 */
 	def toParseString =
 	  _nameToOperator.values.map(_.operator.toParseString).mkString("","\n","\n")
 	
 	/**
 	 * Get the named operator, if it is defined.  If not already defined, and
 	 * `allowUndefined` is true, then the operator is immediately defined as
 	 * described in the class comments.
 	 * 
 	 * @param name	The operator name.
 	 * @return	The operator, if known.
 	 */
 	def apply(name: String) = get(name) match {
 	  case None =>
 	    throw new UndefinedOperatorException(Loc.internal, "The operator " +
 	        toESymbol(name) + " is not known.")
 	    
 	  case Some(op) =>
 	    op
 	}
 	
 	/**
 	 * Get an operator by name.  If the operator is known, it is returned.
 	 * Otherwise None is returned.
 	 * 
 	 * @param name	The name of the operator.
 	 * @return	The optional operator.
 	 */
 	def get(name: String): Option[OperatorRef] = _nameToOperator.get(name) match {
 	  case some:Some[_] => some
 	  case _ => None
 	}
 	
 	/**
 	 * Get the complete list of operators.
 	 * 
 	 * @return  The list of all operators.
 	 */
 	def getAllOperators() = {
 	  var all = List[Operator]()
 	  for ((_, op) <- _nameToOperator) all :+= op.operator
 	  all
 	}
 	
 	/**
 	 * Add an operator to this library.
 	 * 
 	 * @param op		The operator to add.
 	 * @return	The operator just added, to enable chaining if desired.
 	 * @throws OperatorRedefinitionError
 	 * 					The operator is already defined and redefinitions are not allowed.
 	 */
 	def add(op: Operator) = {
 	  val name = op.name
 	  if (_nameToOperator.contains(name))
 	    if (allowRedefinition) {
 	      val oldop = _nameToOperator(name).operator
 	      warn(op.loc, "Redefining operator " + op.name + ".")
 	      warn(oldop.loc, "Prior definition: " + oldop.toParseString)
 	    } else {
 	    	// Reject this!  The operator is already defined.
 	    	throw new OperatorRedefinitionException(op.loc,
 	    			"Attempt to re-define known operator " + name + ".")
 	    }
    // Accept this and store it in the map.  Return the operator reference.
 	  val ref = OperatorRef(op)
 	  _opRefList = ref :: _opRefList
    _nameToOperator += (name -> ref)
 	  ref
 	}
 	
 	/**
 	 * Get a list of known operators, a short prototype, and their short
 	 * description.  Operators whose name starts with an underscore are
 	 * skipped.
 	 * 
 	 * @param app     The destination of the text.
 	 * @param width	  Width of the field.
 	 * @param apropos Only return operators whose description or name contains
 	 *                this string.
 	 */
 	def help(app: Appendable, width: Int, apropos: String = ""): Appendable = {
 	  // Iterate over the operators and compute the longest description.
 	  val dlength =
 	    _nameToOperator.values.foldLeft(0)(_ max _.operator.description.length)
 	    
 	  // Okay, now subtract that from the right margin.
 	  // If this is negative, we are SOL.  Just let the lines wrap.
 	  val dstart = (width - dlength - 1) max 2
 	  
 	  // Sort the operator names.
 	  val keyorder =
 	    _nameToOperator.
 	    keys.
 	    toList.
 	    sortWith(_.toLowerCase < _.toLowerCase).
 	    filter(!_.startsWith("_"))
 	  
 	  // Okay, now write details for each operator.
 	  val APROPOS = apropos.toUpperCase()
 	  keyorder.foreach {
 	    key =>
 	      val op = _nameToOperator(key).operator
 	      if (op.description.toUpperCase().indexOf(APROPOS) >= 0 ||
 	          key.toUpperCase().indexOf(APROPOS) >= 0) {
   	      app.append(' ').append(op.name).append(' ')
   	      val pos = op.name.length + 2
   	      if (pos >= dstart) {
   	        app.append("\n  ")
   	        app.append("."*(dstart-2))
   	      } else {
   	        app.append("."*(dstart-pos))
   	      }
   	      app.append(' ').append(op.description).append('\n')
 	      }
 	  }
 	  
 	  // Done.  The appendable is the value.
 	  app
 	}
 	
 	/**
 	 * Write out help for an operator, specified by operator reference.
 	 * 
 	 * @param app		Appendable to get output.
 	 * @param opref	The operator reference.
   * @param width The maximum width allowed.
 	 * @return	The appendable.
 	 */
 	def help(app: Appendable, opref: OperatorRef, width: Int): Appendable =
 	  help(app, opref.operator, width)
 	
 	/**
 	 * Write out help for an operator.
 	 * 
 	 * @param app		Appendable to get output.
 	 * @param opref	The operator.
 	 * @param width The maximum width allowed.
 	 * @return	The appendable.
 	 */
 	def help(app: Appendable, op: Operator, width: Int): Appendable = {
 	  // Write operator name and short description.
 	  app.append("Operator: " + op.name).append('\n')
 	  app.append(op.description).append("\n\n")
 	  // Construct the prototype.
 	  app.append("Prototype:\n")
 	  op match {
 	    case so:SymbolicOperator =>
		 	  app.append("  ").append(toESymbol(op.name))
		 	  app.append(so.params.mkParseString("(", ", ", "): "))
		 	  app.append(so.theType.toParseString).append('\n')
		 	  app.append("  ").append(so.params.props.toHumaneString).append('\n')
 	    case co:CaseOperator =>
      	app.append("  ").append(toESymbol(op.name))
      	app.append(" . (cases) : ").append(co.theType.toParseString)
      	app.append("\n\nCases:\n")
 	      for (cse <- co.cases) {
 	        app.append("  ").append(cse.toParseString).append('\n')
 	      } // Write cases.
 	  }
 	  app.append('\n')
 	  
 	  // Add the details.
 	  for (line <- new ornl.elision.util.Text().add(op.detail).wrap(width)) {
 	    app.append(line).append('\n')
 	  }
 	  
 	  // The appendable is the value.
 	  app
 	}
 	
 	// Get the well-known operators.
 	import OperatorLibrary._
 	
 	// Add the well-known operators.
 	import SymbolicOperator.{MAP, xx, LIST}
 	_nameToOperator += (MAP.name -> MAP)
 	_nameToOperator += (xx.name -> xx)
 	_nameToOperator += (LIST.name -> LIST)
}
