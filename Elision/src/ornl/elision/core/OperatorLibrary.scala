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
import ornl.elision.ElisionException

/**
 * Indicate an attempt to re-define an already-known operator.
 * 
 * @param msg		A human readable message.
 */
class OperatorRedefinitionException(msg: String) extends ElisionException(msg)

/**
 * Indicate an attempt to improperly define an operator.
 * 
 * @param msg		A human readable message.
 */
class OperatorDefinitionException(msg: String) extends ElisionException(msg)

/**
 * A requested operator was not found, and could not be created.
 * 
 * @param msg		A human readable message.
 */
class UndefinedOperatorException(msg: String) extends ElisionException(msg)

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
 * 													This is false by default.
 */
class OperatorLibrary(
    val allowRedefinition: Boolean = false) extends Fickle with Mutable {

  /**
   * The mapping from operator name to operator.  This holds the mapping as it
   * changes.
   */
 	private var _nameToOperator = MMap[String, OperatorRef]()
 	
 	/**
 	 * Turn the operator library into a sequence of newline-terminated strings
 	 * that are parseable by AtomParser.
 	 * 
 	 * @return	A parseable version of this instance.
 	 */
 	def toParseString =
 	  _nameToOperator.values.map(_.operator.toParseString).mkString("","\n","\n")
 	  
 	/**
 	 * Turn the operator library into a sequence of newline-terminated strings
 	 * that are parseable as Scala.
 	 * 
 	 * @return	A parseable version of this instance.
 	 */
 	override def toString =
 	  _nameToOperator.values.map(_.toString).mkString("","\n","\n")
 	
 	/**
 	 * Register a native handler for an operator.  The operator must already be
 	 * defined.  Note that the handler is not always invoked; only if
 	 * an operator is applied to an argument list is the handler invoked, and
 	 * then only if the argument list survives processing for the operator
 	 * properties (absorber, etc.).
 	 * 
 	 * @param name		The operator name.  This is always passed to the handler
 	 * 								as the first argument.  This makes it possible to write
 	 * 								a single handler for many similar operators.
 	 * @param handler	The handler.  It must take the operator and the argument
 	 * 								list, and will be passed the bindings resulting from
 	 * 								matching the parameters against the arguments.  It must
 	 * 								return a new atom.
 	 */
 	def register(name: String,
 	    handler: (Operator, AtomSeq, Bindings) => BasicAtom) = {
 	  // Go fetch the operator.  It must be defined.
 	  _nameToOperator.get(name) match {
 	    case None =>
 	      warn("Operator " + name + " undeclared; ignoring native handler.")
 	    case Some(op) =>
 	      if (!op.operator.isInstanceOf[SymbolicOperator]) 
          warn("Operator " + name + " is not symbolic; ignoring native handler.")
 	      op.operator.asInstanceOf[SymbolicOperator].handler = Some(handler)
 	  }
 	  this
 	}
 	
 	/**
 	 * Get the named operator, if it is defined.  If not already defined, and
 	 * `allowUndefined` is true, then the operator is immediately defined as
 	 * described in the class comments.
 	 * 
 	 * @param name	The operator name.
 	 * @return	The operator, if known.
 	 */
 	def apply(name: String) = get(name) match {
 	  case None => throw new UndefinedOperatorException(
 	      "The operator " + toESymbol(name) + " is not known.")
 	  case Some(op) => op
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
 	      warn("Redefining operator " + op.name + ".")
 	      warn("Prior definition: " + _nameToOperator(name).operator.toParseString)
 	    } else {
 	    	// Reject this!  The operator is already defined.
 	    	throw new OperatorRedefinitionException(
 	    			"Attempt to re-define known operator " + name + ".")
 	    }
    // Accept this and store it in the map.  Return the operator reference.
 	  val ref = OperatorRef(op)
    _nameToOperator += (name -> ref)
 	  ref
 	}
 	
 	/**
 	 * Get a list of known operators, a short prototype, and their short
 	 * description.  Operators whose name starts with an underscore are
 	 * skipped.
 	 * 
 	 * @param app		The destination of the text.
 	 * @param width	Width of the field.
 	 */
 	def help(app: Appendable, width: Int = 80) = {
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
 	  keyorder.foreach {
 	    key =>
 	      val op = _nameToOperator(key).operator
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
 	  
 	  // Done.  The appendable is the value.
 	  app
 	}
 	
 	/**
 	 * Write out help for an operator, specified by operator reference.
 	 * 
 	 * @param app		Appendable to get output.
 	 * @param opref	The operator reference.
 	 * @return	The appendable.
 	 */
 	def help(app: Appendable, opref: OperatorRef): Appendable =
 	  help(app, opref.operator)
 	
 	/**
 	 * Write out help for an operator.
 	 * 
 	 * @param app		Appendable to get output.
 	 * @param opref	The operator.
 	 * @return	The appendable.
 	 */
 	def help(app: Appendable, op: Operator) = {
 	  // Write operator name and short description.
 	  app.append("Operator: " + op.name).append('\n')
 	  app.append(op.description).append("\n\n")
 	  // Construct the prototype.
 	  app.append("Prototype:\n")
 	  op match {
 	    case so:SymbolicOperator =>
		 	  app.append("  ").append(toESymbol(op.name))
		 	  app.append(so.params.mkParseString("(", ", ", ")")).append('\n')
		 	  app.append("  ").append(so.params.props.toHumaneString).append('\n')
 	    case co:CaseOperator =>
      	app.append("  ").append(toESymbol(op.name))
      	app.append(" . (case)\n\nCases:\n")
 	      for (cse <- co.cases) {
 	        app.append("  ").append(cse.toParseString).append('\n')
 	      } // Write cases.
 	  }
 	  app.append('\n')
 	  
 	  // Add the details.
 	  app.append(op.detail).append('\n')
 	  
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
