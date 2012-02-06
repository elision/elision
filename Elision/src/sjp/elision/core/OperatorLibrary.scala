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
import scala.collection.mutable.{Map => MMap}

/**
 * Indicate an attempt to re-define an already-known operator.
 * @param msg		A human readable message.
 */
case class OperatorRedefinitionError(msg: String) extends Exception(msg)

/**
 * A requested operator was not found, and could not be created.
 * @param msg		A human readable message.
 */
case class UndefinedOperatorException(msg: String) extends Exception(msg)

/**
 * An operator library holds information about the known operators.  Operators
 * are managed by name, and two operators in the same library cannot have the
 * same name (but there can be multiple libraries).
 * 
 * This is the place where instances of an operator are created.  Properties
 * are not checked, nor is any typechecking performed until the operator is
 * applied to some argument list.  Note that there is a trick here; since
 * an operator instance is immutable, only one instance is ever required for
 * an operator, and that instance is created and returned every time the
 * operator is requested.  So, if you need to use an operator many times, you
 * only have to get it once.
 * 
 * You can set this class to allow undefined operators to be used.  When an
 * operator f that has not previously been defined is requested, a definition
 * is created at that point using the following prototype and properties.
 * 
 * {{{
 * operator { f(x: ^TYPE, y: ^TYPE): ^TYPE is associative }
 * }}}
 * 
 * This allows the operator to take any number of arguments, including zero,
 * with any type.
 * 
 * @param allowUndefined		If true, allow undefined operators as described
 * 													above.  This is false by default.
 * @param allowRedefinition	If true, allow redefinition of operators (madness)
 * 													as described above.  A warning is always generated!
 * 													This is false by default.
 */
class OperatorLibrary(
    val allowUndefined: Boolean = false,
    val allowRedefinition: Boolean = false) {

  /**
   * The mapping from operator name to operator.  This holds the mapping as it
   * changes.
   */
 	var nameToOperator = MMap[String, Operator]()
 	
 	/**
 	 * Get the named operator, if it is defined.  If not already defined, and
 	 * `allowUndefined` is true, then the operator is immediately defined as
 	 * described in the class comments.
 	 * @param name	The operator name.
 	 * @return	The operator, if known.
 	 */
 	def apply(name: String) = get(name) match {
 	  case None => throw new UndefinedOperatorException(
 	      "The operator " + toESymbol(name) + " is not known.")
 	  case Some(op) => op
 	}
 	
 	def get(name: String): Option[Operator] = nameToOperator.get(name) match {
 	  case s:Some[Operator] => s
 	  case None if allowUndefined == true =>
 	    // Make the operator now.
 	    val od = SymbolicOperatorDefinition(
 	        OperatorPrototype(
 	        		name,
 	        		List[Variable](),
 	        		List(Variable(TypeUniverse, "x"), Variable(TypeUniverse, "y")),
 	        		TypeUniverse),
 	        OperatorProperties(assoc = true))
 	    add(od)
 	    get(name)
 	  case _ => None
 	}
 	
 	/**
 	 * Add an operator definition to this library.
 	 * @param od		The operator definition to add.
 	 * @return	The operator definition just added, to enable chaining if desired.
 	 * @throws OperatorRedefinitionError
 	 * 					The operator is already defined and redefinitions are not allowed.
 	 */
 	def add(od: OperatorDefinition) = {
 	  val name = od.proto.name
 	  if (nameToOperator.contains(name))
 	    if (allowRedefinition)
 	      println("WARNING: Redefining operator " + od.proto.name)
 	    // Reject this!  The operator is already defined.
 	    else throw OperatorRedefinitionError(
 	        "Attempt to re-define known operator " + name + ".")
    // Accept this and store it in the map.  Return the definition.
    nameToOperator += (name -> Operator(od))
 	  od
 	}
 	
 	// Certain operators are always defined.
 	add(SymbolicOperatorDefinition(
 	    OperatorPrototype(
 	        "MAP",
 	        List[Variable](),
 	        List(Variable(TypeUniverse, "domain"),
 	            Variable(TypeUniverse, "codomain")),
 	        TypeUniverse),
 	    OperatorProperties()))
}
