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
 * = Undefined Operators =
 * 
 * You can set this class to allow undefined operators to be used.  When an
 * operator f that has not previously been defined is requested, a definition
 * is created at that point using the following prototype and properties.
 * 
 * {{{
 * operator { f(x: ^TYPE, y: ^TYPE): ^TYPE is associative }
 * }}}
 * 
 * This is borderline useless, and is only intended for testing and debugging.
 * Since you cannot modify the operator definition, and the above is probably
 * not what you want, there is little reason to use this "feature" and it may
 * be removed in the future.
 * 
 * = Native Operators =
 * 
 * Handlers for native operators come here to register themselves.  Pass a
 * closure that takes the argument list.
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
 	private var _nameToOperator = MMap[String, Operator]()
 	
 	/**
 	 * Turn the operator library into a sequence of newline-terminated strings
 	 * that are parseable by AtomParser.
 	 * 
 	 * @return	A parseable version of this instance.
 	 */
 	def toParseString =
 	  _nameToOperator.values.map(_.opdef.toParseString).mkString("","\n","\n")
 	  
 	/**
 	 * Turn the operator library into a sequence of newline-terminated strings
 	 * that are parseable as Scala.
 	 * 
 	 * @return	A parseable version of this instance.
 	 */
 	override def toString =
 	  _nameToOperator.values.map(_.opdef.toString).mkString("","\n","\n")
 	
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
 	 * @param handler	The handler.  It must take the operator name and the
 	 * 								argument list, post processing for identities and such,
 	 * 								and generate a new atom.
 	 */
 	def register(name: String,
 	    handler: (String, AtomList, Option[Bindings]) => BasicAtom) = {
 	  // Go fetch the operator.  It must be defined.
 	  _nameToOperator.get(name) match {
 	    case None =>
 	      warn("Operator " + name + " undeclared; ignoring native handler.")
 	    case Some(op) =>
 	      if (!op.opdef.isInstanceOf[NativeOperatorDefinition]) 
          warn("Operator " + name + " is not native; ignoring native handler.")
 	      op.handler = Some(handler)
 	  }
 	  this
 	}
 	
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
 	
 	/**
 	 * Get an operator by name.  If the operator is known, it is returned.  If
 	 * not, and if undefined operators are allowed, then the operator is
 	 * immediately defined and returned.  Otherwise None is returned.
 	 * @param name	The name of the operator.
 	 * @return	The optional operator.
 	 */
 	def get(name: String): Option[Operator] = _nameToOperator.get(name) match {
 	  case s:Some[_] => s
 	  case None if allowUndefined == true =>
 	    // Make the operator now.
 	    val od = SymbolicOperatorDefinition(
 	        OperatorPrototype(
 	        		name,
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
 	  if (_nameToOperator.contains(name))
 	    if (allowRedefinition)
 	      println("WARNING: Redefining operator " + od.proto.name)
 	    // Reject this!  The operator is already defined.
 	    else throw OperatorRedefinitionError(
 	        "Attempt to re-define known operator " + name + ".")
    // Accept this and store it in the map.  Return the definition.
    _nameToOperator += (name -> Operator(od))
 	  od
 	}
 	
 	// Get the well-known operators.
 	import OperatorLibrary._
 	
 	// Add the well-known operators.
 	_nameToOperator += (MapOperator.name -> MapOperator)
 	_nameToOperator += (CrossOperator.name -> CrossOperator)
}

object OperatorLibrary {
    
  //======================================================================
  // Universally-known operators.
  //======================================================================
  
 	/**
 	 * The well-known map operator.  Every time an operator library is created,
 	 * it should explicitly add this operator to itself.
 	 */
 	private val MapOperator = ProtoOperator(TypeUniverse,
 	    SymbolicOperatorDefinition(
 	        OperatorPrototype(
 	            "MAP",
		 	        List(Variable(ANYTYPE, "domain"),
		 	            Variable(ANYTYPE, "codomain")),
		 	        ANYTYPE),
		 	    OperatorProperties()))
  
 	/**
 	 * Construct a map type, given a domain and range.
 	 */
  def MAP(dom: BasicAtom, ran: BasicAtom) =
    Apply(MapOperator, AtomList(Seq(dom, ran)))

  /**
   * The well-known cross operator used to build operator types.
   */
  private val CrossOperator = ProtoOperator(TypeUniverse,
      SymbolicOperatorDefinition(
		      OperatorPrototype(
		          "xx",
		          List(Variable(ANYTYPE, "a")),
		          ANYTYPE),
		      OperatorProperties(assoc=true)))
  
  /**
   * Make a cross type.
   */
  def xx(atoms: List[BasicAtom]) =
    Apply(CrossOperator, AtomList(atoms))
}
