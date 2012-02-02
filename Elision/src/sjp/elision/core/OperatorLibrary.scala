/* Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 */
package sjp.elision.core
import scala.collection.mutable.{Map => MMap}

/**
 * Indicate an attempt to re-define an already-known operator.
 * @param msg		A human readable message.
 */
case class OperatorRedefinitionError(msg: String) extends Exception(msg)

/**
 * An operator library holds information about the known operators.  Operators
 * are managed by name, and two operators in the same library cannot have the
 * same name (but there can be multiple libraries).
 */
class OperatorLibrary {

  /**
   * The mapping from operator name to operator definition.  This holds the
   * mapping as it changes.
   */
 	var nameToOperator = MMap[String, OperatorDefinition]()
 	
 	/**
 	 * Get the named operator, if it is defined.
 	 * @param name	The operator name.
 	 * @return	The operator definition, if known.
 	 */
 	def apply(name: String) = nameToOperator.get(name)
 	
 	/**
 	 * Add an operator definition to this library.
 	 * @param od		The operator definition to add.
 	 * @return	The operator definition just added, to enable chaining if desired.
 	 */
 	def add(od: OperatorDefinition) = {
 	  val name = od.proto.name
 	  if (nameToOperator.contains(name))
 	    // Reject this!  The operator is already defined.
 	    throw OperatorRedefinitionError("Attempt to re-define known operator " +
 	        name + ".")
 	  else
 	    // Accept this and store it in the map.  Return the definition.
 	    nameToOperator += (name -> od)
 	  od
 	}
 	
 	
}