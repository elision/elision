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
package ornl.elision.parse

import java.nio.charset.Charset
import org.parboiled.scala.{ ANY => PANY }
import org.parboiled.scala._
import org.parboiled.errors.{ ParseError, ErrorUtils, ParsingException }
import scala.collection.mutable.LinkedList
import ornl.elision.core._
import ornl.elision.core.{ ANY => EANY }
import ornl.elision.ElisionException

/**
 * A special form was incorrectly formatted.
 * 
 * @param msg	The human-readable message.
 */
class SpecialFormException(msg: String) extends ElisionException(msg)

/**
 * Provide abstract syntax tree nodes used during parsing, along with any
 * supporting functions.
 */
object AtomParser {
	
	//----------------------------------------------------------------------
	// Parse result.
	//----------------------------------------------------------------------
  
  /** A parse result. */
  abstract sealed class Presult
  
  /**
   * The parse was successful.
   * 
   * @param nodes	The nodes parsed.
   */
  case class Success(nodes: List[AstNode]) extends Presult
  
  /**
   * The parse failed.
   * 
   * @param err	The reason for the parsing failure.
   */
  case class Failure(err: String) extends Presult

  //----------------------------------------------------------------------
  // Build character literals.
  //----------------------------------------------------------------------

  /**
   * Accumulate a character into a string.  The character may actually be an
   * escape sequence that will be interpreted here.
   *
   * See `toEString` for the reverse conversion.
   *
   * The following escapes are interpreted.
   * {{{
   * \"  -> double quotation mark
   * \`  -> backtick
   * \\  -> backslash
   * \n  -> newline
   * \t  -> tab
   * \r  -> carriage return
   * }}}
   *
   * @param front	The string to get the new character appended at the end.
   * @param last	The new character, possibly an escape to interpret.
   * @return	The new string.
   */
  def append(front: StringBuilder, last: String) = {
    // Interpret the last part.  If it is an escape, then convert it to a
    // character first.
    front.append(last match {
      case """\"""" => "\""
      case """\`""" => "`"
      case """\\""" => "\\"
      case """\n""" => "\n"
      case """\t""" => "\t"
      case """\r""" => "\r"
      case _ => last
    })
  }

  /**
   * Given a list of strings, each of which represents a single character
   * (possibly as an escape), concatenate them into a single string.
   *
   * @param chars	The list of characters.
   * @return	The composed string.
   */
  def construct(chars: List[String]) = {
    chars.foldLeft(new StringBuilder())(append(_, _)).toString
  }

  //======================================================================
  // Definitions for an abstract syntax tree for atoms.
  //----------------------------------------------------------------------
  // Here are all the myriad classes used to capture parts as they are 
  // parsed.  The basic idea is that each thing represented by an AstNode
  // must be interpretable as a BasicAtom, and this is the job of the
  // interpret method.
	//======================================================================
	
	/**
	 * An abstract syntax tree node resulting from parsing an atom.
	 */
	sealed abstract class AstNode {
	  /**
	   * Interpret this abstract syntax tree to generate an atom.
	   * @return	The generated atom.
	   */
	  def interpret: BasicAtom
	}
	
	//----------------------------------------------------------------------
	// Type nodes.
	//----------------------------------------------------------------------
	
	/**
	 * A node representing a simple type, which must be a root type.
	 * @param TYPE	The type.
	 */
	case class SimpleTypeNode(TYPE: NamedRootType) extends AstNode {
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "SimpleTypeNode.interpret") //rwNode.addChild("SimpleTypeNode.interpret")
			RWTree.current = interpretNode
			
			RWTree.addTo(interpretNode, TYPE) //interpretNode.addChild(TYPE)
			TYPE
		}
	}
	
	/**
	 * A node representing the unique type universe.
	 */
	case class TypeUniverseNode() extends AstNode {
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "TypeUniverseNode.interpret") //rwNode.addChild("TypeUniverseNode.interpret")
			RWTree.current = interpretNode
			
			RWTree.addTo(interpretNode, TypeUniverse) //interpretNode.addChild(TypeUniverse)
			TypeUniverse
		}
	}
	
	//----------------------------------------------------------------------
	// Operator reference nodes.
	//----------------------------------------------------------------------
	
	/**
	 * A node representing a "naked" operator.
	 * @param str	The operator name.
	 * @param lib	The operator library that will get the operator.
	 */
	case class OperatorNode(str: String, lib: OperatorLibrary) extends AstNode {
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "OperatorNode.interpret") //rwNode.addChild("OperatorNode.interpret")
			RWTree.current = interpretNode
			
			val result = lib(str)
			
			RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
			result
		}
	}
	
	//----------------------------------------------------------------------
	// Ruleset reference nodes.
	//----------------------------------------------------------------------

	/**
	 * A node representing a ruleset reference.
	 * @param str	The ruleset name.
	 * @param lib	The rule library that contains the ruleset.
	 */
	case class RulesetNode(str: String, lib: RuleLibrary) extends AstNode {
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "RulesetNode.interpret") //rwNode.addChild("RulesetNode.interpret")
			RWTree.current = interpretNode
			
			val result = lib(str)
			
			RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
			result
		}
	}
	
	//----------------------------------------------------------------------
	// Operator application nodes.
	//----------------------------------------------------------------------
	
	/**
	 * A node representing the application of an operator to an argument.
	 * @param context	The context.
	 * @param op			The operator.
	 * @param arg			The argument.
	 */
	case class ApplicationNode(context: Context, op: AstNode, arg: AstNode)
	extends AstNode {
	  def interpret = {
		// get the node representing this atom that is being rewritten
		val rwNode = RWTree.current
		val interpretNode = RWTree.addTo(rwNode, "ApplicationNode.interpret") //rwNode.addChild("ApplicationNode.interpret")
        
        val opNode = RWTree.addTo(interpretNode, "operator: ")
        RWTree.current = opNode
	    // If the operator is a naked symbol, we try to interpret it as an
	    // operator.  Otherwise we just interpret it.
	    val atom = op match {
	      case NakedSymbolNode(name) => context.operatorLibrary(name)
	      case SymbolLiteralNode(None, name) => context.operatorLibrary(name)
	      case _ => op.interpret
	    }
		RWTree.current = RWTree.addTo(opNode, atom) //interpretNode.addChild(atom)
		
        val argNode = RWTree.addTo(interpretNode, "argument: ")
        RWTree.current = argNode
        val argInt = arg.interpret
        
        RWTree.current = interpretNode
	    val result = Apply(atom, argInt)
		
		RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
		result
	  }
	}
	
  //----------------------------------------------------------------------
	// Lambda nodes.
  //----------------------------------------------------------------------

	/**
	 * A node representing a lambda.
	 * @param lvar	The lambda variable.
	 * @param body	The lambda body.
	 */
	case class LambdaNode(lvar: VariableNode, body: AstNode) extends AstNode {
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "LambdaNode.interpret") // rwNode.addChild("LambdaNode.interpret")
			RWTree.current = interpretNode

			val paramNode = RWTree.addTo(interpretNode, "parameter: ") // interpretNode.addChild("parameter: ")
			RWTree.current = paramNode
			val lvarInt = lvar.interpret
			
			val bodyNode = RWTree.addTo(interpretNode, "body: ") //interpretNode.addChild("body: ")
			RWTree.current = bodyNode
			val bodyInt = body.interpret
			
			RWTree.current = interpretNode
			val result = Lambda(lvarInt, bodyInt)
			
			RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
			result
		}
	}
	
  //----------------------------------------------------------------------
	// Atom collection nodes.
  //----------------------------------------------------------------------
	
	/**
	 * An abstract syntax tree node holding a simple list of atoms.
	 * 
	 * @param props	The properties of the list.
	 * @param list	The actual list of atom nodes.
	 */
	case class AtomSeqNode(props: AlgPropNode, list: List[AstNode])
	extends AstNode {
	  /**
	   * Properties of this list, if known.
	   */
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "AtomSeqNode.interpret") //rwNode.addChild("AtomSeqNode.interpret")
			RWTree.current = interpretNode
			
			val propsNode = RWTree.addTo(interpretNode, "properties: ") //interpretNode.addChild("properties: ")
			RWTree.current = propsNode
			val propsInt = props.interpret
			
			val listNode = RWTree.addTo(interpretNode, "atoms: ") //interpretNode.addChild("atoms: ")
			RWTree.current = listNode
			val ASList = list.toIndexedSeq[AstNode] map ( astAtom => {
                    RWTree.current = listNode
					val astAtomInt = astAtom.interpret
					astAtomInt
				}
			)
			
			RWTree.current = interpretNode
			val result = AtomSeq(propsInt, ASList)
			
			RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
			result
		}
	}
	
  //----------------------------------------------------------------------
	// Boolean literal nodes.
  //----------------------------------------------------------------------
	
	/** A true node for fast access. */
	case object TrueNode extends AstNode {
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "TrueNode.interpret") // rwNode.addChild("TrueNode.interpret")
			RWTree.current = interpretNode
			
			val result = Literal.TRUE
			
			RWTree.addTo(interpretNode, result) // interpretNode.addChild(result)
			result
		}
	}
	
	/** A false node for fast access. */
	case object FalseNode extends AstNode {
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "FalseNode.interpret") //rwNode.addChild("FalseNode.interpret")
			RWTree.current = interpretNode
			
			val result = Literal.FALSE
			
			RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
			result
		}
	}
	
	//----------------------------------------------------------------------
	// Algebraic properties nodes.
	//----------------------------------------------------------------------
	
	/** A property node. */
	sealed abstract class PropertyNode
	/** Associative property. */
	case class AssociativeNode(atom:AstNode) extends PropertyNode
	/** Commutative property. */
	case class CommutativeNode(atom:AstNode) extends PropertyNode
	/** Idempotent property. */
	case class IdempotentNode(atom:AstNode) extends PropertyNode
	/** Absorber property. */
	case class AbsorberNode(atom:AstNode) extends PropertyNode
	/** Identity property. */
	case class IdentityNode(atom:AstNode) extends PropertyNode
	
	/**
	 * A data structure holding operator properties as they are discovered during
	 * the parse.
	 */
	class AlgPropNode(props: List[PropertyNode]) extends AstNode {
	  /** An identity, if any. */
	  var withIdentity: Option[AstNode] = None
	  /** An absorber, if any. */
	  var withAbsorber: Option[AstNode] = None
	  /** True iff this operator is idempotent. */
	  var isIdempotent: Option[AstNode] = None
	  /** True iff this operator is commutative. */ 
	  var isCommutative: Option[AstNode] = None
	  /** True iff this operator is associative. */
	  var isAssociative: Option[AstNode] = None
	  
	  // Process any properties we were given.
	  for (prop <- props) prop match {
	    case AssociativeNode(an) => isAssociative = Some(an)
	    case CommutativeNode(cn) => isCommutative = Some(cn)
	    case IdempotentNode(in) => isIdempotent = Some(in)
	    case AbsorberNode(ab) => withAbsorber = Some(ab)
	    case IdentityNode(id) => withIdentity = Some(id)
	  }
	  
	  /** Print this properties object as a string. */
	  override def toString = interpret.toString
	  
	  private def _interpret(atom: Option[AstNode]) = atom match {
	    case None => 
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			rwNode.addChild("n/a")
			None
	    case Some(real) => Some(real.interpret)
	  }
	  
	  /**
	   * Convert this into an operator properties instance.
	   * @return	The operator properties object.
	   */
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "AlgPropNode.interpret") // rwNode.addChild("AlgPropNode.interpret")
			RWTree.current = interpretNode
			
			RWTree.current = RWTree.addTo(interpretNode, "associative: ") //interpretNode.addChild("associative: ")
			val assocInt = _interpret(isAssociative)
			RWTree.current = RWTree.addTo(interpretNode, "commutative: ") //interpretNode.addChild("commutative: ")
			val commuInt = _interpret(isCommutative)
			RWTree.current = RWTree.addTo(interpretNode, "idempotent: ") //interpretNode.addChild("idempotent: ")
			val idempInt = _interpret(isIdempotent)
			RWTree.current = RWTree.addTo(interpretNode, "absorber: ") //interpretNode.addChild("absorber: ")
			val absorInt = _interpret(withAbsorber)
			RWTree.current = RWTree.addTo(interpretNode, "identity: ") //interpretNode.addChild("identity: ")
			val identInt = _interpret(withIdentity)
			
			RWTree.current = interpretNode
			val result = AlgProp(assocInt, commuInt, idempInt, absorInt, identInt)
			
			RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
			result
		} 
	}

	/**
	 * Represent an algebraic properties specification.
	 */
	object AlgPropNode {
	  /** Make a new, empty specification. */
	  def apply() = new AlgPropNode(List())
	  /**
	   * Make a new specification from the provided property nodes.
	   * 
	   * @param list		The property nodes.
	   */
	  def apply(list: List[PropertyNode]) = new AlgPropNode(list)
	}

	//----------------------------------------------------------------------
	// Object and binding nodes.
	//----------------------------------------------------------------------

	/**
	 * Represent a bindings atom.
	 * 
	 * @param map	The bindings.
	 */
	case class BindingsNode(map: List[(NakedSymbolNode,AstNode)]) extends AstNode {
	  def interpret = {
		// get the node representing this atom that is being rewritten
		val rwNode = RWTree.current
		val interpretNode = RWTree.addTo(rwNode, "BindingsNode.interpret") //rwNode.addChild("BindingsNode.interpret")
		RWTree.current = interpretNode
			
	    var binds = Bindings()
	    for ((str,node) <- map) {
			val bindNode = RWTree.addTo(interpretNode, str + " -> ") //interpretNode.addChild(str + " -> ")
			RWTree.current = bindNode
			val nodeInt = node.interpret
			binds += (str.str -> nodeInt)
	    }
		
		RWTree.current = interpretNode
	    val result = BindingsAtom(binds)
		
		RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
		result
	  }
	}
	
	//----------------------------------------------------------------------
	// Map pair nodes.
	//----------------------------------------------------------------------
	
	/**
	 * Represent a map pair atom.
	 * 
	 * @param left	The left atom.
	 * @param right	THe right atom.
	 */
	case class MapPairNode(left: AstNode, right: AstNode) extends AstNode {
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "MapPairNode.interpret") //rwNode.addChild("MapPairNode.interpret")
			RWTree.current = interpretNode

			val leftNode = RWTree.addTo(interpretNode, "left: ") //interpretNode.addChild("left: ")
			RWTree.current = leftNode
			val leftInt = left.interpret
			
			val rightNode = RWTree.addTo(interpretNode, "right: ") //interpretNode.addChild("right: ")
			RWTree.current = rightNode
			val rightInt = right.interpret
			
			RWTree.current = interpretNode
			val result = MapPair(leftInt, rightInt)
			
			RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
			result
		}
	}
	
	//----------------------------------------------------------------------
	// Symbol nodes.
	//----------------------------------------------------------------------
	
	/**
	 * A node representing a "naked" symbol: a symbol whose type is the type
	 * `ANY`.
	 * 
	 * @param str	The symbol text.
	 */
	case class NakedSymbolNode(str: String) extends AstNode {
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "NakedSymbolNode.interpret") //rwNode.addChild("NakedSymbolNode.interpret")
			RWTree.current = interpretNode
			
			val result = SymbolLiteral(SYMBOL, Symbol(str))
			
			RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
			result
		}
	}
	
	/**
	 * A node representing a symbol.
	 * 
	 * @param typ		The type.
	 * @param name	The symbol text.
	 */
	case class SymbolNode(typ: AstNode, name: String) extends AstNode {
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "SymbolNode.interpret") //rwNode.addChild("SymbolNode.interpret")
			RWTree.current = interpretNode
			
			val typeInt = typ.interpret
			RWTree.addTo(interpretNode, typeInt) //interpretNode.addChild(typeInt)
			
			val result = Literal(typeInt, name)
			
			RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
			result
		}
	}
	
	/** A node representing ANY. */
	object AnyNode extends AstNode {
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "AnyNode.interpret") //rwNode.addChild("AnyNode.interpret")
			RWTree.current = interpretNode
			
			val result = EANY
			
			RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
			result
		}
	}
	
	//----------------------------------------------------------------------
	// Variable nodes.
	//----------------------------------------------------------------------
	
	/**
	 * A node representing a variable reference.
	 * 
	 * @param typ			The type.
	 * @param name		The variable name.
	 * @param guard		The variable's guard.
	 * @param labels	Labels associated with the variable.
	 */
	class VariableNode(val typ: AstNode, val name: String,
	    val grd: Option[AstNode], val labels: Set[String]) extends AstNode {
	  def interpret: Variable = {
		// get the node representing this atom that is being rewritten
		val rwNode = RWTree.current
		val interpretNode = RWTree.addTo(rwNode, "VariableNode.interpret") //rwNode.addChild("VariableNode.interpret")
		RWTree.current = interpretNode
		
		val typeInt = typ.interpret
		
		grd match {
			case None => 
				val result = Variable(typeInt, name, Literal.TRUE, labels)
				
				RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
				result
			case Some(guard) => 
				val guardNode = RWTree.addTo(interpretNode, "guard: ") //interpretNode.addChild("guard: ")
				RWTree.current = guardNode
				val guardInt = guard.interpret
				
				RWTree.current = interpretNode
				val result = Variable(typeInt, name, guardInt, labels)
				
				RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
				result
		}
	  }
	}
	
	/**
	 * Simplified creation of variable nodes.
	 */
	object VariableNode {
	  /**
	   * Make a new variable node.
	   * 
		 * @param typ			The type.
		 * @param name		The variable name.
		 * @param guard		The variable's guard.
		 * @param labels	Labels associated with the variable.
	   */
	  def apply(typ: AstNode, name: String, guard: Option[AstNode],
	    labels: Set[String]) = new VariableNode(typ, name, guard, labels)
	}
	
	/**
	 * A node representing a metavariable reference.
	 * 
	 * @param vx	An ordinary variable node to promote.
	 */
	case class MetaVariableNode(vx: VariableNode)
	extends VariableNode(vx.typ, vx.name, vx.grd, vx.labels) {
	  override def interpret: MetaVariable = {
		// get the node representing this atom that is being rewritten
		val rwNode = RWTree.current
		val interpretNode = RWTree.addTo(rwNode, "MetaVariableNode.interpret") // rwNode.addChild("MetaVariableNode.interpret")
		RWTree.current = interpretNode
		
		val vxtypeInt = vx.typ.interpret
		
		grd match {
			case None =>
				val result = MetaVariable(vxtypeInt, vx.name, Literal.TRUE, labels)
			  
				RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
				result
			case Some(guard) =>
				val guardNode = RWTree.addTo(interpretNode, "guard: ") //interpretNode.addChild("guard: ")
				RWTree.current = guardNode
				val guardInt = guard.interpret
				
				RWTree.current = interpretNode
				val result = MetaVariable(vxtypeInt, vx.name, guard.interpret, labels)
			  
				RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
				result
		}
	  }
	}
	
	//----------------------------------------------------------------------
	// Special form node.
	//----------------------------------------------------------------------
	
	/**
	 * A node representing a general special form.
	 * 
	 * @param tag			The tag identifying the special form.
	 * @param content	The content portion of the special form.
	 */
	case class SpecialFormNode(tag: AstNode, content: AstNode) extends AstNode {
	  override def interpret: BasicAtom = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "SpecialFormNode.interpret") //rwNode.addChild("SpecialFormNode.interpret")
			RWTree.current = interpretNode
			
			val tagNode = RWTree.addTo(interpretNode, "tag: ") //interpretNode.addChild("tag: ")
			RWTree.current = tagNode
			val tagInt = tag.interpret
			
			val contentNode = RWTree.addTo(interpretNode, "content: ") //interpretNode.addChild("content: ")
			RWTree.current = contentNode
			val contentInt = content.interpret
			
			RWTree.current = interpretNode
			val result = SpecialForm(tagInt, contentInt)
			
			RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
			result
		}
	}
	
	//----------------------------------------------------------------------
	// Literal nodes - that is, nodes that hold literal values.
	//----------------------------------------------------------------------
	
	/**
	 * A node representing a symbol literal.
	 * 
	 * @param typ		The type.
	 * @param sym		The symbol text.
	 */
	case class SymbolLiteralNode(typ: Option[AstNode], sym: String) extends AstNode {
	  def interpret: BasicAtom = {
		// get the node representing this atom that is being rewritten
		val rwNode = RWTree.current
		val interpretNode = RWTree.addTo(rwNode, "SymbolLiteralNode.interpret") //rwNode.addChild("SymbolLiteralNode.interpret")
		RWTree.current = interpretNode
			
	    // If there is no type, or the type is the type universe, then check the
	    // symbol to see if it is a known root type.  This is also where the
	    // symbol _ gets turned into ANY.
	    if (typ == None || typ.get.isInstanceOf[TypeUniverseNode]) {
	      val lookup = (if (sym == "_") "ANY" else sym)
	      NamedRootType.get(lookup) match {
	        case Some(nrt) => 
				RWTree.addTo(interpretNode,nrt) //interpretNode.addChild(nrt)
				return nrt
	        case _ =>
	      }
	    }
        
	    RWTree.current = interpretNode
	    // There are interesting "untyped" cases.  Without type, true and false
	    // should be made Booleans, and Nothing should have type ANY.
	    if (typ == None) sym match {
	      case "true" => 
				RWTree.addTo(interpretNode, Literal.TRUE) // interpretNode.addChild(Literal.TRUE)
				return Literal.TRUE
	      case "false" => 
				RWTree.addTo(interpretNode, Literal.FALSE) //interpretNode.addChild(Literal.FALSE)
				return Literal.FALSE
	      case _ => 
                val newLit = Literal(SYMBOL, Symbol(sym))
                RWTree.addTo(interpretNode, newLit)
                return newLit
	    } else {
	      typ.get.interpret match {
	        case BOOLEAN if sym == "true" => 
				RWTree.addTo(interpretNode, Literal.TRUE) //interpretNode.addChild(Literal.TRUE)
				return Literal.TRUE
	        case BOOLEAN if sym == "false" => 
				RWTree.addTo(interpretNode, Literal.FALSE) //interpretNode.addChild(Literal.FALSE)
				return Literal.FALSE
	        case t:Any => 
				RWTree.addTo(interpretNode, t) //interpretNode.addChild(t)
				
                RWTree.current = interpretNode
				val result = Literal(t, Symbol(sym))
				
				RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
				return result
	      }
	    }
	  }
	}
	
	/**
	 * A node representing a string literal.
	 * 
	 * @param typ		The type.
	 * @param str		The string text.
	 */
	case class StringLiteralNode(typ: AstNode, str: String) extends AstNode {
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "StringLiteralNode.interpret") //rwNode.addChild("StringLiteralNode.interpret")
			RWTree.current = interpretNode
			
			val typeInt = typ.interpret
			
			val result = Literal(typeInt, str)
			
			RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
			result
		}
	}
	
	//----------------------------------------------------------------------
	// Numeric literal value nodes.
	//----------------------------------------------------------------------
	
	/**
	 * Root of all numeric abstract syntax tree nodes.
	 */
	abstract class NumberNode extends AstNode {
	  /**
	   * Make a new version of this node, with the specified type.
	   * 
	   * @param newtyp		The new type.
	   * @return	A new node with the given type.
	   */
	  def retype(newtyp: AstNode): NumberNode
	}
	
	/**
	 * An abstract syntax tree node holding a numeric value.
	 * 
	 * @param sign			True if positive, false if negative.
	 * @param integer		The integer portion of the number.
	 * @param fraction	The fractional portion of the number, if any.
	 * @param exponent	The exponent, if any.
	 * @param typ				The overriding type.  Otherwise it is inferred.
	 */
	object NumberNode {
	  /**
	   * Make a new node of the appropriate form to hold a number.
	   * 
	   * The number is constructed as follows.
	   * <code>[sign] [integer].[fraction] e [exponent]</code>
	   * The radix for the result is taken from the integer portion.
	   * 
	   * @param sign			If false, the number is negative.  Otherwise positive.
	   * @param integer		The integer portion of the number.
	   * @param fraction	The fractional portion of the number.  By default, none.
	   * @param exponent	The exponent.  By default, zero.
	   * @param typ				The type for the number.
	   * @return	The new number node.
	   */
	  def apply(sign: Option[Boolean], integer: UnsignedIntegerNode,
	      fraction: Option[UnsignedIntegerNode],
	      exponent: Option[SignedIntegerNode],
	      typ: Option[AstNode] = None): NumberNode = {
	    // Make the sign concrete.
	    val theSign = sign.getOrElse(true)
	    // If there is neither fraction nor exponent, then this is just an integer.
	    // Otherwise it is a float.
	    if (fraction.isDefined || exponent.isDefined) {
	      // This is a float.  Get the parts.
	      val fracpart = (if (fraction.isDefined) fraction.get.digits else "")
	      val exppart = exponent.getOrElse(SignedIntegerNode.Zero)
	      FloatNode(theSign, integer.digits, fracpart, integer.radix, exppart)
	    } else {
	      SignedIntegerNode(theSign, integer.digits, integer.radix)
	    }
	  }
	}
	
	/**
	 * An abstract syntax tree node holding an unsigned integer.  The integer
	 * value should be regarded as positive.
	 * 
	 * @param digits	The digits of the number.
	 * @param radix		The radix.
	 * @param typ			The type.  If not specified, INTEGER is used.
	 */
	case class UnsignedIntegerNode(digits: String, radix: Int,
	    typ: AstNode = SimpleTypeNode(INTEGER)) extends NumberNode {
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "UnsignedIntegerNode.interpret") //rwNode.addChild("UnsignedIntegerNode.interpret")
			RWTree.current = interpretNode
			
			val typeInt = typ.interpret
			
			val result = Literal(typeInt, asInt)
			
			RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
			RWTree.current = rwNode
			result
		}
	  
	  /** Get the unsigned integer as a positive native integer value. */
	  lazy val asInt = BigInt(digits, radix)

	  def retype(newtyp: AstNode) = UnsignedIntegerNode(digits, radix, newtyp)
	}
	
	/**
	 * An abstract syntax tree node holding a signed integer.
	 * @param sign		If true, positive, and if false, negative.
	 * @param digits	The digits of the number.
	 * @param radix		The radix.
	 * @param typ			The type.  If not specified, INTEGER is used.
	 */
	case class SignedIntegerNode(sign: Boolean, digits: String, radix: Int,
	    typ: AstNode = SimpleTypeNode(INTEGER)) extends NumberNode {
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "SignedIntegerNode.interpret") //rwNode.addChild("SignedIntegerNode.interpret")
			RWTree.current = interpretNode
			
			val typeInt = typ.interpret
			
			val result = Literal(typeInt, asInt)
			
			RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
			result
		}
	  
	  /** Get the signed integer as a signed native integer value. */
	  lazy val asInt = if (sign) asUInt else -asUInt
	  /** Get the integer as a positive native integer value. */
	  lazy val asUInt = BigInt(digits, radix)
	  
	  def retype(newtyp: AstNode) = SignedIntegerNode(sign, digits, radix, newtyp)
	}
	
	/**
	 * Provide other methods to construct a signed integer.
	 */
	object SignedIntegerNode {
	  /** Zero. */
	  val Zero = SignedIntegerNode(true, "0", 10)
	  /** One. */
	  val One = SignedIntegerNode(true, "1", 10)
	  
	  /**
	   * Create a new signed integer from the given unsigned integer.  The radix
	   * is the same as the signed integer.
	   * 
	   * @param sign		If true or None, positive.  If false, negative.
	   * @param integer	The unsigned integer value.
	   * @param typ			The type.
	   */
	  def apply(sign: Option[Boolean], integer: UnsignedIntegerNode,
	      typ: AstNode): SignedIntegerNode =
	    sign match {
	      case Some(bool) => SignedIntegerNode(bool, integer.digits, integer.radix,
	          typ)
	      case None => SignedIntegerNode(true, integer.digits, integer.radix, typ)
	    }
	  
	  /**
	   * Create a new signed integer from the given unsigned integer.  The radix
	   * is the same as the signed integer.
	   * 
	   * @param sign		If true or None, positive.  If false, negative.
	   * @param integer	The unsigned integer value.
	   */
	  def apply(sign: Option[Boolean],
	      integer: UnsignedIntegerNode): SignedIntegerNode =
	    SignedIntegerNode(sign, integer, SimpleTypeNode(INTEGER))
	}
	
	/**
	 * An abstract syntax tree node holding a floating point value.
	 * 
	 * @param sign			True if positive, false if negative.
	 * @param integer		The integer portion's digits.
	 * @param fraction	The fractional portion's digits.
	 * @param radix			The radix for the integer and fraction.
	 * @param exp				The exponent.
	 * @param typ				The type.  If not specified, FLOAT is used.
	 */
	case class FloatNode(sign: Boolean, integer: String, fraction: String,
	    radix: Int, exp: SignedIntegerNode, typ: AstNode = SimpleTypeNode(FLOAT))
	    extends NumberNode {
	  // We need to modify the integer and fraction parts to create the proper
	  // significand.  This is done as follows.  If there are n digits in the
	  // fraction, then we need to subtract n from the exponent.  Now, to do the
	  // math we have to convert the exponent into an actual integer, then do the
	  // math, and then convert it back to the proper number in the correct radix.
	  // We do all that now.
	  
	  /**
	   * The normalized version of this float.  It is a pair consisting of the
	   * significand and exponent.  Note that the two may use different radices.
	   */
	  lazy val norm = {
	    // Correct the significand by adding the integer and fractional part
	    // together.  This looks odd, but remember that they are still strings.
	    val significand = integer + fraction
	    // Now get the exponent, and adjust it to account for the fractional part.
	    // Since the decimal moves right, we subtract from the original exponent.
	    var exponent = exp.asInt
	    exponent -= fraction.length
	    // The sign of the exponent might change, so we recompute the sign here.
	    // We separate the sign and the exponent, so we also need the unsigned
	    // version of the exponent.
	    val newsign = exponent >= 0
	    exponent = exponent.abs
	    // Construct the new exponent as a signed integer node.  We use the same
	    // radix as the significand.
	    val newexp = SignedIntegerNode(newsign,
	        exponent.toString(exp.radix), exp.radix)
	    // Done.  Return the significand and exponent.  Note these two might have
	    // different radix.
	    (SignedIntegerNode(sign, significand, radix), newexp)
	  }
	  
	  /**
	   * The simple triple representation of this float.  It consists of the
	   * significand, the exponent, and the preferred radix (from the significand).
	   */
	  lazy val asTriple = (norm._1.asInt, norm._2.asInt, radix)
	  
	  /**
	   * Compute the platform float representation of this number.  This is not
	   * very likely to be useful, but might be very good for debugging.
	   */
	  lazy val asFloat = norm._1.asInt * BigInt(radix).pow(norm._2.asInt.toInt)
	  
	  def interpret = {
			// get the node representing this atom that is being rewritten
			val rwNode = RWTree.current
			val interpretNode = RWTree.addTo(rwNode, "FloatNode.interpret") //rwNode.addChild("FloatNode.interpret")
			RWTree.current = interpretNode
			
			val typeInt = typ.interpret
			
			val result = Literal(typeInt, norm._1.asInt.toInt, norm._2.asInt.toInt, radix)
			
			RWTree.addTo(interpretNode, result) //interpretNode.addChild(result)
			result
		}
	  
	  def retype(newtyp: AstNode) = FloatNode(sign, integer, fraction, radix, exp,
	      newtyp)
	}
}

//======================================================================
// Parse and build atoms.
//======================================================================

/**
 * A parser to parse a single atom.
 * 
 * TODO Remove the use of toggle and prefer a configuration option.
 * 
 * @param context	The context for rulesets and operators.
 * @param trace 	If true, enable tracing.  Off by default.
 * @param toggle  If true, select the new (parser combinator) parser.
 */
class AtomParser(val context: Context, val trace: Boolean = false,
    toggle: Boolean = false)
  extends Parser with Fickle {
  import AtomParser._

  //----------------------------------------------------------------------
  // Perform parsing.
  //----------------------------------------------------------------------

  /**
   * Entry point to parse all atoms from the given string.
   * 
   * @param line	The string to parse.
   * @return	The parsing result.
   */
  def parseAtoms(line: String): Presult = {
    if (toggle) {
      val parser = new ParseCombinators(context)
      
      import scala.util.parsing.combinator.Parsers
      parser.run(line) match {
        case parser.Success(list, _) => Success(list.asInstanceOf[List[AstNode]])
        case parser.NoSuccess(msg, _)	 => Failure(msg)
      }
    } else {      
    	val tr =
    			if (trace) TracingParseRunner(AtomSeq)
    			else ReportingParseRunner(AtomSeq)
		val parsingResult = tr.run(line)
		parsingResult.result match {
		case Some(nodes) => Success(nodes)
		case None => Failure("Invalid MPL2 source:\n" +
				ErrorUtils.printParseErrors(parsingResult))
		}
    }
  }
    
  /**
   * Entry point to parse all atoms from the given source.
   * 
   * @param line	The string to parse.
   * @return	The parsing result.
   */
  // FIXME: Currently only supports the parboiled parser
  def parseAtoms(source: scala.io.Source): Presult = {
    val tr =
      if (trace) TracingParseRunner(AtomSeq)
      else ReportingParseRunner(AtomSeq)
    val parsingResult = tr.run(source)
    parsingResult.result match {
      case Some(nodes) => Success(nodes)
      case None => Failure("Invalid MPL2 source:\n" +
          ErrorUtils.printParseErrors(parsingResult))
    }
  }

  //----------------------------------------------------------------------
  // Parse and build atoms.
  //----------------------------------------------------------------------
  // The basic approach is the following.
  //
  // We parse a sequence of atoms (so we parse everything in a line) using
  // the AtomSeq rule.
  // 
  // Because we want the applicative dot (.) to bind with the least precedence,
  // we next parse it in Atom.
  //
  // Most atoms are parsed with FirstAtom, including allowing parenthesized
  // atoms (and thus matches and general application).
  //
  // For each "thing" processed, we create an instance of a subclass of
  // AstNode and return it.  This has an interpret method that reads the
  // AST to generate the concrete parsed "thing," whatever it should be.
  //----------------------------------------------------------------------
  
  /**
   * Parse all the atoms that can be found in the input.
   */
  def AtomSeq = rule {
    zeroOrMore(Atom) ~ WS ~ EOI
  }.label("a sequence of atoms")
  
  /**
   * Parse an atom.
   */
  def Atom: Rule1[AstNode] = rule {
    FirstAtom ~ WS ~ "-> " ~ Atom ~~> (MapPairNode(_,_)) |
    FirstAtom ~ zeroOrMore(". " ~ FirstAtom) ~~> (
        (firstarg: AstNode, funlist: List[AstNode]) =>
          funlist.foldLeft(firstarg)(ApplicationNode(context,_,_)))
  }.label("an atom")
  
  /**
   * Parse an atom, with the exception of the general operator application.
   */
  def FirstAtom: Rule1[AstNode] = rule {
    WS ~ (
      // Handle parenthetical expressions.
      "( " ~ Atom ~ ") " |
        
      // Parse a lambda.
      ParsedLambda |
      
      // Parse a typical operator application.
      ParsedApply |
      
      // Parse a typed list.
      ParsedTypedList |
      
      // Parse a property set.
      ParsedAlgProp |
      
      // Parse a variable.  The leading dollar sign is used to distinguish
      // between a symbol and a variable.  If a type is not specified for a
      // variable, it gets put in the type universe.
      ParsedVariable |

      // A "naked" operator is specified by explicitly giving the operator
      // type OPREF.  Otherwise it is parsed as a symbol.
      ESymbol ~ ": " ~ "OPREF " ~~> (
          (sym: NakedSymbolNode) =>
            OperatorNode(sym.str, context.operatorLibrary)) |
            
      // A "naked" ruleset reference is specified by explicitly giving the
      // ruleset reference type RSREF.  Otherwise we continue and parse as
      // a symbol.
      ESymbol ~ ": " ~ "RSREF " ~~> (
          (sym: NakedSymbolNode) =>
            RulesetNode(sym.str, context.ruleLibrary)) |
       
      // Parse a literal.  A literal can take many forms, but it should be
      // possible to always detect the kind of literal during parse.  By
      // default literals go into a simple type, but this can be overridden.
      ParsedLiteral |

      // Parse a number.  This can be any number.
      AnyNumber ~ ": " ~ FirstAtom ~~> (
          (num:NumberNode, typ:AstNode) => num.retype(typ)) |
      AnyNumber |
      
      // Parse a special form.
      ParsedSpecialForm |
      
      // Invoke an external parser to do something.
      ExternalParse.suppressNode |

      // Parse the special type universe.
      "^TYPE " ~> (x => TypeUniverseNode()))
  }.label("a simple atom")
  
  //----------------------------------------------------------------------
  // Invoke an external parser.
  //----------------------------------------------------------------------
  
  /**
   * Parse an external parser reference.
   */
  def ExternalParse = rule {
    "[" ~ oneOrMore(ESymbol, ",") ~ "[" ~
    zeroOrMore(&(!"]]") ~ PANY) ~
    "]]" ~~> (AtomSeqNode(AlgPropNode(List()), _))
  }
  
  //----------------------------------------------------------------------
  // Parse the generalized "special form."
  //----------------------------------------------------------------------
  
  /** Parse a special form node. */
  def ParsedSpecialForm = rule {
    AlternativeOperatorDefinition | ParsedGeneralForm | ParsedSpecialBindForm
  }.label("a special form")
  
  /** Parse the general "two atom" form of the special form. */
  def ParsedGeneralForm = rule {
    "{: " ~ Atom ~ Atom ~ ":} " ~~> (SpecialFormNode(_,_))
  }
  
  /** Parse the specialized short form of a special form. */
  def ParsedSpecialBindForm = rule {
    "{ " ~ ESymbol ~ (
        zeroOrMore(Atom) ~~> (
            first =>
              if (first.length == 0) None
              else Some(NakedSymbolNode("") ->
              	AtomSeqNode(AlgPropNode(),first))) ~
        zeroOrMore(BindBlock | ListBlock)
    ) ~~> ((x,y) => x match {
      case None => BindingsNode(y)
      case Some(map) => BindingsNode(map::y)
    }) ~ "} " ~~> (SpecialFormNode(_,_))
  }
  
  /** Parse a list block from a special form. */
  def ListBlock = rule {
    "#" ~ ESymbol ~ zeroOrMore(Atom, ", ") ~~>
    	(_ -> AtomSeqNode(AlgPropNode(), _))
  }.label("a # list block")
  
  /** Parse a simple bind from a special form. */
  def BindBlock = rule {
    "#" ~ ESymbol ~ "= " ~ Atom ~~> (_ -> _)
  }.label("a # binding")
  
  //----------------------------------------------------------------------
  // Parse a the syntactic sugar version of an operator prototype.
  //----------------------------------------------------------------------
  
  /** Parse an operator definition. */
  def AlternativeOperatorDefinition = rule {
    "{! " ~ OperatorPrototypeNode ~
    optional("is " ~ (OperatorPropertiesNode | ParsedAlgProp)) ~
    zeroOrMore(BindBlock | ListBlock) ~
    "} " ~~> { (proto, props, blocks) =>
      val newparams = props match {
        case None => AtomSeqNode(AlgPropNode(), proto._2)
        case Some(ap) => AtomSeqNode(ap, proto._2)
      }
      val binds = BindingsNode(List(
          NakedSymbolNode("name")->proto._1,
          NakedSymbolNode("params")->newparams,
          NakedSymbolNode("type")->proto._3) ++ blocks)
      SpecialFormNode(NakedSymbolNode("operator"), binds)
    }
  }
  
  /** Parse an operator prototype. */
  def OperatorPrototypeNode = rule {
    ESymbol ~ "( " ~ zeroOrMore(Atom, ", ") ~ ") " ~
    	optional(": " ~ FirstAtom) ~~> {
    	  (name, params, typ) => typ match {
    	    case None => (name, params, AnyNode)
    	    case Some(typeNode) => (name, params, typeNode.asInstanceOf[AstNode])
    	  }
    	}
  }.label("an operator prototype")
  
  /** Parse an operator properties block. */
  def OperatorPropertiesNode = rule {
    oneOrMore(
        ignoreCase("absorber") ~ WS ~ Atom ~~> ((x) => AbsorberNode(x)) |
        ignoreCase("identity") ~ WS ~ Atom ~~> ((x) => IdentityNode(x)) |
        ignoreCase("not") ~ WS ~ (
	        ignoreCase("associative") ~>
	        	((x) => AssociativeNode(FalseNode)) |
	        ignoreCase("commutative") ~>
	        	((x) => CommutativeNode(FalseNode)) |
	        ignoreCase("idempotent") ~>
	        	((x) => IdempotentNode(FalseNode))
        ) |
        ignoreCase("associative") ~>
        	((x) => AssociativeNode(TrueNode)) |
        ignoreCase("commutative") ~>
        	((x) => CommutativeNode(TrueNode)) |
        ignoreCase("idempotent") ~>
        	((x) => IdempotentNode(TrueNode)), WS ~ ", ") ~ WS ~~>
    (AlgPropNode(_))
  }.label("operator properties")
  
  //----------------------------------------------------------------------
  // Parse a simple operator application.  The other form of application
  // (the more general kind) is parsed in Atom.
  //----------------------------------------------------------------------

  /** Parse the "usual" operator application form. */
  def ParsedApply = rule {
    // Parse an operator application.  This just applies an operator to
    // some other atom.  The operator name is given as a symbol, and the
    // argument may be enclosed in parens if it is a list, or joined to
    // the operator with a dot if not.  The dot form is parsed elsewhere.
    //
    // If you want to use a general atom, use a dot to join it to the argument.
    // The same comment applies if you want to use a general atom as the
    // argument.
    ESymbol ~ "( " ~ ParsedAtomSeq ~ ") " ~~> (
      (op: NakedSymbolNode, arg: AtomSeqNode) =>
        ApplicationNode(context, op, arg))
  }.label("an operator application")
  
  //----------------------------------------------------------------------
  // Parse a lambda.
  //----------------------------------------------------------------------

  /** Parse a lambda expression. */
  def ParsedLambda = rule {
    "\\ " ~ ParsedVariable ~ ". " ~ FirstAtom ~~> (
        (lvar: VariableNode, body: AstNode) => LambdaNode(lvar, body))
  }.label("a lambda expression")
  
  //----------------------------------------------------------------------
  // Parse trivial literals.
  //----------------------------------------------------------------------

  /**
   * Parse a literal symbol or a literal string.
   */
  def ParsedLiteral = rule {
      ESymbol ~ ": " ~ FirstAtom ~~>
      	((sym: NakedSymbolNode, typ: AstNode) =>
      	  SymbolLiteralNode(Some(typ), sym.str)) |
      ESymbol ~~>
      	((sym: NakedSymbolNode) =>
      	  SymbolLiteralNode(None, sym.str)) |
      EVerb ~ ": " ~ FirstAtom ~~>
      	((str: String, typ: AstNode) => StringLiteralNode(typ, str)) |
      EVerb ~~>
      	((str: String) => StringLiteralNode(SimpleTypeNode(STRING), str)) |
      EString ~ ": " ~ FirstAtom ~~>
      	((str: String, typ: AstNode) => StringLiteralNode(typ, str)) |
      EString ~~>
      	((str: String) => StringLiteralNode(SimpleTypeNode(STRING), str))
  }.label("a literal expression")
  
  /** Parse a verbatim block. */
  def EVerb = rule {
    "\"\"\"".suppressNode ~
    zeroOrMore(&(!"\"\"\"") ~ PANY) ~> (x => x) ~
    "\"\"\" ".suppressNode
  }.label("a verbatim block")

  /** Parse a double-quoted string. */
  def EString = rule {
    "\"" ~ zeroOrMore(Character) ~~> (x => construct(x)) ~ "\" "
  }.label("a string")

  /**
   * Parse a character in a string.  The character is added to the end of the
   * string passed in (if any) and the composite string is returned.  Escapes
   * are interpreted here.
   */
  def Character = rule {
    (EscapedCharacter | NormalCharacter) ~> (x => x)
  }.label("a single character")

  /** Parse an escaped character. */
  def EscapedCharacter = rule {
    "\\" ~ anyOf("""`"nrt\""")
  }.label("a character escape sequence")

  /** Parse a normal character. */
  def NormalCharacter = rule { noneOf(""""\""") }.label("a character")

  /** Parse a symbol. */
  def ESymbol = rule {
    val str = new StringBuilder()
    "`" ~ zeroOrMore(SymChar) ~~> (x => NakedSymbolNode(construct(x))) ~ "` " |
    group(("a" - "z" | "A" - "Z" | "_") ~ zeroOrMore(
      "a" - "z" | "A" - "Z" | "0" - "9" | "_")) ~> (NakedSymbolNode(_)) ~ WS
  }.label("a symbol")

  /** Parse a character that is part of a symbol. */
  def SymChar = rule {
    (EscapedCharacter | SymNorm) ~> (x => x)
  }.label("a single character")
  
  /** Parse a "normal" non-escaped character that is part of a symbol. */
  def SymNorm = rule { noneOf("""`\""") }.label("a character")

  //----------------------------------------------------------------------
  // Parse property lists.
  //----------------------------------------------------------------------
  
  /** Parse an algebraic properties specification. */
  def ParsedAlgProp = rule {
    "% " ~ OperatorPropertiesNode |
    "%" ~ zeroOrMore(
        ignoreCase("B") ~ "[ " ~ Atom ~ "]" ~~> ((x) => AbsorberNode(x)) |
        ignoreCase("D") ~ "[ " ~ Atom ~ "]" ~~> ((x) => IdentityNode(x)) |
        ignoreCase("A") ~ optional("[ " ~ Atom ~ "]") ~~>
        	((x) => AssociativeNode(x.getOrElse(TrueNode))) |
        ignoreCase("C") ~ optional("[ " ~ Atom ~ "]") ~~>
        	((x) => CommutativeNode(x.getOrElse(TrueNode))) |
        ignoreCase("I") ~ optional("[ " ~ Atom ~ "]") ~~>
        	((x) => IdempotentNode(x.getOrElse(TrueNode))) |
        ignoreCase("!A") ~> ((x) => AssociativeNode(FalseNode)) |
        ignoreCase("!C") ~> ((x) => CommutativeNode(FalseNode)) |
        ignoreCase("!I") ~> ((x) => IdempotentNode(FalseNode))) ~ WS ~~>
    (AlgPropNode(_))
  }.label("an algebraic properties specification.")

  //----------------------------------------------------------------------
  // Parse lists of atoms.
  //----------------------------------------------------------------------

  /**
   * Parse a "typed" list.  That is, a list whose properties are specified.
   */
  def ParsedTypedList = rule {
    ParsedAlgProp ~ WS ~ "( " ~ ParsedAtomSeq ~ ") " ~~>
    ((props: AlgPropNode, list: AtomSeqNode) => AtomSeqNode(props, list.list))
  }.label("a typed list of atoms")
  
  /**
   * Parse a list of atoms, separated by commas.  No concept of associativity,
   * commutativity, etc., is inferred at this point.
   */
  def ParsedAtomSeq = rule {
    zeroOrMore(Atom, ", ") ~~> (AtomSeqNode(AlgPropNode(), _))
  }.label("a comma-separated list of atoms")

  //----------------------------------------------------------------------
  // Parse a map pair.
  //----------------------------------------------------------------------
  
  /**
   * Parse a map pair.
   */
  def ParsedMapPair = rule {
    FirstAtom ~ "-> " ~ Atom ~~> (MapPairNode(_,_))
  }

  //----------------------------------------------------------------------
  // Parse variables.
  //----------------------------------------------------------------------
  
  /**
   * Parse a variable.
   */
  def ParsedVariable = rule {
    ParsedTermVariable | ParsedMetaVariable
  }.label("a variable")

  /** Parse a term variable. */
  def ParsedTermVariable = rule {
    "$" ~ (ParsedTypedVariable | ParsedUntypedVariable)
  }.label("a term variable")
  
  /** Parse a meta variable. */
  def ParsedMetaVariable = rule {
    "$$" ~ (ParsedTypedVariable | ParsedUntypedVariable) ~~> (
        vx => MetaVariableNode(vx))
  }.label("a metavariable")
  
  /** Parse a typed variable or metavariable. */
  def ParsedTypedVariable = rule {
    ESymbol ~ optional("{ " ~ Atom ~ "} ") ~ ": " ~ FirstAtom ~
    zeroOrMore("@" ~ ESymbol) ~~> (
      (sval: NakedSymbolNode, grd: Option[AstNode], typ: AstNode,
          list: List[NakedSymbolNode]) =>
        VariableNode(typ, sval.str, grd, list.map(_.str).toSet))
  }.label("a variable name and type")
  
  /** Parse an untyped variable or metavariable. */
  def ParsedUntypedVariable = rule {
    ESymbol ~ optional("{ " ~ Atom ~ "} ") ~
    zeroOrMore("@" ~ ESymbol) ~~> (
      (sval, grd, list) =>
        VariableNode(SimpleTypeNode(EANY), sval.str, grd,
            list.map(_.str).toSet))
  }.label("a variable name")

  //----------------------------------------------------------------------
  // Parse whitespace.
  //----------------------------------------------------------------------

  /** Parse ignorable whitespace. */
  def WS: Rule0 = rule { SuppressNode
    zeroOrMore(
        // Whitesapce.
        oneOrMore(anyOf(" \n\r\t\f")) |
        "/*" ~ zeroOrMore(&(!"*/") ~ PANY) ~ "*/" |
        "//" ~ zeroOrMore(&(!anyOf("\r\n")) ~ PANY) ~ ("\r\n" | "\r" | "\n" | EOI)
        )
  }.label("whitespace or comments")

  //----------------------------------------------------------------------
  // Parse a number.
  //----------------------------------------------------------------------

  /**
   * Parse a number.  The number can be an integer or a float, and can be
   * positive or negative.
   */
  def AnyNumber: Rule1[NumberNode] = rule {
    optional("-" ~ push(false)) ~ (
      HNumber |
      BNumber |
      DNumber |
      ONumber) ~ WS ~~> (NumberNode(_:Option[Boolean],_:UnsignedIntegerNode,
          _:Option[UnsignedIntegerNode],_:Option[SignedIntegerNode]))
  }.label("an integer or floating point number")

  /**
   * Parse a hexadecimal number that may be either an integer or a float.
   */
  def HNumber = rule {
    HInteger ~
      optional("." ~ zeroOrMore(HDigit) ~> (UnsignedIntegerNode(_, 16))) ~
      optional(ignoreCase("p") ~ Exponent)
  }.label("a hexadecimal number")

  /**
   * Parse a binary number that may be either an integer or a float.
   */
  def BNumber = rule {
    BInteger ~
      optional("." ~ zeroOrMore(BDigit) ~> (UnsignedIntegerNode(_, 2))) ~
      optional((ignoreCase("e") | ignoreCase("p")) ~ Exponent)
  }.label("a binary number")

  /**
   * Parse a decimal number that may be either an integer or a float.
   */
  def DNumber = rule {
    DInteger ~
      optional("." ~ zeroOrMore(DDigit) ~> (UnsignedIntegerNode(_, 10))) ~
      optional((ignoreCase("e") | ignoreCase("p")) ~ Exponent)
  }.label("a decimal number")

  /**
   * Parse an octal number that may be either an integer or a float.
   */
  def ONumber = rule {
    OInteger ~
      optional("." ~ zeroOrMore(ODigit) ~> (UnsignedIntegerNode(_, 8))) ~
      optional((ignoreCase("e") | ignoreCase("p")) ~ Exponent)
  }.label("an octal number")

  /**
   * Parse an exponent expression.  The expression does not include the
   * linking "e" or "p" exponent indicator.
   */
  def Exponent = rule {
      optional("+" ~ push(true) | "-" ~ push(false)) ~
      AnyInteger ~~> (SignedIntegerNode(_, _))
  }.label("an exponent")

  /**
   * Parse an integer in hexadecimal, decimal, octal, or binary.
   * @return	An unsigned integer.
   */
  def AnyInteger = rule { (
    HInteger |
    BInteger |
    DInteger |
    OInteger ) ~ WS
  }.label("an integer")

  /**
   * Parse a hexadecimal integer.
   * @return	An unsigned integer.
   */
  def HInteger = rule {
    ignoreCase("0x") ~ oneOrMore(HDigit) ~> (UnsignedIntegerNode(_: String, 16))
  }.label("a hexadecimal integer")

  /**
   * Parse a binary integer.
   * @return	An unsigned integer.
   */
  def BInteger = rule {
    ignoreCase("0b") ~ oneOrMore(BDigit) ~> (UnsignedIntegerNode(_: String, 2))
  }.label("a binary integer")

  /**
   * Parse a decimal integer.
   * @return	An unsigned integer.
   */
  def DInteger = rule {
    group(("1" - "9") ~ zeroOrMore(DDigit)) ~>
    (UnsignedIntegerNode(_: String, 10))
  }.label("a decimal integer")

  /**
   * Parse an octal integer.
   * @return	An unsigned integer.
   */
  def OInteger = rule {
    group("0" ~ zeroOrMore(ODigit)) ~> (UnsignedIntegerNode(_: String, 8))
  }.label("an octal integer")

  /** Parse a decimal digit. */
  def DDigit = rule { "0" - "9" }.label("a decimal digit")
  
  /** Parse an octal digit. */
  def ODigit = rule { "0" - "7" }.label("an octal digit")
  
  /** Parse a hexadecimal digit. */
  def HDigit = rule {
    "0" - "9" | "a" - "f" | "A" - "F"
  }.label("a hexadecimal digit")
  
  /** Parse a binary digit. */
  def BDigit = rule { "0" | "1" }.label("a binary digit")

  //----------------------------------------------------------------------
  // Other methods affecting the parse.
  //----------------------------------------------------------------------
  
  /**
   * Eliminate trailing whitespace.  This trick is found on the Parboiled web
   * site in the examples.
   * @param string	Parsed text.
   */
  override implicit def toRule(string: String) =
    if (string.endsWith(" ")) str(string.trim) ~ WS else str(string)
}

import scala.util.parsing.combinator.JavaTokenParsers
import scala.util.parsing.combinator.PackratParsers
class ParseCombinators(val context: Context) extends JavaTokenParsers with PackratParsers {
  import scala.util.matching.Regex
  import ornl.elision.parse.AtomParser._
  import scala.util.parsing.input.CharSequenceReader
  import ornl.elision.core.{ ANY => EANY }

  // TODO: This is a quickfix to implement c-style comments. It works well, but we cannot
  // nest comments. From:
  // http://stackoverflow.com/questions/5952720/ignoring-c-style-comments-in-a-scala-combinator-parser
  protected override val whiteSpace = """(\s|//.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  
  //======================================================================
  // Parse application/lambda.
  //======================================================================
  lazy val AtomSeq: PackratParser[List[AstNode]] = (
    rep(AtomApplication))

  lazy val Atom: PackratParser[AstNode] = (
    "(" ~> AtomApplication <~ ")" |

    Lambda |

    OperatorApplication |

    TypedList |

    AlgProp |

    Variable |

    // A "naked" operator is specified by explicitly giving the operator
    // type OPREF.  Otherwise it is parsed as a symbol.
    Esym ~ ":" ~ "OPREF" ^^ {
      case sym ~ _ ~ _ => OperatorNode(sym, context.operatorLibrary)
    } |

    // A "naked" ruleset reference is specified by explicitly giving the
    // ruleset reference type RSREF.  Otherwise we continue and parse as
    // a symbol. 
    Esym ~ ":" ~ "RSREF" ^^ {
      case sym ~ _ ~ _ => RulesetNode(sym, context.ruleLibrary)
    } |

    // Parse a literal.  A literal can take many forms, but it should be
    // possible to always detect the kind of literal during parse.  By
    // default literals go into a simple type, but this can be overridden.
    Literal |

    // Parse a typed number.  This can be any number.
    AnyNumber ~ ":" ~ Atom ^^ {
      case num ~ _ ~ typ => num.retype(typ)
    } |

    // Parse an untyped number.  This can be any number.
    AnyNumber |

    // Parse a special form.
    ParsedSpecialForm |

    // Invoke an external parser to do something.
//    ExternalParse |

    "^TYPE" ^^ {
      case _ => TypeUniverseNode()
    })

  lazy val OperatorApplication: PackratParser[ApplicationNode] =
    Esym ~ "(" ~ repsep(AtomApplication, ",") ~ ")" ^^ {
      case op ~ "(" ~ arg ~ ")" => ApplicationNode(
        context, NakedSymbolNode(op), AtomSeqNode(AlgPropNode(), arg))
    }

  lazy val AtomApplication: PackratParser[AstNode] =
    Atom ~ "->" ~ Atom ^^ { case x ~ "->" ~ y => MapPairNode(x, y) } |
      Atom ~ rep("." ~> Atom) ^^ {
        case x ~ list => list.foldLeft(x)(
          ApplicationNode(context, _, _))
      }

  // TODO: fix lambdas
  lazy val Lambda: PackratParser[LambdaNode] =
    "\\" ~ Variable ~ "." ~ Atom ^^ {
      case "\\" ~ v ~ "." ~ body => LambdaNode(v, body)
    }

  //======================================================================
  // END application/lambda.
  //======================================================================    

  //======================================================================
  // Parse special form / external parser
  //======================================================================
  // TODO: Make ExternalParse work, should really return AtomSeqNode
  lazy val ExternalParse: PackratParser[NakedSymbolNode] =
    "[" ~ rep1sep(Esym, ",") ~ "[" ~ ("]]" ~> Atom).? ~ "]]" ^^^ {
      NakedSymbolNode("NOT IMPLEMENTED")
    }

  lazy val ParsedSpecialForm: PackratParser[SpecialFormNode] =
    AlternativeOperatorDefinition | GeneralForm | SpecialBindForm

  // TODO: this should differ a little from the other parser, check it for correctness
  lazy val AlternativeOperatorDefinition: PackratParser[SpecialFormNode] =
    "{!" ~ OperatorPrototypeNode ~ (ignoreCase("is") ~> (PropListLong | AlgProp)).? ~
      (BindBlock | ListBlock).* ~ "}" ^^ {
        case _ ~ proto ~ props ~ blocks ~ _ =>
          val newparams = props match {
            case None => AtomSeqNode(AlgPropNode(), proto._2)
            case Some(ap) => AtomSeqNode(ap, proto._2)
          }
          val binds = BindingsNode(List(
            NakedSymbolNode("name") -> proto._1,
            NakedSymbolNode("params") -> newparams,
            NakedSymbolNode("type") -> proto._3) ++ blocks)
          SpecialFormNode(NakedSymbolNode("operator"), binds)
      }

  lazy val GeneralForm: PackratParser[SpecialFormNode] =
    "{:" ~ AtomApplication ~ AtomApplication ~ ":}" ^^ {
      case _ ~ x ~ y ~ _ => SpecialFormNode(x, y)
    }

  // TODO: Please, make it stop. What have we done.
  lazy val SpecialBindForm: PackratParser[SpecialFormNode] =
    "{" ~ Esym ~ (
      ((AtomApplication).* ^^ {
        case list =>
          if (list.length == 0) None
          else Some(NakedSymbolNode("") -> AtomSeqNode(AlgPropNode(), list))
      }) ~
      (BindBlock | ListBlock).* ^^ {
        case x ~ y => x match {
          case None => BindingsNode(y)
          case Some(map) => BindingsNode(map :: y)
        }
      }) ~ "}" ^^ {
        case _ ~ sym ~ monster ~ _ => SpecialFormNode(NakedSymbolNode(sym), monster)
      }

  // TODO: This Tuple3 is gross, change it
  lazy val OperatorPrototypeNode: PackratParser[Tuple3[NakedSymbolNode, List[AstNode], AstNode]] =
    Esym ~ "(" ~ repsep(Atom, ",") ~ ")" ~ (":" ~> Atom).? ^^ {
      case name ~ _ ~ params ~ _ ~ typ => typ match {
        case None => (NakedSymbolNode(name), params, AnyNode)
        case Some(typeNode) => (NakedSymbolNode(name), params, typeNode.asInstanceOf[AstNode])
      }
    }

  lazy val ListBlock: PackratParser[(NakedSymbolNode, AtomSeqNode)] =
    "#" ~ Esym ~ repsep(AtomApplication, ",") ^^ {
      case _ ~ sym ~ list => (NakedSymbolNode(sym), AtomSeqNode(AlgPropNode(), list))
    }

  lazy val BindBlock: PackratParser[(NakedSymbolNode, AstNode)] =
    "#" ~ Esym ~ "=" ~ AtomApplication ^^ {
      case _ ~ sym ~ _ ~ appl => (NakedSymbolNode(sym) -> appl)
    }
  //======================================================================
  // END special form / external parser
  //======================================================================  

  //======================================================================
  // Parse property/typed list
  //======================================================================    
  lazy val TypedList: PackratParser[AtomSeqNode] = (
    AlgProp ~ "(" ~ repsep(AtomApplication, ",") ~ ")" ^^ {
      case props ~ _ ~ list ~ _ => AtomSeqNode(props, list)
    })

  lazy val AlgProp: PackratParser[AlgPropNode] =
    "%" ~> (PropListLong | PropListShort)

  lazy val PropListShort: PackratParser[AlgPropNode] =
    (
      ignoreCase("B") ~ "[" ~ AtomApplication ~ "]" ^^ {
        case _ ~ _ ~ x ~ _ => AbsorberNode(x)
      } |
      ignoreCase("D") ~ "[" ~ AtomApplication ~ "]" ^^ {
        case _ ~ _ ~ x ~ _ => IdentityNode(x)
      } |
      ignoreCase("A") ~ ("[" ~> AtomApplication <~ "]").? ^^ {
        case _ ~ x => AssociativeNode(x.getOrElse(TrueNode))
      } |
      ignoreCase("C") ~ ("[" ~> AtomApplication <~ "]").? ^^ {
        case _ ~ x => CommutativeNode(x.getOrElse(TrueNode))
      } |
      ignoreCase("I") ~ ("[" ~> AtomApplication <~ "]").? ^^ {
        case _ ~ x => IdempotentNode(x.getOrElse(TrueNode))
      } |
      ignoreCase("!A") ^^ { case _ => AssociativeNode(FalseNode) } |
      ignoreCase("!C") ^^ { case _ => CommutativeNode(FalseNode) } |
      ignoreCase("!I") ^^ { case _ => IdempotentNode(FalseNode) }).* ^^ {
        AlgPropNode(_)
      }

  // TODO: Check and make sure this supports guards like the more succinct version
  lazy val PropListLong: PackratParser[AlgPropNode] =
    rep1sep(
      ignoreCase("absorber") ~> AtomApplication ^^ { AbsorberNode(_) } |
        ignoreCase("identity") ~> AtomApplication ^^ { IdentityNode(_) } |
        ignoreCase("not") ~> (
          ignoreCase("associative") ^^^ { AssociativeNode(FalseNode) } |
          ignoreCase("commutative") ^^^ { CommutativeNode(FalseNode) } |
          ignoreCase("idempotent") ^^^ { IdempotentNode(FalseNode) }) |
          ignoreCase("associative") ^^^ { AssociativeNode(TrueNode) } |
          ignoreCase("commutative") ^^^ { CommutativeNode(TrueNode) } |
          ignoreCase("idempotent") ^^^ { IdempotentNode(TrueNode) }, ",") ^^ {
        AlgPropNode(_)
      }
  //======================================================================
  // END property/typed list
  //======================================================================

  //======================================================================
  // Parse a variable.
  //======================================================================
  lazy val guard: PackratParser[Option[AstNode]] =
    opt("{" ~> AtomApplication <~ "}") ^^ {
      case Some(appl) => Some(appl)
      case _ => None
    }

  lazy val label: PackratParser[String] = "@" ~> Esym

  lazy val VariableTyped: PackratParser[VariableNode] =
    Esym ~ guard ~ ":" ~ Atom ~ rep(label) ^^ {
      case sym ~ grd ~ _ ~ typ ~ list => VariableNode(
        typ, sym, grd, list.toSet)
    }

  lazy val VariableUntyped: PackratParser[VariableNode] =
    Esym ~ guard ~ rep(label) ^^ {
      case sym ~ grd ~ list => VariableNode(
        SimpleTypeNode(EANY), sym, grd, list.toSet)
    }

  lazy val VariableTerm: PackratParser[VariableNode] =
    "$" ~> (VariableTyped | VariableUntyped)

  lazy val VariableMeta: PackratParser[MetaVariableNode] =
    "$$" ~> (VariableTyped | VariableUntyped) ^^ {
      MetaVariableNode(_)
    }

  lazy val Variable: PackratParser[VariableNode] =
    VariableTerm | VariableMeta

  //======================================================================
  // END a variable.
  //======================================================================

  //======================================================================
  // Parse a literal.
  //======================================================================

  // this also does unicode
  // TODO: Fix hard tabs, for some reason they don't work in either parser
  lazy val strSym = """(\\[\\bfnrt`"]|[^\p{Cntrl}\\`]|\\u[a-fA-F0-9]{4})*"""
  lazy val strNrm = """(\\[\\bfnrt`"]|[^\p{Cntrl}\\"]|\\u[a-fA-F0-9]{4})*"""

  lazy val EscapedCharacter: PackratParser[String] =
    """\\[`"nrt\\]""".r ^^ { _.toString }
    
  lazy val NormalCharacter: PackratParser[String] =
    """[^"\\]""".r ^^ { _.toString }
  
  lazy val SymNorm: PackratParser[String] =
    """[^`\\]""".r ^^ { _.toString }
    
  lazy val SymCharacter: PackratParser[String] = 
    EscapedCharacter | SymNorm
  
  lazy val Character: PackratParser[String] = 
    EscapedCharacter | NormalCharacter
    
  lazy val Estr: PackratParser[String] =
    "\"" ~> rep(Character) <~ "\"" ^^ { construct(_) }
  
  lazy val Esym: PackratParser[String] =
    "`" ~> rep(SymCharacter) <~ "`" ^^ { construct(_) } |
    """[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ { _.toString }
    
//  // TODO: Find a better way to use construct here. Probably just add a new method
//  lazy val Estr: PackratParser[String] =
//    ("\"" + strNrm + "\"").r ^^ { case str =>
//      val stringList = str.toString.drop(1).dropRight(1).toList.map(_.toString) 
//      construct(stringList) }
//
//  lazy val Esym: PackratParser[String] =
//    ("`" + strSym + "`").r ^^ { case str =>
//      val stringList = str.toString.drop(1).dropRight(1).toList.map(_.toString) 
//      construct(stringList) } |
//      """[a-zA-Z_][a-zA-Z0-9_]*""".r ^^ { _.toString }

  lazy val Everb: PackratParser[String] = {
    "(?s)\"\"\".*?\"\"\"".r ^^ { case str =>
      construct(str.toString.drop(3).dropRight(3).toList.map(_.toString)) }
  }

  // TODO: construct doesn't correctly handle things like `\t\n`
  lazy val Literal: PackratParser[AstNode] = { 		
    Esym ~ ":" ~ Atom ^^ {
      case sym ~ _ ~ typ => SymbolLiteralNode(Some(typ), sym)
    } |
    Esym ^^ { SymbolLiteralNode(None, _) } |
    Everb ~ ":" ~ Atom ^^ {
      case str ~ _ ~ typ => StringLiteralNode(typ, str)
    } |
    Everb ^^ { StringLiteralNode(SimpleTypeNode(STRING), _) } |
	  Estr ~ ":" ~ Atom ^^ {
	    case str ~ _ ~ typ => StringLiteralNode(typ, str)
	  } |
    Estr ^^ { StringLiteralNode(SimpleTypeNode(STRING), _) }
  }
  //======================================================================
  // END a literal.
  //======================================================================  

  //======================================================================
  // Parse a number.
  //======================================================================

  // Decimals ( with exponents )
  lazy val optSign: PackratParser[Option[Boolean]] = (
    opt("""[+-]""".r) ^^ {
      case Some("+") => Some(true)
      case Some("-") => Some(false)
      case _ => None
    })

  lazy val optFrac: PackratParser[Option[UnsignedIntegerNode]] =
    opt("." ~> AnyInteger) ^^ {
      case Some(int) => Some(int)
      case _ => None
    }

  lazy val optExp: PackratParser[Option[SignedIntegerNode]] = (
    opt("""[eEpP]""".r ~> Exponent) ^^ {
      case Some(exp) => Some(exp)
      case _ => None
    })

  lazy val optExpHex: PackratParser[Option[SignedIntegerNode]] = (
    opt("""[pP]""".r ~> Exponent) ^^ {
      case Some(exp) => Some(exp)
      case _ => None
    })

  lazy val AnyNumber: PackratParser[NumberNode] = (
    // hex
    optSign ~ ignoreCase("0x") ~ HInteger ~ optFrac ~ optExpHex ^^ {
      case sign ~ _ ~ int ~ frac ~ exp =>
        NumberNode(sign: Option[Boolean], int: UnsignedIntegerNode,
          frac: Option[UnsignedIntegerNode], exp: Option[SignedIntegerNode])
    } |
    // binary
    optSign ~ ignoreCase("0b") ~ BInteger ~ optFrac ~ optExp ^^ {
      case sign ~ _ ~ int ~ frac ~ exp =>
        NumberNode(sign: Option[Boolean], int: UnsignedIntegerNode,
          frac: Option[UnsignedIntegerNode], exp: Option[SignedIntegerNode])
    } |
    // octal
    optSign ~ ignoreCase("0") ~ OInteger ~ optFrac ~ optExp ^^ {
      case sign ~ _ ~ int ~ frac ~ exp =>
        NumberNode(sign: Option[Boolean], int: UnsignedIntegerNode,
          frac: Option[UnsignedIntegerNode], exp: Option[SignedIntegerNode])
    } |
    // decimal
    optSign ~ DInteger ~ optFrac ~ optExp ^^ {
      case sign ~ int ~ frac ~ exp =>
        NumberNode(sign: Option[Boolean], int: UnsignedIntegerNode,
          frac: Option[UnsignedIntegerNode], exp: Option[SignedIntegerNode])
    })

  // Integers
  lazy val AnyInteger: PackratParser[UnsignedIntegerNode] =
    ignoreCase("0x") ~> HInteger |
      ignoreCase("0b") ~> BInteger |
      ignoreCase("0") ~> OInteger |
      DInteger

  lazy val DInteger: PackratParser[UnsignedIntegerNode] =
    """[0-9]+""".r ^^ (UnsignedIntegerNode(_: String, 10))
  lazy val HInteger: PackratParser[UnsignedIntegerNode] =
    """[a-fA-F0-9]+""".r ^^ (UnsignedIntegerNode(_: String, 16))
  lazy val BInteger: PackratParser[UnsignedIntegerNode] =
    """[01]+""".r ^^ (UnsignedIntegerNode(_: String, 2))
  lazy val OInteger: PackratParser[UnsignedIntegerNode] =
    """[0-7]+""".r ^^ (UnsignedIntegerNode(_: String, 8))

  // Exponent
  lazy val Exponent: PackratParser[SignedIntegerNode] = (
    optSign ~ AnyInteger ^^ {
      case sign ~ int => SignedIntegerNode(sign, int)
    })
// " // " 
  //======================================================================
  // END a number.
  //======================================================================
  def run(arg: String): ParseResult[Any] = {
//    Console println "`"+ arg +"`"
    parseAll(AtomSeq, new PackratReader(new CharSequenceReader(arg)))
  }

  def ignoreCase(str: String): Regex = ("""(?i)\Q""" + str + """\E""").r
}

object Main extends App {
  val _parser = new ParseCombinators(new Context)
  val t0 = System.nanoTime();

  val text =
<execute>
<![CDATA[
def({ operator #name=typeof #cases %($x:$T)->$T
      #description="Extract and show the type of the argument."
      #detail=
"""Given a single argument, extract the type $T of that argument $x
and return the extracted type. """
} ) 

def({ operator #name=getop #params=%($x:OPREF)
      #description="Given an operator reference, return the operator."
      #detail=
"""Given a single argument, extract the type $T of that argument $x
and return the extracted type. """})
]]>
</execute>
    val ret = _parser.run(text.text)
    Console.println(ret)
  //    _parser.run("add(1000,add(1000,1000,add(1000,1000,add(1000,1000,add(1000,1000,add(1000,1000,add(1000,1000,add(1000,1000,add(1000,1000,add(1000,1000,add(1000,1000,add(1000,1000,add(900,45)))))))))))))")

  val t1 = System.nanoTime();

  Console.println(((t1 - t0).toDouble / 1000000000).toString())
}
