/* Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 */
package sjp.elision.core

import org.parboiled.scala._
import org.parboiled.errors.{ ErrorUtils, ParsingException }
import scala.collection.mutable.LinkedList

//======================================================================
// Definitions for an abstract syntax tree for atoms.
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
case class SimpleTypeNode(TYPE: RootType) extends AstNode {
  def interpret = TYPE
}

/**
 * A node representing the unique type universe.
 */
case class TypeUniverseNode() extends AstNode {
  def interpret = TypeUniverse
}

//----------------------------------------------------------------------
// Operator application.
//----------------------------------------------------------------------

/**
 * A node representing the application of an operator to an argument.
 * @param op		The operator.
 * @param arg		The argument.
 */
case class ApplicationNode(op: AstNode, arg: AstNode) extends AstNode {
  def interpret = Apply(op.interpret, arg.interpret)
}

/**
 * Provide alternate ways to create an application node.
 */
object ApplicationNode {
  /**
   * Create an application node for a binary operator.
   * @param op		The binary operator.
   * @param arg1	First argument.
   * @param arg2	Second argument.
   * @return	The new application node.
   */
  def apply(op: AstNode, arg1: AstNode, arg2: AstNode): ApplicationNode =
    ApplicationNode(op, AtomListNode(List(arg1, arg2)))
}

/**
 * A node representing a "naked" operator.
 * @param str	The operator name.
 */
case class OperatorNode(str: String) extends AstNode {
  def interpret = Operator(str)
}

/**
 * A node representing a lambda.
 * @param lvar	The lambda variable.
 * @param body	The lambda body.
 */
case class LambdaNode(lvar: VariableNode, body: AstNode) extends AstNode {
  def interpret = Lambda(lvar.interpret, body.interpret)
}

/**
 * An abstract syntax tree node holding a simple list of atoms.
 * @param list	The actual list of atom nodes.
 */
case class AtomListNode(list: List[AstNode]) extends AstNode {
  /**
   * Properties of this list, if known.  The properties stored are
   * associativity and commutativity.  If not specified, then the properties
   * have not yet been specified, and should be inherited from an operator.
   */
  var props: Option[(Boolean, Boolean)] = None
  def interpret = AtomList(list map (_.interpret))
  /**
   * Set the properties for this list.
   * @param assoc		If true, the list is associative.
   * @param comm		If true, the list is commutative.
   */
  def setProperties(assoc: Boolean, comm: Boolean) = {
    props = Some(assoc, comm)
    this
  }
}

//----------------------------------------------------------------------
// Operator definition.
//----------------------------------------------------------------------

/**
 * A data structure holding operator properties as they are discovered during
 * the parse.
 */
class OperatorPropertiesNode {
  /** An identity, if any. */
  var withIdentity: Option[AstNode] = None
  /** An absorber, if any. */
  var withAbsorber: Option[AstNode] = None
  /** True iff this operator is idempotent. */
  var isIdempotent = false
  /** True iff this operator is commutative. */ 
  var isCommutative = false
  /** True iff this operator is associative. */
  var isAssociative = false
  
  /**
   * Get the optional identity.
   * @return	The optional identity.
   */
  def identity = withIdentity match {
    case Some(node) => Some(node.interpret)
    case None => None
  }
  
  /**
   * Get the optional absorber.
   * @return	The optional absorber.
   */
  def absorber = withAbsorber match {
    case Some(node) => Some(node.interpret)
    case None => None
  }
  
  /**
   * Convert this into an operator properties instance.
   * @return	The operator properties object.
   */
  def interpret = OperatorProperties(isAssociative, isCommutative,
      isIdempotent, absorber, identity)
}

/**
 * Represent an operator prototype.
 * @param name			The operator name.
 * @param typepars	The type parameters.
 * @param pars			The formal parameter.
 * @param typ				The type.
 */
class OperatorPrototypeNode(
    val name: String,
    val typepars: Option[List[VariableNode]],
    val pars: Option[List[VariableNode]],
    val typ: AstNode) {
  def interpret = OperatorPrototype(
      name,
      typepars match {
        case Some(list) => list.map(_.interpret)
        case None => List[Variable]()
      },
      pars match {
        case Some(list) => list.map(_.interpret)
        case None => List[Variable]()
      },
      typ.interpret)
}

sealed abstract class OperatorDefinitionNode extends AstNode {
  def interpret: OperatorDefinition
}

/**
 * Represent an immediate operator definition.
 * @param opn		The prototype node.
 * @param body	The body.
 */
case class ImmediateOperatorDefinitionNode(
    opn: OperatorPrototypeNode,
    body: AstNode) extends OperatorDefinitionNode {
  def interpret = ImmediateOperatorDefinition(opn.interpret, body.interpret)
}

/**
 * Represent a symbolic operator definition.
 * @param opn		The prototype node.
 * @param prop	The properties.
 */
case class SymbolicOperatorDefinitionNode(
    opn: OperatorPrototypeNode,
    prop: OperatorPropertiesNode) extends OperatorDefinitionNode {
  def interpret = SymbolicOperatorDefinition(opn.interpret, prop.interpret)
}

/**
 * Represent a native operator definition.
 * @param opn		The prototype node.
 * @param prop	The properties.
 */
case class NativeOperatorDefinitionNode(
    opn: OperatorPrototypeNode,
    prop: OperatorPropertiesNode) extends OperatorDefinitionNode {
  def interpret = NativeOperatorDefinition(opn.interpret, prop.interpret)
}

//----------------------------------------------------------------------
// Symbol nodes.
//----------------------------------------------------------------------

/**
 * A node representing a "naked" symbol: a symbol whose type is the type
 * universe.
 * @param str	The symbol text.
 */
case class NakedSymbolNode(str: String) extends AstNode {
  def interpret = Literal(TypeUniverse, SymVal(Symbol(str)))
}

/**
 * A node representing a symbol.
 * @param typ		The type.
 * @param name	The symbol text.
 */
case class SymbolNode(typ: AstNode, name: String) extends AstNode {
  def interpret = Literal(typ.interpret, name)
}

//----------------------------------------------------------------------
// Variable nodes.
//----------------------------------------------------------------------

/**
 * A node representing a variable reference.
 * @param typ		The type.
 * @param name	The variable name.
 */
case class VariableNode(typ: AstNode, name: String) extends AstNode {
  def interpret = Variable(typ.interpret, name)
}

//----------------------------------------------------------------------
// Rule nodes.
//----------------------------------------------------------------------

/**
 * A node representing a rewrite rule.
 * @param decls			Optional pattern variable declarations.
 * @param pattern		The pattern to match.
 * @param rewrite		The rewrite to apply when the pattern matches.
 * @param guards		The list of guards.
 * @param rulesets	The optional list of rulesets.
 * @param level			The optional cache level.
 */
case class RuleNode(
    decls: Option[List[VariableNode]],
    pattern: AstNode,
    rewrite: AstNode,
    guards: List[AstNode],
    rulesets: Option[List[NakedSymbolNode]],
    level: Option[UnsignedIntegerNode]) extends AstNode {
  println(this)
  def interpret = {
    // Get the rulesets as a list of strings.
    val rs = rulesets match {
      case None => Set[String]()
      case Some(list) => list.map(_.str).toSet
    }
    // Get the cache level.  The default is zero.
    val cl = level match {
      case None => 0
      case Some(value) => value.asInt.toInt
    }
    // Make the rule.
    RewriteRule(
        pattern.interpret,
        rewrite.interpret,
        guards.map(_.interpret),
        rs,
        cl)
  }
}

//----------------------------------------------------------------------
// Literal nodes - that is, nodes that hold literal values.
//----------------------------------------------------------------------

/**
 * A node representing a symbol literal.
 * @param typ		The type.
 * @param sym		The symbol text.
 */
case class SymbolLiteralNode(typ: AstNode, sym: String) extends AstNode {
  def interpret = Literal(typ.interpret, SymVal(Symbol(sym)))
}

/**
 * A node representing a string literal.
 * @param typ		The type.
 * @param str		The string text.
 */
case class StringLiteralNode(typ: AstNode, str: String) extends AstNode {
  def interpret = Literal(typ.interpret, str)
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
   * @param newtyp		The new type.
   * @return	A new node with the given type.
   */
  def retype(newtyp: AstNode): NumberNode
}

/**
 * An abstract syntax tree node holding a numeric value.
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
 * An abstract syntax tree node holding an unsigned integer.  The integer value
 * should be regarded as positive.
 * @param digits	The digits of the number.
 * @param radix		The radix.
 * @param typ			The type.  If not specified, INTEGER is used.
 */
case class UnsignedIntegerNode(digits: String, radix: Int,
    typ: AstNode = SimpleTypeNode(INTEGER)) extends NumberNode {
  def interpret = Literal(typ.interpret, asInt)
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
  def interpret = Literal(typ.interpret, asInt)
  lazy val asInt = if (sign) asUInt else -asUInt
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
   * @param sign		If true or None, positive.  If false, negative.
   * @param integer	The unsigned integer value.
   */
  def apply(sign: Option[Boolean],
      integer: UnsignedIntegerNode): SignedIntegerNode =
    SignedIntegerNode(sign, integer, SimpleTypeNode(INTEGER))
}

/**
 * An abstract syntax tree node holding a floating point value.
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
    // together.
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
    // radix as before.
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
  
  def interpret = Literal(typ.interpret, norm._1.asInt.toInt,
      norm._2.asInt.toInt, radix)
  
  def retype(newtyp: AstNode) = FloatNode(sign, integer, fraction, radix, exp,
      newtyp)
}

//======================================================================
// Parse and build atoms.
//======================================================================

/**
 * A parser to parse a single atom.
 */
class AtomParser extends Parser {
  abstract sealed class Presult
  case class Success(node: AstNode) extends Presult
  case class Failure(badness: Option[Throwable]) extends Presult
  
  /**
   * Entry point to parse an atom from the given string.
   * @param atom	The string to parse.
   * @return	The parsed atom.
   */
  def parseAtom(atom: String): Presult = {
    try {
    	Success(TracingParseRunner(Atom).
    	    run(atom).
    	    result.
    	    getOrElse(Failure(None)).
    	    asInstanceOf[AstNode])
    } catch {
      case th: Throwable => Failure(Some(th))
    }
    /*
    run.result match {
      case Some(parsedAtom) => parsedAtom.toString
      case None => throw new ParsingException("Invalid atom.\n" +
          ErrorUtils.printParseErrors(run))
    }
     */
  }
  
  /**
   * Attempt to parse an atom from the provided string, and return it.  If
   * something goes wrong, null is returned by this method.
   * @param atom	The text to parse.
   * @return	The parsed atom, or null if something went wrong.
   */
  def tryParse[ATOM >: BasicAtom](atom: String): ATOM =
    parseAtom(atom) match {
	    case Success(node) => node.interpret
	    case Failure(badness) => null
	  }

  //======================================================================
  // Parse and build atoms.
  //======================================================================

  /**
   * Parse an atom.
   */
  def Atom: Rule1[AstNode] = rule {
    // Handle the special case of the general operator application.  These
    // bind to the right, so: f.g.h.7 denotes Apply(f,Apply(g,Apply(h,7))).
    zeroOrMore(FirstAtom ~ WS ~ ".") ~ FirstAtom ~~> (
        (funlist: List[AstNode], lastarg: AstNode) =>
          funlist.foldRight(lastarg)(ApplicationNode(_,_)))
  }

  /**
   * Parse an atom, with the exception of the general operator application.
   */
  def FirstAtom: Rule1[AstNode] = rule {
    WS ~ (
      // Handle parenthetical expressions.
      "(" ~ Atom ~ WS ~ ")" |
      
      // Parse a rule.
      ParsedRule |
      
      // Parse an operator definition.
      ParsedNativeOperatorDefinition |
        
      // Parse a lambda.
      ParsedLambda |

      // Parse a typical operator application.
      ParsedApply |
        
      // Parse the special OPTYPE.
      "OPTYPE" ~> (x => SimpleTypeNode(OPTYPE)) |

      // Parse the special RULETYPE.
      "RULETYPE" ~> (x => SimpleTypeNode(RULETYPE)) |

      // Parse a typed list.
      ParsedTypedList |

      // Parse a variable.  The leading dollar sign is used to distinguish
      // between a symbol and a variable.  If a type is not specified for a
      // variable, it gets put in the type universe.
      ParsedVariable |

      // A "naked" operator is specified by explicitly giving the operator
      // type OPTYPE.  Otherwise it is parsed as a symbol.
      ESymbol ~ WS ~ ":" ~ WS ~ "OPTYPE" ~~> (
          (sym: NakedSymbolNode) => OperatorNode(sym.str)) |

      // Parse a literal.  A literal can take many forms, but it should be
      // possible to always detect the kind of literal during parse.  By
      // default literals go into a simple type, but this can be overridden.
      ParsedLiteral |
      
      AnyNumber ~ WS ~ ":" ~ Atom ~~> (
          (num:NumberNode, typ:AstNode) => num.retype(typ)) |
      AnyNumber |

      // Parse the special type universe.
      "^TYPE" ~> (x => TypeUniverseNode()))
  }

  /**
   * Parse the "usual" operator application form.
   */
  def ParsedApply = rule {
    // Parse an operator application.  This just applies an operator to
    // some other atom.  The operator name is given as a symbol, and the
    // argument may be enclosed in parens if it is a list, or joined to
    // the operator with a dot if not.  The dot form is parsed elsewhere.
    //
    // If you want to use a general atom, use a dot to join it to the argument.
    // The same comment applies if you want to use a general atom as the
    // argument.
    ESymbol ~ "(" ~ ParsedAtomList ~ WS ~ ")" ~~> (
      (op: NakedSymbolNode, arg: AtomListNode) => ApplicationNode(op, arg))
  }
  
  /**
   * Parse a lambda expression.
   */
  def ParsedLambda = rule {
    "\\" ~ ParsedVariable ~ WS ~ "." ~ FirstAtom ~~> (
        (lvar: VariableNode, body: AstNode) => LambdaNode(lvar, body))
  }
  
  /**
   * Parse a literal symbol or a literal string.
   */
  def ParsedLiteral = rule {
      ESymbol ~ WS ~ ":" ~ Atom ~~>
      	((sym: NakedSymbolNode, typ: AstNode) =>
      	  SymbolLiteralNode(typ, sym.str)) |
      ESymbol ~~>
      	((sym: NakedSymbolNode) =>
      	  SymbolLiteralNode(SimpleTypeNode(SYMBOL), sym.str)) |
      EString ~ WS ~ ":" ~ Atom ~~>
      	((str: String, typ: AstNode) => StringLiteralNode(typ, str)) |
      EString ~~>
      	((str: String) => StringLiteralNode(SimpleTypeNode(STRING), str))
  }

  /**
   * Parse a "typed" list.  That is, a list whose properties are specified.
   */
  def ParsedTypedList = rule {
    // Parse an atom list that specifies its properties.  There are
    // multiple different forms of lists.  The associativity or
    // commutativity of the list is specified by writing a percent sign and
    // an A (for associativity) or C (for commutativity), followed by the
    // list in parens.
    //
    // Note that f(x,y,z), if f is associative, could be written as:
    // f.%A(x,y,z)
    "%" ~ (
      ("AC" | "CA") ~ WS ~ "(" ~ ParsedAtomList ~ WS ~ ")" ~~> (
        (list: AtomListNode) => list.setProperties(true, true)) |
      "A" ~ WS ~ "(" ~ ParsedAtomList ~ WS ~ ")" ~~> (
        (list: AtomListNode) => list.setProperties(true, false)) |
      "C" ~ WS ~ "(" ~ ParsedAtomList ~ WS ~ ")" ~~> (
        (list: AtomListNode) => list.setProperties(false, true)) |
      WS ~ "(" ~ ParsedAtomList ~ WS ~ ")" ~~> (
        (list: AtomListNode) => list.setProperties(false, false)))
  }
  
  /**
   * Parse a list of atoms, separated by commas.  No concept of associativity,
   * commutativity, etc., is inferred at this point.
   */
  def ParsedAtomList = rule {
    (Atom ~ zeroOrMore(WS ~ "," ~ Atom)) ~~> (
      (head: AstNode, tail: List[AstNode]) => AtomListNode(head :: tail))
  }

  /**
   * Parse a rule.
   */
  def ParsedRule = rule(
    // First there are optional variable declarations.  In a rule, all pattern
    // variables must be declared.
    optional("@" ~ ParsedVariable ~
        zeroOrMore(WS ~ anyOf(",@") ~ ParsedVariable) ~~> (
        (head: VariableNode, tail: List[VariableNode]) => head :: tail)) ~

    // Next is the rule itself, consisting of a pattern, a rewrite, and zero
    // or more guards.
    WS ~ ignoreCase("rule") ~ WS ~ "{" ~
    Atom ~ WS ~ "->" ~ Atom ~ zeroOrMore(WS ~ "if" ~ Atom) ~

    // Next the rule can be declared to be in zero or more rulesets.
    optional(WS ~ (ignoreCase("rulesets") | ignoreCase("ruleset")) ~
        WS ~ ESymbol ~ zeroOrMore(WS ~ "," ~ WS ~ ESymbol) ~~> (
          (head: NakedSymbolNode, tail: List[NakedSymbolNode]) => head :: tail)) ~

    // Finally the rule's cache level can be declared.  This must be the
    // last item, if present.
    optional(WS ~ "level" ~ WS ~ DInteger) ~ WS ~ "}" ~~> (
        RuleNode(
            _: Option[List[VariableNode]],
            _: AstNode,
            _: AstNode,
            _: List[AstNode],
            _: Option[List[NakedSymbolNode]],
            _: Option[UnsignedIntegerNode])))

  /**
   * Parse a variable.
   */
  def ParsedVariable = rule {
    ParsedTypedVariable | ParsedUntypedVariable
  }
  
  def ParsedTypedVariable = rule {
    "$" ~ ESymbol ~ WS ~ ":" ~ Atom ~~> (
      (sval: NakedSymbolNode, typ: AstNode) => VariableNode(typ, sval.str))
  }
  
  def ParsedUntypedVariable = rule {
    "$" ~ ESymbol ~~> (sval => VariableNode(TypeUniverseNode(), sval.str))
  }

  //======================================================================
  // Parse whitespace.
  //======================================================================

  /** Parse ignorable whitespace. */
  def WS = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

  //======================================================================
  // Parse a string.
  //======================================================================

  /** Parse a double-quoted string. */
  def EString = rule {
    "\"" ~ zeroOrMore(Character) ~> (_.toString) ~ "\""
  }

  /** Parse a character in a string. */
  def Character = rule { EscapedCharacter | NormalCharacter }

  /** Parse an escaped character. */
  def EscapedCharacter = rule { "\\" ~ anyOf("""`"\nrt""") }

  /** Parse a normal character. */
  def NormalCharacter = rule { noneOf(""""\""") }

  //======================================================================
  // Parse a symbol.
  //======================================================================

  /** Parse a symbol. */
  def ESymbol = rule {
    "`" ~ zeroOrMore(SymChar) ~> (NakedSymbolNode(_)) ~ "`" |
    group(("a" - "z" | "A" - "Z" | "_") ~ zeroOrMore(
      "a" - "z" | "A" - "Z" | "0" - "9" | "_")) ~> (NakedSymbolNode(_))
  }

  /** Parse a character that is part of a symbol. */
  def SymChar = rule { EscapedCharacter | SymNorm }
  
  /** Parse a "normal" non-escaped character that is part of a symbol. */
  def SymNorm = rule { noneOf("""`\""") }

  //======================================================================
  // Parse a number.
  //======================================================================

  /**
   * Parse a number.  The number can be an integer or a float, and can be
   * positive or negative.
   */
  def AnyNumber: Rule1[NumberNode] = rule {
    optional("-" ~ push(true)) ~ (
      HNumber |
      BNumber |
      DNumber |
      ONumber) ~~> (NumberNode(_:Option[Boolean],_:UnsignedIntegerNode,
          _:Option[UnsignedIntegerNode],_:Option[SignedIntegerNode]))
  }

  /**
   * Parse a hexadecimal number that may be either an integer or a float.
   */
  def HNumber = rule {
    HInteger ~
      optional("." ~ zeroOrMore(HDigit) ~> (UnsignedIntegerNode(_, 16))) ~
      optional(ignoreCase("p") ~ Exponent)
  }

  /**
   * Parse a binary number that may be either an integer or a float.
   */
  def BNumber = rule {
    BInteger ~
      optional("." ~ zeroOrMore(BDigit) ~> (UnsignedIntegerNode(_, 2))) ~
      optional((ignoreCase("e") | ignoreCase("p")) ~ Exponent)
  }

  /**
   * Parse a decimal number that may be either an integer or a float.
   */
  def DNumber = rule {
    DInteger ~
      optional("." ~ zeroOrMore(DDigit) ~> (UnsignedIntegerNode(_, 10))) ~
      optional((ignoreCase("e") | ignoreCase("p")) ~ Exponent)
  }

  /**
   * Parse an octal number that may be either an integer or a float.
   */
  def ONumber = rule {
    OInteger ~
      optional("." ~ zeroOrMore(ODigit) ~> (UnsignedIntegerNode(_, 8))) ~
      optional((ignoreCase("e") | ignoreCase("p")) ~ Exponent)
  }

  /**
   * Parse an exponent expression.  The expression does not include the
   * linking "e" or "p" exponent indicator.
   */
  def Exponent = rule {
      optional("+" ~ push(true) | "-" ~ push(false)) ~
      AnyInteger ~~> (SignedIntegerNode(_, _))
  }

  /**
   * Parse an integer in hexadecimal, decimal, octal, or binary.
   * @return	An unsigned integer.
   */
  def AnyInteger = rule {
    HInteger |
    BInteger |
    DInteger |
    OInteger
  }

  /**
   * Parse a hexadecimal integer.
   * @return	An unsigned integer.
   */
  def HInteger = rule {
    ignoreCase("0x") ~ oneOrMore(HDigit) ~> (UnsignedIntegerNode(_: String, 16))
  }

  /**
   * Parse a binary integer.
   * @return	An unsigned integer.
   */
  def BInteger = rule {
    ignoreCase("0b") ~ oneOrMore(BDigit) ~> (UnsignedIntegerNode(_: String, 2))
  }

  /**
   * Parse a decimal integer.
   * @return	An unsigned integer.
   */
  def DInteger = rule {
    group(("1" - "9") ~ zeroOrMore(DDigit)) ~>
    (UnsignedIntegerNode(_: String, 10))
  }

  /**
   * Parse an octal integer.
   * @return	An unsigned integer.
   */
  def OInteger = rule {
    group("0" ~ zeroOrMore(ODigit)) ~> (UnsignedIntegerNode(_: String, 8))
  }

  /** Parse a decimal digit. */
  def DDigit = rule { "0" - "9" }
  
  /** Parse an octal digit. */
  def ODigit = rule { "0" - "7" }
  
  /** Parse a hexadecimal digit. */
  def HDigit = rule { "0" - "9" | "a" - "f" | "A" - "F" }
  
  /** Parse a binary digit. */
  def BDigit = rule { "0" | "1" }
  
  //======================================================================
  // Define an operator.
  //======================================================================
  
  /**
   * Parse an operator definition.
   * 
   * Operator definitions look as follows.
   * 
   * An operator whose definition is provided by a body.  Whenever the operator
   * is encountered, it is replaced at construction time by binding the formal
   * parameters and then rewriting the body.  It is essentially a macro.
   * {{{
   *   operator abel($x: STRING, $y: ^TYPE): ^TYPE = cain($x, seth($y))
   * }}}
   * A symbolic operator whose properties are specified, if any.
   * {{{
   *   operator join($x: ^TYPE, $y: ^TYPE): ^TYPE
   *   operator product($x: NUMBER, $y: NUMBER): NUMBER is
   *     associative, commutative, absorber 0, identity 1
   *   operator or($p: BOOLEAN, $q: BOOLEAN): BOOLEAN is
   *     associative, commutative, idempotent, identity false, absorber true
   * }}} 
   * An operator whose definition is provided by the runtime system - that is,
   * it is implemented in software.
   * {{{
   *   native operator `+`($x: NUMBER, $y: NUMBER): NUMBER is
   *     associative, commutative, identity 0
   * }}}
   */
  def ParsedOperatorDefinition = rule {
    ParsedNativeOperatorDefinition |
    ParsedSymbolicOperatorDefinition |
    ParsedImmediateOperatorDefinition
  }
  
  /**
   * Parse a native operator definition.
   * {{{
   * native operator `+`($x: NUMBER, $y: NUMBER): NUMBER is
   *   associative, commutative, identity 0
   * }}}
   */
  def ParsedNativeOperatorDefinition: Rule1[OperatorDefinitionNode] = rule {
    "native" ~ WS ~ "{" ~ ParsedOperatorPrototype ~ ParsedOperatorProperties ~
    WS ~ "}" ~~> (NativeOperatorDefinitionNode(_,_))
  }
  
  /**
   * Parse a symbolic operator definition.
   * {{{
   * operator join($x: ^TYPE, $y: ^TYPE): ^TYPE
   * operator product($x: NUMBER, $y: NUMBER): NUMBER is
   *   associative, commutative, absorber 0, identity 1
   * operator or($p: BOOLEAN, $q: BOOLEAN): BOOLEAN is
   *   associative, commutative, idempotent, identity false, absorber true
   * }}}
   */
  def ParsedSymbolicOperatorDefinition: Rule1[OperatorDefinitionNode] = rule {
    "operator" ~ WS ~ "{" ~ ParsedOperatorPrototype ~ ParsedOperatorProperties ~
    WS ~ "}" ~~> (SymbolicOperatorDefinitionNode(_,_))
  }
  
  /**
   * Parse a native operator definition.
   * {{{
   * operator abel($x: STRING, $y: ^TYPE): ^TYPE = cain($x, seth($y))
   * }}}
   */
  def ParsedImmediateOperatorDefinition: Rule1[OperatorDefinitionNode] = rule {
    "operator" ~ WS ~ "{" ~ ParsedOperatorPrototype ~ ParsedImmediateDefinition ~
    WS ~ "}" ~~> (ImmediateOperatorDefinitionNode(_,_))
  }
  
  /** Parse an operator prototype. */
  def ParsedOperatorPrototype = rule {
    WS ~ ESymbol ~ optional(ParsedTypeParameterList) ~ WS ~
    "(" ~ optional(ParsedParameterList) ~ WS ~ ")" ~ WS ~ ":" ~ Atom ~~>
    ((name:NakedSymbolNode, typepars:Option[List[VariableNode]],
        pars:Option[List[VariableNode]], typ:AstNode) =>
          new OperatorPrototypeNode(name.str, typepars, pars, typ))
  }

  /** Parse a type parameter list. */
  def ParsedTypeParameterList = rule {
    "[" ~ ParsedParameterList ~ WS ~ "]"
  }
  
  /** Parse a parameter list. */
  def ParsedParameterList = rule {
    ParsedTypedVariable ~
    zeroOrMore(WS ~ "," ~ WS ~ ParsedTypedVariable) ~~> (_ :: _)
  }
  
  /** Parse an immediate definition for an operator. */
  def ParsedImmediateDefinition = rule {
    "=" ~ Atom
  }
  
  /** Parse a sequence of operator properties. */
  def ParsedOperatorProperties = {
    val pop = new OperatorPropertiesNode
    rule {
      optional(WS ~ "is" ~
	    	WS ~ ParsedOperatorProperty(pop) ~ zeroOrMore(
	    	    WS ~ "," ~ WS ~ ParsedOperatorProperty(pop))) ~~> (x => pop)
    }
  }
  
  /**
   * Parse an operator property.
   * @param pop		An operator properties node to fill in.
   */
  def ParsedOperatorProperty(pop: OperatorPropertiesNode) =
    rule {
      "associative" ~> (x => pop.isAssociative = true) |
      "commutative" ~> (x => pop.isCommutative = true) |
      "idempotent" ~> (x => pop.isIdempotent = true) |
      "absorber" ~ Atom ~~> (abs => pop.withAbsorber = Some(abs)) |
      "identity" ~ Atom ~~> (id => pop.withIdentity = Some(id)) ~~> (x => pop)
  	}

  //======================================================================
  // Other methods affecting the parse.
  //======================================================================
  
  /**
   * Eliminate trailing whitespace.  This trick is found on the Parboiled web
   * site in the examples.
   * @param string	Parsed text.
   */
  override implicit def toRule(string: String) =
    if (string.endsWith(" ")) str(string.trim) ~ WS else str(string)
}
