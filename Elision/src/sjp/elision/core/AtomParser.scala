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
 * A node representing a "naked" operator.
 * @param str	The operator name.
 */
case class OperatorNode(str: String) extends AstNode {
  def interpret = Operator(str)
}

/**
 * An abstract syntax tree node holding a simple list of atoms.
 * @param list	The actual list of atom nodes.
 */
case class AtomListNode(list: List[AstNode]) extends AstNode {
  var props: Option[(Boolean, Boolean)] = None
  def interpret = AtomList(list map (_.interpret))
  def setProperties(assoc: Boolean, comm: Boolean) = {
    props = Some(assoc, comm)
    this
  }
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
// Literal nodes - that is, nodes that hold literal values.
//----------------------------------------------------------------------

/**
 * A node representing a symbol literal.
 * @param typ		The type.
 * @param sym		The symbol text.
 */
case class SymbolLiteralNode(typ: AstNode, sym: String) extends AstNode {
  println("Making a symbol: " + sym + ":" + typ.toString)
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
  def apply(sign: Option[Boolean], integer: UnsignedIntegerNode):
  SignedIntegerNode =
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
  
  def Atom: Rule1[AstNode] = rule {
    // Handle the special case of the general operator application.
    FirstAtom ~ "." ~ Atom ~~> (
      (op: AstNode, arg: AstNode) => ApplicationNode(op, arg)) |
    // Parse an atom.
    FirstAtom
  }

  def FirstAtom: Rule1[AstNode] = rule {
    ParsedWhitespace ~ (
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
      ESymbol ~ ":" ~ "OPTYPE" ~~> (
          (sym: NakedSymbolNode) => OperatorNode(sym.str)) |

      // Parse a literal.  A literal can take many forms, but it should be
      // possible to always detect the kind of literal during parse.  By
      // default literals go into a simple type, but this can be overridden.
      ParsedLiteral |
      
      AnyNumber ~ ":" ~ Atom ~~> (
          (num:NumberNode, typ:AstNode) => num.retype(typ)) |
      AnyNumber |

      // Parse the special type universe.
      "^TYPE" ~> (x => TypeUniverseNode()))
  }

  def ParsedApply = rule {
    // Parse an operator application.  This just applies an operator to
    // some other atom.  The operator name is given as a symbol, and the
    // argument may be enclosed in parens if it is a list, or joined to
    // the operator with a dot if not.  The dot form is parsed elsewhere.
    //
    // If you want to use a general atom, use a dot to join it to the argument.
    // The same comment applies if you want to use a general atom as the
    // argument.
    ESymbol ~ "(" ~ ParsedAtomList ~ ")" ~~> (
      (op: NakedSymbolNode, arg: AtomListNode) => ApplicationNode(op, arg))
  }
  
  def ParsedLiteral = rule {
      ESymbol ~ ":" ~ Atom ~~>
      	((sym: NakedSymbolNode, typ: AstNode) =>
      	  SymbolLiteralNode(typ, sym.str)) |
      ESymbol ~~>
      	((sym: NakedSymbolNode) =>
      	  SymbolLiteralNode(SimpleTypeNode(SYMBOL), sym.str)) |
      EString ~ ":" ~ Atom ~~>
      	((str: String, typ: AstNode) => StringLiteralNode(typ, str)) |
      EString ~~>
      	((str: String) => StringLiteralNode(SimpleTypeNode(STRING), str))
  }

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
      ("AC" | "CA") ~ "(" ~ ParsedAtomList ~ ")" ~~> (
        (list: AtomListNode) => list.setProperties(true, true)) |
      "A" ~ "(" ~ ParsedAtomList ~ ")" ~~> (
        (list: AtomListNode) => list.setProperties(true, false)) |
      "C" ~ "(" ~ ParsedAtomList ~ ")" ~~> (
        (list: AtomListNode) => list.setProperties(false, true)) |
      "(" ~ ParsedAtomList ~ ")" ~~> (
        (list: AtomListNode) => list.setProperties(false, false)))
  }

  /**
   * Parse a list of atoms, separated by commas.  No concept of associativity,
   * commutativity, etc., is inferred at this point.
   */
  def ParsedAtomList = rule {
    (Atom ~ zeroOrMore("," ~ Atom)) ~~> (
      (head: AstNode, tail: List[AstNode]) => AtomListNode(head :: tail))
  }

  /**
   * Parse a rule.
   */
  def ParsedRule = rule(
    // First there are optional variable declarations.  In a rule, all pattern
    // variables must be declared.
    zeroOrMore("@" ~ ParsedVariable ~ zeroOrMore("," ~ ParsedVariable)) ~

      // Next is the rule itself, consisting of a pattern, a rewrite, and zero
      // or more guards.
      ignoreCase("rule") ~ Atom ~ "->" ~ Atom ~ zeroOrMore("if" ~ Atom) ~

      // Next the rule can be declared to be in zero or more rulesets.
      optional((ignoreCase("rulesets") | ignoreCase("ruleset")) ~ ESymbol ~
        zeroOrMore("," ~ ESymbol)) ~

      // Finally the rule's cache level can be declared.  This must be the
      // last item, if present.
      optional("level" ~ DInteger))

  /**
   * Parse a variable.
   */
  def ParsedVariable = rule {
    "$" ~ ESymbol ~ ":" ~ Atom ~~> (
      (sval: NakedSymbolNode, typ: AstNode) => VariableNode(typ, sval.str)) |
    "$" ~ ESymbol ~~> (sval => VariableNode(TypeUniverseNode(), sval.str))
  }

  //======================================================================
  // Parse whitespace.
  //======================================================================

  /** Parse ignorable whitespace. */
  def ParsedWhitespace = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

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
  def EscapedCharacter = rule { "\\" ~ anyOf("\"\\nrt") }

  /** Parse a normal character. */
  def NormalCharacter = rule { noneOf("\"\\") }

  //======================================================================
  // Parse a symbol.
  //======================================================================

  /** Parse a symbol. */
  def ESymbol = rule {
    "`" ~ zeroOrMore(SymChar) ~> (NakedSymbolNode(_)) ~ "`" |
    group(("a" - "z" | "A" - "Z" | "_") ~ zeroOrMore(
      "a" - "z" | "A" - "Z" | "0" - "9" | "_")) ~> (NakedSymbolNode(_))
  }

  def SymChar = rule { SymEsc | SymNorm }
  def SymEsc = rule {
    "\\" ~ anyOf("`\\nrt")
  }
  def SymNorm = rule { noneOf("`\\") }

  //======================================================================
  // Parse a number.
  //======================================================================

  /* Numbers are parsed in a somewhat unusual way.  We parse the number, and
		 * make a decision later if the number is a float or integer based on what
		 * we find.
		 */

  def AnyNumber: Rule1[NumberNode] = rule {
    optional("-" ~ push(true)) ~ (
      HNumber |
      BNumber |
      DNumber |
      ONumber) ~~> (NumberNode(_:Option[Boolean],_:UnsignedIntegerNode,
          _:Option[UnsignedIntegerNode],_:Option[SignedIntegerNode]))
  }

  def HNumber = rule {
    HInteger ~
      optional("." ~ zeroOrMore(HDigit) ~> (UnsignedIntegerNode(_, 16))) ~
      optional(ignoreCase("p") ~ Exponent)
  }

  def BNumber = rule {
    BInteger ~
      optional("." ~ zeroOrMore(BDigit) ~> (UnsignedIntegerNode(_, 2))) ~
      optional((ignoreCase("e") | ignoreCase("p")) ~ Exponent)
  }

  def DNumber = rule {
    DInteger ~
      optional("." ~ zeroOrMore(DDigit) ~> (UnsignedIntegerNode(_, 10))) ~
      optional((ignoreCase("e") | ignoreCase("p")) ~ Exponent)
  }

  def ONumber = rule {
    OInteger ~
      optional("." ~ zeroOrMore(ODigit) ~> (UnsignedIntegerNode(_, 8))) ~
      optional((ignoreCase("e") | ignoreCase("p")) ~ Exponent)
  }

  /**
   * Parse an exponent expression.
   * @return	A signed integer.
   */
  def Exponent = rule {
      optional("+" ~ push(true) | "-" ~ push(false)) ~
      AnyInteger ~~> (SignedIntegerNode(_, _))
  }

  /**
   * Parse an integer.
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

  def DDigit = rule { "0" - "9" }
  def ODigit = rule { "0" - "7" }
  def HDigit = rule { "0" - "9" | "a" - "f" | "A" - "F" }
  def BDigit = rule { "0" | "1" }

  //======================================================================
  // Other methods affecting the parse.
  //======================================================================

  /**
   * Eliminate trailing whitespace.  This trick is found on the Parboiled web
   * site in the examples.
   */
  override implicit def toRule(string: String) =
    if (string.endsWith(" ")) str(string.trim) ~ ParsedWhitespace
    else str(string)
}
