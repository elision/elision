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

sealed abstract class AstNode {
  def interpret: BasicAtom
}
case class AtomList(list: LinkedList[AstNode]) {
  def interpret = AtomList(list)
}
case class UnsignedInteger(digits: String, radix: Int) {
  def interpret = Literal(INTEGER, Integer.parseInt(digits, radix))
}
case class SignedInteger(sign: Boolean, digits: String, radix: Int) {
  def interpret = Literal(INTEGER,
      if (sign) Integer.parseInt(digits, radix) 
      else -Integer.parseInt(digits, radix))
}
object SignedInteger {
	def apply(sign: Option[Boolean], integer: UnsignedInteger): SignedInteger =
		sign match {
		case Some(bool) => SignedInteger(bool, integer.digits, integer.radix)
		case None => SignedInteger(true, integer.digits, integer.radix)
	}
}

/**
 * A parser to parse a single atom.
 */
class AtomParser extends Parser {
	def parseAtom(atom: String) = {
		val run = TracingParseRunner(Atom).run(atom)
		println(run.result)
		/*
    run.result match {
      case Some(parsedAtom) => parsedAtom.toString
      case None => throw new ParsingException("Invalid atom.\n" +
          ErrorUtils.printParseErrors(run))
    }
		 */
	}

	def show(x: String) = println("\n++++++++++ " + x + " ++++++++++\n")

	//======================================================================
	// Parse and build atoms.
	//======================================================================

	def Atom: Rule1[BasicAtom] = rule {
		Whitespace ~ (
				// Parse the special OPTYPE.
				"OPTYPE" ~> (x => OPTYPE) |
		    
		    // Parse the special RULETYPE.
		    "RULETYPE" ~> (x => RULETYPE) |
		    
		    // Parse a "typical" operator application.
		    OperatorApplication |
		    
		    // Parse a typed list.
		    TypedList |

				// Parse a variable.  The leading dollar sign is used to distinguish
				// between a symbol and a variable.  If a type is not specified for a
				// variable, it gets put in the type universe.
				Variable |

				// A "naked" operator is specified by explicitly giving the operator
				// type OPTYPE.  Otherwise it is parsed as a symbol.
				ESymbol ~ ":" ~ "OPTYPE" |

				// Parse a literal.  A literal can take many forms, but it should be
				// possible to always detect the kind of literal during parse.  By
				// default literals go into a simple type, but this can be overridden.
				ESymbol ~ ":" ~ Atom |
				ESymbol |
				EString ~ ":" ~ Atom |
				EString |
				AnyNumber ~ ":" ~ Atom |
				AnyNumber |
				
				// Parse the special type universe.
				"^TYPE"
		) ~% (show)
	}

	def OperatorApplication: Rule1[AstNode] = rule {
	  		// Parse an operator application.  This just applies an operator to
				// some other atom.  The operator name is given as a symbol, and the
				// argument may be enclosed in parens if it is a list, or joined to
				// the operator with a dot if not.
				ESymbol ~ "." ~ Atom |
				ESymbol ~ "(" ~ AtomList ~ ")"
	}
	
	def TypedList = rule {
	  		// Parse an atom list that specifies its properties.  There are
				// multiple different forms of lists.  The associativity or
				// commutativity of the list is specified by writing a percent sign and
				// an A (for associativity) or C (for commutativity), followed by the
				// list in parens.
				// Note that f(x,y,z), if f is associative, could be written as:
				// f.%A(x,y,z)
				"%" ~ ("AC" | "CA" | "C" | "A") ~ "(" ~ AtomList ~ ")"
	}
	
	/**
	 * Parse a list of atoms, separated by commas.  No concept of associativity,
	 * commutativity, etc., is inferred at this point.
	 */
	def AtomList: LinkedList[BasicAtom] = rule {
			var list = new LinkedList[BasicAtom]
			                          Atom ~> { list :+ _ } ~
			                          zeroOrMore("," ~
			                          		Atom ~> { list :+ _ }
			                          ) ~~> ()
		}

		/**
		 * Parse a rule.
		 */
		def Rule = rule (
				// First there are optional variable declarations.  In a rule, all pattern
				// variables must be declared.
				zeroOrMore("@" ~ Variable ~ zeroOrMore("," ~ Variable)) ~

				// Next is the rule itself, consisting of a pattern, a rewrite, and zero
				// or more guards.
				ignoreCase("rule") ~ Atom ~ "->" ~ Atom ~ zeroOrMore("if" ~ Atom) ~

				// Next the rule can be declared to be in zero or more rulesets.
				optional((ignoreCase("rulesets") | ignoreCase("ruleset")) ~ ESymbol ~
						zeroOrMore("," ~ ESymbol)) ~

						// Finally the rule's cache level can be declared.  This must be the
						// last item, if present.
						optional("level" ~ DInteger)
		)

		/**
		 * Parse a variable.
		 */
		def Variable: Rule1[BasicAtom] = rule {
			"$" ~ ESymbol ~ ":" ~ Atom ~~> (
			    (name: String, typ: BasicAtom) => Variable(typ, Symbol(name))) |
			"$" ~ ESymbol ~~> (sval => Variable(TypeUniverse, Symbol(sval)))
		}

		//======================================================================
		// Parse whitespace.
		//======================================================================

		/** Parse ignorable whitespace. */
		def Whitespace = rule { zeroOrMore(anyOf(" \n\r\t\f")) }

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
		def NormalCharacter = rule { noneOf("\"\\") ~ ANY }

		//======================================================================
		// Parse a symbol.
		//======================================================================

		/** Parse a symbol. */
		def ESymbol = rule {
			"`" ~ zeroOrMore(SymChar) ~> (_.toString) ~ "`" |
			(("a" - "z" | "A" - "Z" | "_") ~ zeroOrMore(
					"a" - "z" | "A" - "Z" | "0" - "9" | "_")) ~> (_.toString)
		}

		def SymChar = rule { SymEsc | SymNorm }
		def SymEsc = rule { "\\" ~ anyOf("`\\nrt") }
		def SymNorm = rule { noneOf("`\\") ~ ANY }

		//======================================================================
		// Parse a number.
		//======================================================================

		/* Numbers are parsed in a somewhat unusual way.  We parse the number, and
		 * make a decision later if the number is a float or integer based on what
		 * we find.
		 */

		def AnyNumber = rule {
			optional("-") ~
			(HNumber |
					BNumber |
					DNumber |
					ONumber)
		}

		def HNumber = rule {
			HInteger ~
			optional("." ~ zeroOrMore(HDigit) ~> (UnsignedInteger(_,16))) ~
			optional(Exponent)
		}

		def BNumber = rule {
			BInteger ~
			optional("." ~ zeroOrMore(BDigit) ~> (UnsignedInteger(_,2))) ~
			optional(Exponent)
		}

		def DNumber = rule {
			DInteger ~
			optional("." ~ zeroOrMore(DDigit) ~> (UnsignedInteger(_,10))) ~
			optional(Exponent)
		}

		def ONumber = rule {
			OInteger ~
			optional("." ~ zeroOrMore(ODigit) ~> (UnsignedInteger(_,8))) ~
			optional(Exponent)
		}

		/**
		 * Parse an exponent expression.
		 * @return	A signed integer.
		 */
		def Exponent = rule {
			(ignoreCase("e") | ignoreCase("p")) ~
			optional("+" ~ push(true) | "-" ~ push(false)) ~
			AnyInteger ~~> (SignedInteger(_,_))
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
			ignoreCase("0x") ~ oneOrMore(HDigit) ~> (UnsignedInteger(_:String,16))
		}

		/**
		 * Parse a binary integer.
		 * @return	An unsigned integer.
		 */
		def BInteger = rule {
			ignoreCase("0b") ~ oneOrMore(BDigit) ~> (UnsignedInteger(_:String,2))
		}

		/**
		 * Parse a decimal integer.
		 * @return	An unsigned integer.
		 */
		def DInteger = rule {
			(("1" - "9") ~ zeroOrMore(DDigit)) ~> (UnsignedInteger(_:String,10))
		}

		/**
		 * Parse an octal integer.
		 * @return	An unsigned integer.
		 */
		def OInteger = rule {
			("0" ~ zeroOrMore(ODigit)) ~> (UnsignedInteger(_:String,8))
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
			if (string.endsWith(" ")) str(string.trim) ~ Whitespace
			else str(string)
}
