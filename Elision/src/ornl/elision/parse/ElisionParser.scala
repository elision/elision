/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2013 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package ornl.elision.parse

import org.parboiled.scala.{ANY => PANY}
import org.parboiled.scala._
import ornl.elision.core.Context
import ornl.elision.core.Fickle
import ornl.elision.core.BasicAtom
import ornl.elision.core.TypeUniverse
import ornl.elision.core.MapPair
import ornl.elision.core.Apply
import ornl.elision.core.Identity
import ornl.elision.core.Absorber


/**
 * Base class for abstract syntax tree nodes.
 */
abstract class AST {
  def interpret(context: Context): BasicAtom
}

/**
 * Create abstract syntax tree nodes.
 */
object AST {
  /**
   * Make a simple AST around a known atom.
   * @param atom  The atom to store.
   * @return  The AST node.
   */
  def known(atom: BasicAtom) = new AST {
    def interpret(context: Context) = atom
  }
  def mappair(left: AST, right: AST) = new AST {
    def interpret(context: Context) =
      MapPair(left.interpret(context), right.interpret(context))
  }
  def apply(left: AST, right: AST) = new AST {
    def interpret(context: Context) = 
      Apply(left.interpret(context), right.interpret(context))
  }
  def absorber(atom: AST) = new AST {
    def interpret(context: Context) = Absorber(atom.interpret(context))
  }
  
}


/**
 * Implement a parser for Elision atoms.
 * 
 * @param context The context to search for rulesets and operators.
 * @param trace   If true, enable tracing.  False by default.
 */
class ElisionParser(val context: Context, val trace: Boolean = false)
extends Parser with Fickle {
  import AST._

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
   * @param front The string to get the new character appended at the end.
   * @param last  The new character, possibly an escape to interpret.
   * @return  The new string.
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
   * @param chars The list of characters.
   * @return  The composed string.
   */
  def construct(chars: List[String]) = {
    chars.foldLeft(new StringBuilder())(append(_, _)).toString
  }

  //----------------------------------------------------------------------
  // Parse sequences of atoms.
  //----------------------------------------------------------------------
  
  /**
   * Parse a sequence of atoms.  The sequence may consist of zero or more
   * atoms.  The end of input is matched here.
   * 
   * {{{
   * Atoms ::= Atom* EOI
   * }}}
   */
  def Atoms = rule {
    zeroOrMore(Atom) ~ WS ~ EOI
  }.label("a sequence of atoms")
  
  /**
   * Parse an atom.
   * 
   * This method handles map pairs and the applicative dot.  The map pair
   * is checked first, followed by the applicative dot, giving the dot the
   * lowest priority.  While the map pair cannot be repeated, the applicative
   * dot can be, and it is treated as right associative.
   * 
   * {{{
   * Atom ::= FirstAtom ("->" Atom | ("." FirstAtom)*)
   * }}}
   */
  def Atom: Rule1[AST] = rule {
    FirstAtom ~ WS ~ (
        "-> " ~ Atom ~~> {
          (left: AST, right: AST) =>
            mappair(left, right)
        } |
        zeroOrMore(". " ~ FirstAtom) ~~> {
          (first: AST, rest: List[AST]) =>
            rest.foldLeft(first)(apply(_, _))
        }
    )
  }.label("an atom")
  
  /**
   * Parse an atom with the exception of a map pair or general application.
   * Most atoms are matched at this point.
   * 
   * {{{
   * FirstAtom ::= "(" Atom ")"
   *             | "^TYPE"
   * }}}
   */
  def FirstAtom: Rule1[AST] = rule {
    WS ~ (
        "( " ~ Atom ~ ") " |
        
        "^TYPE " ~> (x => known(TypeUniverse))
        
//        ParsedVariable |
        
//        "\\ " ~ ParsedVariable ~ ". " ~ FirstAtom |
        
//        "{: " ~ Atom ~ Atom ~ ":} " |
        
//        "{ " ~ ESymbol ~ (
//            zeroOrMore(Atom) ~ zeroOrMore(
//                "#" ~ ESymbol ~ (
//                    "= " ~ Atom |
//                    zeroOrMore(Atom, ", ")
//                )
//            )
//        ) ~ "} " |

//        "%" ~ ParsedAlgProp ~ WS ~ optional("( " ~ ParsedRawAtomSeq ~ ") ") |

//        ESymbol ~ (
//            "( " ~ ParsedRawAtomSeq ~ ") " |
//            ": " ~ (
//                "OPREF " |
//                "RSREF " |
//                FirstAtom
//            )
//        ) |

//        (EVerb | EString | AnyNumber) ~ optional(": " ~ FirstAtom)
    )
  }.label("a simple atom")
  
  //----------------------------------------------------------------------
  // Parse a variable.
  //----------------------------------------------------------------------
  
  def ParsedVariable = rule {
    ("$$" | "$") ~ (
        (ESymbol | EString) ~
        optional("{ " ~ Atom ~ "} ") ~
        optional(": " ~ FirstAtom) ~
        zeroOrMore("@" ~ ESymbol)
    )
  }.label("a variable")
  
  //----------------------------------------------------------------------
  // Symbols and strings.
  //----------------------------------------------------------------------
    
  /** Parse a verbatim block. */
  def EVerb = rule {
    "\"\"\"".suppressNode ~
    zeroOrMore(&(!"\"\"\"") ~ PANY) ~> (x => x) ~
    "\"\"\" ".suppressNode
  }.label("a verbatim block")

  /** Parse a double-quoted string. */
  def EString = rule {
    "\"".suppressNode ~
    zeroOrMore(Character) ~~> (x => construct(x)) ~
    "\" ".suppressNode
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
  def NormalCharacter = rule {
    noneOf(""""\""")
  }.label("a character")

  /** Parse a symbol. */
  def ESymbol = rule {
    "`".suppressNode ~
    zeroOrMore(SymChar) ~~> (x => construct(x)) ~
    "` ".suppressNode |
    group(("a" - "z" | "A" - "Z" | "_") ~
        zeroOrMore("a" - "z" | "A" - "Z" | "0" - "9" | "_")) ~> (x => x) ~ WS
  }.label("a symbol")

  /** Parse a character that is part of a symbol. */
  def SymChar = rule {
    (EscapedCharacter | SymNorm) ~> (x => x)
  }.label("a single character")
  
  /** Parse a "normal" non-escaped character that is part of a symbol. */
  def SymNorm = rule {
    noneOf("""`\""")
  }.label("a character")

  //----------------------------------------------------------------------
  // Parse a list of atoms.
  //----------------------------------------------------------------------
  
  /**
   * Parse a list of atoms, separated by commas.  No concept of associativity,
   * commutativity, etc., is inferred at this point.
   */
  def ParsedRawAtomSeq = rule {
    zeroOrMore(Atom, ", ")
  }.label("a comma-separated list of atoms")
  
  /** Parse an algebraic properties specification. */
  def ParsedAlgProp = rule {
    (zeroOrMore(
        ignoreCase("B") ~ "[ " ~ Atom ~ "]" ~~>
          (absorber(_)) |
        ignoreCase("D") ~ "[ " ~ Atom ~ "]" ~~>
          (known(Identity(_)))|
        ignoreCase("A") ~ optional("[ " ~ Atom ~ "]") |
        ignoreCase("C") ~ optional("[ " ~ Atom ~ "]") |
        ignoreCase("I") ~ optional("[ " ~ Atom ~ "]") |
        ignoreCase("!A") |
        ignoreCase("!C") |
        ignoreCase("!I")) ~ WS |
        WS ~ OperatorPropertiesNode
    )
  }.label("an algebraic properties specification.")
  
  /** Parse an operator properties block. */
  def OperatorPropertiesNode = rule {
    oneOrMore(
        ignoreCase("absorber") ~ WS ~ Atom |
        ignoreCase("identity") ~ WS ~ Atom |
        ignoreCase("not") ~ WS ~ (
          ignoreCase("associative") |
          ignoreCase("commutative") |
          ignoreCase("idempotent")
        ) |
        ignoreCase("associative") |
        ignoreCase("commutative") |
        ignoreCase("idempotent"), WS ~ ", ") ~ WS
  }.label("operator properties")

  //----------------------------------------------------------------------
  // Parse numbers.
  //----------------------------------------------------------------------
  
  /**
   * Parse a number.  The number can be an integer or a float, and can be
   * positive or negative.
   */
  def AnyNumber = rule {
    optional("-") ~ (
      HNumber |
      BNumber |
      DNumber |
      ONumber) ~ WS
  }.label("an integer or floating point number")

  /**
   * Parse a hexadecimal number that may be either an integer or a float.
   */
  def HNumber = rule {
    HInteger ~
      optional("." ~ zeroOrMore(HDigit)) ~
      optional(ignoreCase("p") ~ Exponent)
  }.label("a hexadecimal number")

  /**
   * Parse a binary number that may be either an integer or a float.
   */
  def BNumber = rule {
    BInteger ~
      optional("." ~ zeroOrMore(BDigit)) ~
      optional((ignoreCase("e") | ignoreCase("p")) ~ Exponent)
  }.label("a binary number")

  /**
   * Parse a decimal number that may be either an integer or a float.
   */
  def DNumber = rule {
    DInteger ~
      optional("." ~ zeroOrMore(DDigit)) ~
      optional((ignoreCase("e") | ignoreCase("p")) ~ Exponent)
  }.label("a decimal number")

  /**
   * Parse an octal number that may be either an integer or a float.
   */
  def ONumber = rule {
    OInteger ~
      optional("." ~ zeroOrMore(ODigit)) ~
      optional((ignoreCase("e") | ignoreCase("p")) ~ Exponent)
  }.label("an octal number")

  /**
   * Parse an exponent expression.  The expression does not include the
   * linking "e" or "p" exponent indicator.
   */
  def Exponent = rule {
      optional("+" | "-") ~ AnyInteger
  }.label("an exponent")

  /**
   * Parse an integer in hexadecimal, decimal, octal, or binary.
   * @return  An unsigned integer.
   */
  def AnyInteger = rule { (
    HInteger |
    BInteger |
    DInteger |
    OInteger ) ~ WS
  }.label("an integer")

  /**
   * Parse a hexadecimal integer.
   * @return  An unsigned integer.
   */
  def HInteger = rule {
    ignoreCase("0x") ~ oneOrMore(HDigit)
  }.label("a hexadecimal integer")

  /**
   * Parse a binary integer.
   * @return  An unsigned integer.
   */
  def BInteger = rule {
    ignoreCase("0b") ~ oneOrMore(BDigit)
  }.label("a binary integer")

  /**
   * Parse a decimal integer.
   * @return  An unsigned integer.
   */
  def DInteger = rule {
    group(("1" - "9") ~ zeroOrMore(DDigit))
  }.label("a decimal integer")

  /**
   * Parse an octal integer.
   * @return  An unsigned integer.
   */
  def OInteger = rule {
    group("0" ~ zeroOrMore(ODigit))
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
  //----------------------------------------------------------------------
  //----------------------------------------------------------------------
  //----------------------------------------------------------------------
  //----------------------------------------------------------------------
  //----------------------------------------------------------------------
  //----------------------------------------------------------------------
  //----------------------------------------------------------------------
  //----------------------------------------------------------------------
  //----------------------------------------------------------------------
  
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
  // Other methods affecting the parse.
  //----------------------------------------------------------------------
  
  /**
   * Eliminate trailing whitespace.  This trick is found on the Parboiled web
   * site in the examples.
   * 
   * @param string  Parsed text.
   */
  override implicit def toRule(string: String) =
    if (string.endsWith(" ")) str(string.trim) ~ WS else str(string)
}