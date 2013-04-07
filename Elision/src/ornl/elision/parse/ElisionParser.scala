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
import org.parboiled.errors.ErrorUtils
import scala.io.Source
import org.parboiled.Context
import ornl.elision.util.Loc

/**
 * Implement a parser for Elision atoms.
 * 
 * @param name    Name of the input source (typically a filename).  This should
 *                be `"(console)"` for the console, and `""` for internal use.
 * @param trace   If true, enable tracing.  False by default.
 */
class ElisionParser(val name: String, trace: Boolean = false)
extends Parser with AbstractParser {
  import AST._
  
  /**
   * Transform a Parboiled context into a location.
   * 
   * @param context The parboiled context.
   * @return The new location instance.
   */
  implicit def toLoc(context: Context[_]) = context.getPosition match {
    case pos => Loc(name, pos.line, pos.column, Some(context.getMatch))
  }
  
  //----------------------------------------------------------------------
  // Perform parsing.
  //----------------------------------------------------------------------
    
  /**
   * Entry point to parse all atoms from the given source.
   * 
   * @param source  The input source.
   * @return  The parsing result.
   */
  def parseAtoms(source: Source): Presult = {
    val tr =
      if (trace) TracingParseRunner(Atoms)
      else ReportingParseRunner(Atoms)
    val parsingResult = tr.run(source)
    parsingResult.result match {
      case Some(nodes) =>
        Success(nodes)
        
      case None =>
        Failure("Invalid Elision source:\n" +
            ErrorUtils.printParseErrors(parsingResult))
    }
  }

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
  def Atom: Rule1[BA] = rule {
    FirstAtom ~ WS ~ (
        "-> " ~ Atom ~~> {
          (left: BA, right: BA) =>
            mappair(left, right)
        } |
        zeroOrMore(". " ~ FirstAtom) ~~> {
          (first: BA, rest: List[BA]) =>
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
   *             | ParsedVariable
   *             | "\" ParsedVariable "." FirstAtom
   *             | "{:" Atom Atom ":}"
   *             | "{" ESymbol Atom* ("#" ESymbol ("=" Atom | ParsedAtomSeq))* "}"
   *             | "%" ParsedAlgProp ("(" ParsedRawAtomSeq ")")?
   *             | ESymbol ( "(" ParsedRawAtomSeq ")"
   *                       | ":" ("OPREF" | "RSREF" | FirstAtom)
   *                       )?
   * }}}
   */
  def FirstAtom: Rule1[BA] = rule {
    WS ~ (
        "( " ~ Atom ~ ") " |
        
        "^TYPE " ~> {
          (x => typeuniverse)
        } |
        
        ParsedVariable |
        
        "\\ " ~ ParsedVariable ~ ". " ~ FirstAtom ~~> withContext{
          (lvar, lbody, context) =>
          (lambda(lvar, lbody))
        } |
        
        "{: " ~ Atom ~ Atom ~ ":} " ~~> withContext{
          (left, right, context) =>
          (special(left, right, context))
        } |
        
        "{! " ~ OperatorPrototype ~
        optional(optional("is ") ~ optional("%") ~
            (OperatorPropertiesNode | ParsedAlgProp)) ~
        zeroOrMore(
          "#" ~ ESymbol ~ (
              "= " ~ Atom |
              ParsedRawAtomSeq ~~> {
                // Convert the sequence of atoms into a sequence atom.
                atomseq(noprops, _)
              }
          )
        ) ~ "} " ~~> withContext{
          // Add pairs to the body list for the parts that are specified by
          // the prototype.
          (proto, proplist, binds, context) =>
            special(sym("operator"), binding(List(
                "name"->proto._1,
                "params"->atomseq(algprop(proplist.getOrElse(List())), proto._2),
                "type"->proto._3) ++ binds), context)
        } |
        
        "{ " ~ ESymbol ~ (
            zeroOrMore(Atom) ~ zeroOrMore(
                "#" ~ ESymbol ~ (
                    "= " ~ Atom |
                    ParsedRawAtomSeq ~~> {
                      // Convert the sequence of atoms into a sequence atom.
                      atomseq(noprops, _)
                    }
                )
            ) ~~> {
              // Combine the initial list with the remaining list of pairs.
              (first: List[BA], second: List[(String,BA)]) => {
                if (first.isEmpty) binding(second)
                else binding(("", atomseq(noprops, first)) :: second)
              }
            }
        ) ~ "} " ~~> withContext{
          // Build the special form using the tag and content.
          (tag, binds, context) => (special(sym(tag), binds, context))
        } |

        "%" ~ ParsedAlgProp ~ WS ~ optional("( " ~ ParsedRawAtomSeq ~ ") ") ~~>
        withContext{
          (proplist, atoms, context) =>
            val props = algprop(proplist)
            if (atoms.isEmpty) props else atomseq(props, atoms.get)
        } |

        ESymbol ~ optional(
            "( " ~ ParsedRawAtomSeq ~ ") " ~~> {
                (seq) => ('apply, atomseq(noprops, seq))
            } |
            ": " ~ (
                "OPREF " ~ push(('opref, any)) |
                "RSREF " ~ push(('rsref, any)) |
                FirstAtom ~~> {
                  ('typed, _)
                }
            )
        ) ~~> {
          // Decide what to do based on what symbol was passed along.
          (name: String, opt: Option[(Symbol, BA)]) =>
            val data = opt.getOrElse(('plain, any))
            data._1 match {
              case 'apply => apply(opref(name), data._2)
              case 'opref => opref(name)
              case 'rsref => rsref(name)
              case 'typed => sym(name, data._2)
              case 'plain => sym(name)
            }
        } |

        (EVerb | EString) ~ optional(": " ~ FirstAtom) ~~> {
          (name, otyp) => otyp match {
            case Some(typ) => string(name, typ)
            case None => string(name)
          }
        } |
        
        AnyNumber ~ optional(":" ~ FirstAtom) ~~> {
          // Construct a number from all the pieces.  The number can be either
          // an integer or a float.
          (flag: Option[Boolean],
              whole: (Int, String),
              frac: Option[(Int, String)],
              exp: Option[(Boolean, Int, String)], 
              otyp: Option[BA]) =>
            number(flag, whole, frac, exp, otyp)
        }
    )
  }.label("a simple atom")
  
  //----------------------------------------------------------------------
  // Parse an operator prototype.
  //----------------------------------------------------------------------
  
  /**
   * Parse an operator prototype.
   * 
   * {{{
   * OperatorPrototype ::= ESymbol "(" ParsedRawAtomSeq ")" (":" FirstAtom)?
   * }}}
   */
  def OperatorPrototype = rule {
    ESymbol ~ "( " ~ ParsedRawAtomSeq ~ ") " ~ optional(": " ~ FirstAtom) ~~> {
      (name: String, params: List[BA], typ: Option[BA]) =>
        (sym(name), params, typ.getOrElse(any))
    }
  }
  
  //----------------------------------------------------------------------
  // Parse a variable.
  //----------------------------------------------------------------------
  
  /**
   * Parse a variable or metavariable.
   * 
   * {{{
   * ParsedVariable ::= ( "`$``$`" | "`$`" ) ESymbol
   *                    ("{" Atom "}")? (":" FirstAtom)? ("@" ESymbol)*
   * }}}
   */
  def ParsedVariable = rule {
    ("$$" ~ push(true) | "$" ~ push(false)) ~ (
        (ESymbol ~ push(false) | EString ~ push(true)) ~
        optional("{ " ~ Atom ~ "} ") ~
        optional(": " ~ FirstAtom) ~
        zeroOrMore("@" ~ ESymbol)
    ) ~~> (variable(_, _, _, _, _, _))
  }.label("a variable")
  
  //----------------------------------------------------------------------
  // Symbols and strings.
  //----------------------------------------------------------------------
    
  /**
   * Parse a verbatim block.
   * 
   * {{{
   * EVerb ::= [ any text in triple double-quotation-marks ]
   * }}}
   */
  def EVerb = rule {
    "\"\"\"".suppressNode ~
    zeroOrMore(&(!"\"\"\"") ~ PANY) ~> (x => x) ~
    "\"\"\" ".suppressNode
  }.label("a verbatim block")

  /**
   * Parse a double-quoted string.
   * 
   * {{{
   * EString ::= "\"" Character* "\""
   * }}}
   */
  def EString = rule {
    "\"".suppressNode ~
    zeroOrMore(Character) ~~> (x => construct(x)) ~
    "\" ".suppressNode
  }.label("a string")

  /**
   * Parse a character in a string.  This may be an escape.
   * 
   * {{{
   * Character ::= EscapedCharacter | NormalCharacter
   * }}}
   */
  def Character = rule {
    (EscapedCharacter | NormalCharacter) ~> (x => x)
  }.label("a single character")

  /**
   * Parse an escaped character.
   * 
   * {{{
   * EscapedCharacter ::= "\" ("'" | "\"" | "n" | "r" | "t" | "\")
   * }}}
   */
  def EscapedCharacter = rule {
    "\\" ~ anyOf("""`"nrt\""")
  }.label("a character escape sequence")

  /**
   * Parse a normal character.
   * 
   * {{{
   * NormalCharacter ::= [not \ or "]
   * }}}
   */
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
   * 
   * {{{
   * ParsedRawAtomSeq ::= Atom ("," ParsedRawAtomSeq)*
   * }}}
   */
  def ParsedRawAtomSeq = rule {
    zeroOrMore(Atom, ", ")
  }.label("a comma-separated list of atoms")
  
  /**
   * Parse an algebraic properties specification.
   * 
   * {{{
   * ParsedAlgProp ::= OperatorPropertiesNode
   *                 | ( "B[" Atom "]"
   *                   | "D[" Atom "]"
   *                   | "A" ("[" Atom "]")?
   *                   | "C" ("[" Atom "]")?
   *                   | "D" ("[" Atom "]")?
   *                   | "!A"
   *                   | "!C"
   *                   | "!I" )*
   * }}}
   */
  def ParsedAlgProp = rule {
    (zeroOrMore(
        ignoreCase("B") ~ "[ " ~ Atom ~ "]" ~~> (absorber(_)) |
        ignoreCase("D") ~ "[ " ~ Atom ~ "]" ~~> (identity(_)) |
        ignoreCase("A") ~ optional("[ " ~ Atom ~ "]") ~~>
          ((x) => associative(x.getOrElse(known(true)))) |
        ignoreCase("C") ~ optional("[ " ~ Atom ~ "]") ~~>
          ((x) => commutative(x.getOrElse(known(true)))) |
        ignoreCase("I") ~ optional("[ " ~ Atom ~ "]") ~~>
          ((x) => idempotent(x.getOrElse(known(true)))) |
        ignoreCase("!A") ~> ((x) => associative(known(false))) |
        ignoreCase("!C") ~> ((x) => commutative(known(false))) |
        ignoreCase("!I") ~> ((x) => idempotent(known(false)))
      ) ~ WS |
      WS ~ OperatorPropertiesNode
    )
  }.label("an algebraic properties specification.")
  
  /**
   * Parse a long-form algebraic properties specification.
   * 
   * {{{
   * OperatorPropertiesNode ::=
   *   ( "not"? ( "associative" | "commutative" | "idempotent" )
   *   | "absorber" Atom
   *   | "identity" Atom ) ("," OperatorPropertiesNode)*
   * }}}
   */
  def OperatorPropertiesNode = rule {
    oneOrMore(
        ignoreCase("absorber") ~ WS ~ Atom ~~> (absorber(_)) |
        ignoreCase("identity") ~ WS ~ Atom ~~> (identity(_)) |
        ignoreCase("not") ~ WS ~ (
          ignoreCase("associative") ~> ((x) => associative(known(false))) |
          ignoreCase("commutative") ~> ((x) => commutative(known(false))) |
          ignoreCase("idempotent") ~> ((x) => idempotent(known(false)))
        ) |
        ignoreCase("associative") ~> ((x) => associative(known(true))) |
        ignoreCase("commutative") ~> ((x) => commutative(known(true))) |
        ignoreCase("idempotent") ~> ((x) => idempotent(known(true))),
        WS ~ ", ") ~ WS
  }.label("operator properties")

  //----------------------------------------------------------------------
  // Parse numbers.
  //----------------------------------------------------------------------
  
  /**
   * Parse a number.  The number can be an integer or a float, and can be
   * positive or negative.
   */
  def AnyNumber = rule {
    optional("-" ~ push(true)) ~ (
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
      optional("." ~ zeroOrMore(HDigit) ~> {
        (16, _)
      }) ~
      optional(ignoreCase("p") ~ Exponent)
  }.label("a hexadecimal number")

  /**
   * Parse a binary number that may be either an integer or a float.
   */
  def BNumber = rule {
    BInteger ~
      optional("." ~ zeroOrMore(BDigit) ~> {
        (2, _)
      }) ~
      optional((ignoreCase("e") | ignoreCase("p")) ~ Exponent)
  }.label("a binary number")

  /**
   * Parse a decimal number that may be either an integer or a float.
   */
  def DNumber = rule {
    DInteger ~
      optional("." ~ zeroOrMore(DDigit) ~> {
        (10, _)
      }) ~
      optional((ignoreCase("e") | ignoreCase("p")) ~ Exponent)
  }.label("a decimal number")

  /**
   * Parse an octal number that may be either an integer or a float.
   */
  def ONumber = rule {
    OInteger ~
      optional(".".suppressNode ~ zeroOrMore(ODigit) ~> {
        (8, _)
      }) ~
      optional((ignoreCase("e") | ignoreCase("p")) ~ Exponent)
  }.label("an octal number")

  /**
   * Parse an exponent expression.  The expression does not include the
   * linking "e" or "p" exponent indicator.
   */
  def Exponent = rule {
      optional("+" ~ push(false) | "-" ~ push(true)) ~ AnyInteger ~~> {
        (sign, num) => (sign.getOrElse(false), num._1, num._2)
      }
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
    ignoreCase("0x").suppressNode ~ oneOrMore(HDigit) ~> {
      (16, _)
    }
  }.label("a hexadecimal integer")

  /**
   * Parse a binary integer.
   * @return  An unsigned integer.
   */
  def BInteger = rule {
    ignoreCase("0b").suppressNode ~ oneOrMore(BDigit) ~> {
      (2, _)
    }
  }.label("a binary integer")

  /**
   * Parse a decimal integer.
   * @return  An unsigned integer.
   */
  def DInteger = rule {
    group(("1" - "9") ~ zeroOrMore(DDigit)) ~> {
      (10, _)
    }
  }.label("a decimal integer")

  /**
   * Parse an octal integer.
   * @return  An unsigned integer.
   */
  def OInteger = rule {
    group("0" ~ zeroOrMore(ODigit)) ~> {
      (8, _)
    }
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