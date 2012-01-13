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
  
  def Atom: Rule0 = rule {
    zeroOrMore(Whitespace ~ (
        // Parse an operator application.  This just applies an operator to
        // some other atom.  The operator name is given as a symbol, and the
        // argument may be enclosed in parens if it is a list, or joined to
        // the operator with a dot if not.
        ESymbol ~ "." ~ Atom |
        ESymbol ~ "(" ~ AtomList ~ ")" |
        
        // Parse an atom list that specifies its properties.  There are
        // multiple different forms of lists.  The associativity or
        // commutativity of the list is specified by writing a percent sign and
        // an A (for associativity) or C (for commutativity), followed by the
        // list in parens.
        // Note that f(x,y,z), if f is associative, could be written as:
        // f.%A(x,y,z)
        "%" ~ ("AC" | "CA" | "C" | "A") ~ "(" ~ AtomList ~ ")" |
        
        // Parse a variable.  The leading dollar sign is used to distinguish
        // between a symbol and a variable.  If a type is not specified for a
        // variable, it gets put in the type universe.
        "$" ~ ESymbol ~ ":" ~ Atom |
        "$" ~ ESymbol |
        
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
      )
    )
  }
  
  /**
   * Parse a list of atoms, separated by commas.  No concept of associativity,
   * commutativity, etc., is inferred at this point.
   */
  def AtomList = rule {
    Atom ~ zeroOrMore("," ~ Atom)
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
    "\"" ~ zeroOrMore(Character) ~ "\""
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
    "`" ~ zeroOrMore(SymChar) ~ "`" |
      (("a" - "z" | "A" - "Z" | "_") ~ zeroOrMore(
        "a" - "z" | "A" - "Z" | "0" - "9" | "_"))
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
    HInteger ~ optional("." ~ zeroOrMore(HDigit)) ~ optional(Exponent)
  }

  def BNumber = rule {
    BInteger ~ optional("." ~ zeroOrMore(BDigit)) ~ optional(Exponent)
  }

  def DNumber = rule {
    DInteger ~ optional("." ~ zeroOrMore(DDigit)) ~ optional(Exponent)
  }

  def ONumber = rule {
    OInteger ~ optional("." ~ zeroOrMore(ODigit)) ~ optional(Exponent)
  }

  def Exponent = rule {
    (ignoreCase("e") | ignoreCase("p")) ~ optional("+" | "-") ~ AnyInteger
  }

  def AnyInteger = rule {
    HInteger |
      BInteger |
      DInteger |
      OInteger
  }

  def HInteger = rule {
    ignoreCase("0x") ~ oneOrMore(HDigit)
  }

  def BInteger = rule {
    ignoreCase("0b") ~ oneOrMore(BDigit)
  }

  def DInteger = rule {
    ("1" - "9") ~ zeroOrMore(DDigit)
  }

  def OInteger = rule {
    "0" ~ zeroOrMore(ODigit)
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
