/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2014 by Stacy Prowell (sprowell@gmail.com).
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

import scala.io.Source
import scala.collection.mutable.Queue
import ornl.elision.core.BasicAtom
import ornl.elision.util.ElisionException
import java.io.StringReader
import ornl.elision.util.Loc
import scala.collection.immutable.List
import ornl.elision.context.Context
import ornl.elision.core.STRING
import ornl.elision.core.StringLiteral
import ornl.elision.core.SYMBOL
import ornl.elision.core.SymbolLiteral
import ornl.elision.core.NamedRootType
import ornl.elision.core.Literal
import ornl.elision.core.BooleanLiteral
import ornl.elision.core.ANY
import ornl.elision.core.MetaVariable
import ornl.elision.core.Variable
import ornl.elision.core.Lambda
import ornl.elision.core.SpecialForm
import ornl.elision.core.Bindings
import ornl.elision.core.AlgProp
import ornl.elision.core.AtomSeq
import scala.collection.mutable.IndexedSeq
import scala.collection.immutable.Map
import scala.collection.immutable.HashMap
import java.io.BufferedReader
import java.io.InputStreamReader
import ornl.elision.core.TypeUniverse
import scala.collection.mutable.Stack
import java.io.Reader
import ornl.elision.core.BitStringLiteral
import scala.math.BigInt
import ornl.elision.core.BITSTRING
import ornl.elision.core.FloatLiteral
import ornl.elision.core.IntegerLiteral
import ornl.elision.core.FLOAT
import ornl.elision.core.INTEGER
import ornl.elision.core.MapPair
import ornl.elision.core.Apply
import ornl.elision.core.OPREF

/**
 * Parse Elision source.
 * @param name    Name of the source.
 * @param trace   Whether to trace the parse (not supported).
 */
class FastEliParser(val name: String, trace: Boolean = false)
extends AbstractParser {
  def parseAtoms(source: Reader, context: Context): Presult = {
    // Go an build a new EliParser to actually perform the parse.
    val ep = new EliParser(context, source, name, 1)
    try {
      Success(ep.parseAtoms())
    } catch {
      case ex: Exception =>
        Failure(ex.toString())
    }
  }
}

/**
 * Companion object for the fast Elision parser.  It provides the main
 * method.
 */
object EliParser {
  /**
   * Run the Elision parser.
   * @param argv    The arguments.  These are concatenated and parsed.
   */
  def main(argv: Array[String]) {
    // Concatenate all the arguments together with spaces, and then try to
    // parse.
    var count = 0
    if (argv.length > 0) {
      val source = new StringReader(argv.mkString(" "))
      val parser = new EliParser(new Context, source, "(console)", 1)
      println("Parsed atoms:")
      parser.parseAtoms() foreach {
        ba: BasicAtom =>
          println(ba.toParseString)
          count += 1
      } // Print all atoms.
    } else {
      // Now read from standard input.
      println("Reading from the standard input.")
      val reader = new BufferedReader(new InputStreamReader(System.in))
      var theLine = 0
      def getline() = {
        theLine += 1
        print("e> ")
        reader.readLine()
      }
      while (getline() match {
        case null => false
        case line =>
          val source = new StringReader(line)
          val parser = new EliParser(new Context, source, "(console)", theLine)
          try {
            parser.parseAtoms() foreach {
              ba: BasicAtom =>
                println(ba.toParseString)
                count += 1
            } // Print all atoms.
          } catch {
            case ex: ParseException =>
              println("ERROR: " + ex)
              println(line)
              println((" "*(ex.loc.column-1)) + "^")
          }
          true
      }) {} // Read until end of stream.
      println("")
      println("Processed "+count+" atoms.")
    }
  }
}

/**
 * This is a hand-coded recursive descent parser for Elision.
 * 
 * @param context The context needed to construct atoms.
 * @param source  The source stream to parse.
 * @param name    The name of the stream.
 * @param line    Starting line number for the stream.
 * @author Stacy Prowell (sprowell@gmail.com)
 */
class EliParser(context: Context, source: Reader, val name: String,
    line: Int = 1) {
  
  /** Current parenthesis depth. */
  private var parendepth = 0
  
  /** A parse worker to handle lookahead and reading. */
  private val worker = new ParseWorker(name, source)
  
  /**
   * If the most recent symbol parsed was a naked symbol, this is the text.
   * Otherwise this is null, indicating that the symbol was a typed symbol.
   */
  private var naked: String = null
  
  /**
   * Get the current location in the source.
   * @return  The current location in the source.
   */
  def getLocation() = worker.loc
  
  /**
   * Parse a sequence of atoms.  The sequence may be empty on return.  This
   * will parse until either the end of file is reached, or an unrecoverable
   * error is encountered.
   * @return  The list of atoms parsed.
   */
  def parseAtoms(): List[BasicAtom] = {
    var ret = List[BasicAtom]()
    while (! worker.isAtEof) {
      val nextAtom = parseAtom()
      nextAtom match {
        case None => // Did not find any atom.
        case Some(at) => ret :+= at
      }
    } // Loop over collecting atoms.
    return ret
  }
  
  /**
   * Parse the next atom in the stream.
   * @param what  A short name for the atom being parsed, including the article,
   *              if this atom is part of a larger atom.  This is for error
   *              messages, and can be omitted.
   * @return  The next atom from the stream.
   */
  def parseAtom(what: String = null): Option[BasicAtom] = {
    val action = what match {
      case null => ""
      case _ => "While parsing "+what+": "
    }
    
    // Consume whitespace.
    worker.consumeWhitespace()
    
    // Consume a single atom.
    val first = parseInnerAtom(what) match {
      case None => return None
      case Some(ba) => ba
    }
    val firsttext = naked
    worker.consumeWhitespace()
    
    // Check for an arrow for a map pair and then a dot for application.
    if (worker.peekAndConsume("->")) {
      // Found a map pair.  We need one more atom.
      val second = parseAtom("a map pair") match {
        case None =>
          worker.fail("Expected an atom following a map pair arrow, "+
              "but did not find one.")
        case Some(ba) =>
          ba
      }
      
      // Got both atoms.  Make the map pair and return.
      return Some(MapPair(first, second))
    } else if (worker.peekAndConsume(".")) {
      // Found the applicative dot.  We need to process all applies.
      val second = parseAtom("an apply") match {
        case None =>
          worker.fail("Expected an atom following the applicative dot, "+
              "but did not find one.")
        case Some(ba) =>
          ba
      }
      
      // If the first atom is a symbol, try to interpret it as an operator.
      if (firsttext != null) {
        // Apply the naked symbol as an operator.
        val op = context.operatorLibrary(firsttext)
        Some(Apply(op, second))
      } else {      
        // Build the apply.
        Some(Apply(first, second))
      }
    } else {
      // Just return the single atom.
      return Some(first)
    }
    
    // Check for a dot indicating application.
  }
  
  /**
   * Parse the next atom in the stream.
   * @param what  A short name for the atom being parsed, including the article,
   *              if this atom is part of a larger atom.  This is for error
   *              messages, and can be omitted.
   * @return  The next atom from the stream.
   */
  def parseInnerAtom(what: String = null): Option[BasicAtom] = {
    naked = null
    val action = what match {
      case null => ""
      case _ => "While parsing "+what+": "
    }
    
    // Consume whitespace.
    worker.consumeWhitespace()

    // What is the next character?
    val ch = worker.peek()
    ch match {
      case worker.EOF =>
        worker.consume()
        return None
        
      case '^' =>
        // This must be the root.  Check that and, if it is, okay.  Otherwise
        // this is an error.
        worker.consume()
        if (! worker.peekAndConsume("TYPE")) {
          worker.fail(action+"Found ^ and expected ^TYPE.  Did you misspell " +
              "^TYPE or forget to quote a symbol or string?")
        }
        return Some(TypeUniverse)
        
      case ')' =>
        // If we have an open parenthesis we are allowed to close it.
        if (parendepth > 0) {
          worker.consume()
          parendepth -= 1
          return None
        } else {
          worker.fail(action+"Found closing parenthesis, but there is no " +
              "matching open parenthesis.  Did you forget an open " +
              "parenthesis, or include an extra closing parenthesis?")
        }
        
      case '(' =>
        // This is a nested atom.  We need to parse the atom inside the
        // parenthesis and return it.  Then we need to consume the closing
        // parenthesis.
        worker.consume()
        parendepth += 1
        val nextAtom = parseAtom()
        if (! worker.peekAndConsume(")")) {
          worker.fail(action+"Did not find a closing parenthesis.  At most " +
              "one atom may be parenthesized.  Did you forget the closing " +
              "parenthesis?")
        }
        parendepth -= 1
        return nextAtom
        
      case '"' =>
        // We've either found a normal quoted string, or we have found a
        // here string.  To decide which it is, look ahead at the next two
        // characters to see what we get.  If we get two additional quotation
        // marks, then we have a here string.
        if (worker.peek("\"\"\"")) {
          // This is a here string, so process it as such.
          return Some(parseHereString())
        } else {
          // This is a normal string, so process it as such.
          return Some(parseQuotedString())
        }
        
      case '$' =>
        // We've found a variable or a metavaraible.
        return Some(parseVariable())
        
      case '`' =>
        // We've found a quoted symbol.
        return Some(parseSymbol())
        
      case '\\' =>
        // We've found a lambda.
        return Some(parseLambda())
        
      case '{' =>
        // We've found a special form.
        return Some(parseSpecialForm())
        
      case '%' =>
        // We've found a property specification.
        val prop = parseAlgebraicProperties()
            
        // A list might immediately follow.  Check for that and parse.
        return if (worker.peek() == '(') {
          Some(AtomSeq(prop, parseNakedList("an atom sequence")))
        } else {
          Some(prop)
        }

      case _ if Character.isJavaIdentifierStart(ch) =>
        // We've found the start of an unquoted symbol.  Note that we captured
        // the dollar sign above, so this should work correctly.
        return Some(parseSymbol())
        
      case _ if Character.isDigit(ch) || ch == '-' =>
        // We've found a digit.  This is a number, so we need to process it
        // as such.  It might be an integer, a float, or a bit string.
        return Some(parseNumber())
        
      case _ =>
        // This is not expected.
        worker.fail(action+"Expected to find the start of an atom, but found " +
            printify(ch) + " instead.")
        
    } // Decide what we have based on the next character.
    return None
  }
  
  def parseHereString() = {
    // Consume the opening triple quotation mark.
    if(! worker.peekAndConsume("\"\"\"")) {
      worker.fail("Expected to parse a here string at this position, but did " +
          "not find three opening double quotation marks as required.")
    }
    
    // Now consume everything up to the closing triple quotation mark.
    val content = new StringBuffer()
    while (! worker.peekAndConsume("\"\"\"")) {
      val ch = worker.consume()
      if (ch < 0) {
        worker.fail("Found end of file while looking for closing triple " +
            "quotation mark for a here string.  Did you forget to close the " +
            "here string properly?")
      }
      content.append(ch.toChar)
    } // Consume everything.
    
    // If we get here we have found and read the closing quotation marks.  Now
    // we have to consider whether there is a type, so check for that.
    val theType = parseType(STRING)
    
    // Generate a string literal and return it.
    StringLiteral(theType, content.toString())
  }
  
  def parseQuotedString() = {
    val text = parseDelimitedString('"', "quoted string")

    // If we get here we have found and read the closing quotation mark.  Now
    // we have to consider whether there is a type, so check for that.
    val theType = parseType(STRING)
    
    // Generate a string literal and return it.
    StringLiteral(theType, text)
  }
  
  def parseVariable() = {
    var ismeta = false
    if (! worker.peekAndConsume("$")) {
      worker.fail("Expected to parse a variable or metavariable at this " +
          "location, but did not find a dollar sign to start the variable.")
    }
    if (worker.peekAndConsume("$")) {
      // This is a metavariable.
      ismeta = true
    }
    
    // Get the variable name.  If the variable name is enclosed in quotation
    // marks, it is a "by name" variable.
    var text: String = ""
    var byname = false
    if (worker.peek() == '"') {
      if (ismeta) {
        worker.fail("Found a metavariable name enclosed in quotation marks, " +
            "which typically indicates that you want a by-name variable.  " +
            "However, by-name metavariables are not supported.")
      }
      text = parseDelimitedString('"', "by-name variable name")
      byname = true
    } else {
      text = parseNakedSymbol("variable name")
    }
    worker.consumeWhitespace()
    
    // Variables can have guards.  A guard must be given in curly brackets
    // immediately after the name.  Go parse the guard if it is there.
    var guard: BasicAtom = true
    if (worker.peekAndConsume("{")) {
      // Found a guard.
      parseAtom("a guard") match {
        case None =>
          worker.fail("Found { indicating a guard, but did not find a guard." +
              "  Make sure you have enclosed the gaurd properly.  If you " +
              "did not intend a guard here, then enclose the variable in " +
              "parentheses to avoid ambiguity.")
        case Some(grd) =>
          guard = grd
      }
      worker.consumeWhitespace()
      val ch = worker.consume()
      if (ch != '}') {
        worker.fail("Expected to find } at the end of the variable guard, " +
            "but instead found " + printify(ch) +
            ".  Make sure you have enclosed the guard properly.  If you " +
            "did not intend a guard here, then enclose the variable in " +
            "parentheses to avoid ambiguity.")
      }
    }
    
    // The variable may have a type specified after the guard.  Handle that
    // now.  If no type is given then ANY is assumed.
    val theType = parseType(ANY)
    
    // Variables may have zero or more tags.  Look for any tags and parse them.
    var tags = Set[String]()
    worker.consumeWhitespace()
    while (worker.peek() == '@') {
      // Found a tag.  The tag name is a symbol.
      worker.consume()
      val tagname = parseNakedSymbol("variable tag name")
      tags += tagname
      worker.consumeWhitespace()
    } // Parse all tags.
      
    // Generate a variable and return it.
    if (ismeta) {
      MetaVariable(theType, text, guard, tags, byname)
    } else {
      Variable(theType, text, guard, tags, byname)
    }
  }
  
  def parseLambda() = {
    // Consume the lambda marker.
    if (! worker.peekAndConsume("\\")) {
      worker.fail("Expected to parse a lambda at this location, but did not " +
          "find the starting \\.")
    }
    worker.consumeWhitespace()
    
    // Get the parameter, which is a variable.
    val parameter = parseVariable()
    
    // Now get the lambda dot.
    worker.consumeWhitespace()
    var ch = worker.consume()
    if (ch != '.') {
      worker.fail("Expected to find the lambda dot joining the parameter and " +
          "body, but instead found " + ch + ".")
    }
    
    // Now get the body.
    val body = parseInnerAtom("a lambda body") match {
      case None =>
        worker.fail("The body for a lambda should be right after the dot, " +
            "but it is missing.")
      case Some(ba) =>
        ba
    }
    
    // Make the lambda.
    Lambda(parameter, body)
  }
  
  def parseSymbol(): BasicAtom = {
    val text = parseNakedSymbol("symbol")
    
    // If there is no type specified, then this might be a root type name,
    // the shorthand for ANY, or the special values true or false.
    worker.consumeWhitespace()
    if (worker.peek() != ':') {
      naked = text
      
      if (worker.peek() == '(') {
        val loc = worker.loc
        val op = context.operatorLibrary(text)
        val args = parseNakedList("an argument list")
        val seq = AtomSeq(AlgProp(loc), args)
        Apply(op, seq)
      } else {
        text match {
          case "true" => true
          case "false" => false
          case _ =>
            val lookup = (if (text == "_") "ANY" else text)
            NamedRootType.get(lookup) match {
              case Some(nrt) => nrt
              case _ => SymbolLiteral(SYMBOL, Symbol(text))
            }
        }
      }
    } else {
      naked = null
      
      // Parse the type and build the atom.
      val theType = parseType(SYMBOL)
      if (theType == OPREF) {
        context.operatorLibrary(text)
      } else {
        SymbolLiteral(theType, Symbol(text))
      }
    }
  }
  
  def parseSpecialForm() = {
    // Save the location.
    val start = worker.loc
    
    // Consume the opening brace.
    if (! worker.peekAndConsume("{")) {
      worker.fail("Expected to parse a special form at this location, but " +
          "did not find the opening brace.")
    }
    
    // Check for a colon.
    if (worker.peekAndConsume(":")) {
      // This is the general form for the special form.
      val tag = parseAtom("a special form tag") match {
        case None =>
          worker.fail("Did not find any atoms in the special form.  There " +
              "must be exactly two atoms when the {: .. :} form is used.")
        case Some(ba) =>
          ba
      }
      val content = parseAtom("a special form content") match {
        case None =>
          worker.fail("Did not find a second atoms in the special form.  " +
              "There must be exactly two atoms when the {: .. :} form is used.")
        case Some(ba) =>
          ba
      }
      worker.consumeWhitespace()
      if (! worker.peekAndConsume(":}")) {
        worker.fail("Expected to find the closing :} delimiter, but did not." +
            "  There must be exactly two atoms when the {: .. :} form is " +
            "used.  Make sure you have the closing delimiter, and be sure " +
            "you did not inadvertently include an extra atom.")
      }
      SpecialForm(start, tag, content)
    } else {
      var map = HashMap[String, BasicAtom]()
      var tag: BasicAtom = false
      
      // This is the long, sugary form.  Check to see if we have a prototype.
      if (worker.peekAndConsume("!")) {
        // This is a function prototype.  Parse it and save the parts.
        tag = SymbolLiteral(SYMBOL, 'operator)
        worker.consumeWhitespace()
        
        // Parse the function name.
        val fname = parseNakedSymbol("a function name")
        map += "name" -> SymbolLiteral(SYMBOL, Symbol(fname))
        worker.consumeWhitespace()
        
        // The parameter list must be next.  Consume it.
        var ap = AlgProp(worker.loc)
        val params = parseNakedList("a parameter list")
        map += "params" -> AtomSeq(ap, params)
        worker.consumeWhitespace()
        
        // The fully-applied type may be next.  Consume it if present.
        val fatype = parseType(ANY)
        map += "type" -> fatype
        worker.consumeWhitespace()
        
        // The keyword "is" may be present to signal that an algebraic property
        // specification is on the way.  Check for that.
        if (worker.peekAndConsume("is")) {
          // Found the keyword.  Process it.
          worker.consumeWhitespace()
          ap = parseAlgebraicProperties()
          worker.consumeWhitespace()
        }
      } else {
        // The tag must be the first thing inside the braces.
        worker.consumeWhitespace()
        tag = SymbolLiteral(SYMBOL, Symbol(parseNakedSymbol("the tag")))
        worker.consumeWhitespace()
        
        // After the tag there may be zero or more atoms.  These will not start
        // with a hash mark, and get collected into a list and bound to the
        // empty symbol in the map.
        val loc = worker.loc
        var seq = IndexedSeq[BasicAtom]()
        while (worker.peek() != '#' && worker.peek() != '}') {
          seq :+= (parseAtom("an atom in a sequence") match {
            case None =>
              worker.fail("Expected an atom but did not find one.")
            case Some(ba) =>
              ba
          })
          worker.consumeWhitespace()
        } // Parse all simple atoms.
        if (seq.length != 0) {
          map += "" -> AtomSeq(AlgProp(loc), seq)
        }
      }
      
      // Now we can consume the rest of the content, until we encounter the
      // closing brace.
      while (! worker.peekAndConsume("}")) {
        // The next thing must be a tag name, starting with a hashmark.
        if (! worker.peekAndConsume("#")) {
          worker.fail("Expected a bind pair starting with a hash mark (#) " +
              "or the end of the special form, but instead found: " +
              printify(worker.peek()) + ".")
        }
        
        // Parse the symbol.
        val name = parseNakedSymbol("a bind name")
        worker.consumeWhitespace()
        
        // The next thing is either an equal sign followed by an atom, or a
        // comma-separated list of atoms that will become a list.  Either way,
        // parse it.
        val value = if (worker.peekAndConsume("=")) {
          parseAtom() match {
            case None =>
              worker.fail("Expected an atom as the value of the bind but "+
                  "did not find one.")
            case Some(ba) =>
              ba
          }
        } else {
          val loc = worker.loc
          var seq = IndexedSeq[BasicAtom]()
          seq :+= (parseAtom("an atom in a sequence") match {
            case None =>
              worker.fail("Expected an atom sequence as the value of "+
                  "the bind but did not find one.")
            case Some(ba) =>
              ba
          })
          worker.consumeWhitespace()
          while (worker.peekAndConsume(",")) {
            seq :+= (parseAtom("an atom in a sequence") match {
              case None =>
                worker.fail("Expected an atom following a comma in a bind "+
                    "value but did not find one.  Did you incude an extra "+
                    "comma or omit an atom?")
              case Some(ba) =>
                ba
            })
            worker.consumeWhitespace()
          } // Get all the atoms in the sequence.
          AtomSeq(AlgProp(loc), seq)
        }
        
        // Add this pair to the map.
        map += name -> value
      } // Read the rest of the special form.
      
      // Construct the special form.
      SpecialForm(start, tag, Bindings(map))
    }
  }
  
  def parseNumber(): BasicAtom = {
    // A number can be an integer, a float, or a bit string.
    
    // Now we must find an integer at the next position.
    val (radix, integer) = signedInteger()
    
    // Is this a float?
    var isFloat = false
    
    // Is this a bit string?
    var isBitString = false
    
    // If the next character is a period, then this is a float.
    val fraction = if (worker.peekAndConsume(".")) {
      // Get the fractional part of the number.
      isFloat = true
      radix match {
        case 16 => hexInteger(integer._1)
        case 10 => decInteger(integer._1)
        case 8  => octInteger(integer._1)
        case 2  => binInteger(integer._1)
      }
    } else {
      (integer._1, 0)
    }
      
    // Look for an exponent.  If we find one, this is a float.  Because the
    // parser is greedy, if we encountered an e or E as part of a hexadecimal
    // integer, we already grabbed it up.  Thus it is okay to check for those
    // now.
    val exponent = if (
        worker.peekAndConsume("e") ||
        worker.peekAndConsume("E") ||
        worker.peekAndConsume("p") ||
        worker.peekAndConsume("P")) {
      isFloat = true
      signedInteger()._2
    } else {
      (BigInt(0), 1)
    }
    
    // Look for a length indicator.
    val length = if (
        ! isFloat &&
        (worker.peekAndConsume("l") ||
         worker.peekAndConsume("L"))) {
      isBitString = true
      nnInteger()._2._1.toInt
    } else {
      0
    }
    
    // Look for a type.
    worker.consumeWhitespace()
    val theType = parseType(
        if (isFloat) FLOAT
        else if (isBitString) BITSTRING
        else INTEGER)
    
    // If we have a length, then create a bit string.
    if (isBitString) {
      new BitStringLiteral(theType, integer._1, length)
    } else if (isFloat) {
      // Floats need to be normalized so that the fractional part is merged
      // into the significand.  Here's how that works.
      //
      // Suppose we have 1.5e2.  This is 1.5 times 10 squared, or 1.5 times
      // 100.  That is, 150.  We want to write this as 15e1, so there is an
      // integer significand and an integer exponent.  Note what happened.
      // Since there was one digit on the right of the decimal, we decreased
      // the exponent by one and moved the digit to the left.
      //
      // Because we passed the prior integer value to the part that parsed
      // the fraction, we have already "merged" the integer and fractional
      // part.  All we need to do now is fix up the exponent by subtracting
      // the number of digits in the fraction.  We do that now.
      val newexp = exponent._1.toInt - fraction._2
      new FloatLiteral(theType, fraction._1, newexp, radix)
    } else {
      new IntegerLiteral(theType, integer._1)
    }
  }
  
  def parseAlgebraicProperties() = {
    // Consume the percent sign.
    if (! worker.peekAndConsume("%")) {
      worker.fail("Expected to parse an algebraic property specification at " +
          "this location, but did not find the initial percent sign (%).")
    }
    
    // Now process until we run out of the usual suspects.
    val loc = worker.loc
    var assoc: Option[BasicAtom] = None
    var commu: Option[BasicAtom] = None
    var idemp: Option[BasicAtom] = None
    var absor: Option[BasicAtom] = None
    var ident: Option[BasicAtom] = None
    while (worker.peek() match {
      
      case '!' =>
        worker.consume()
        worker.consume() match {
          case ch if ch == 'A' || ch == 'a' => assoc = Some(false)
          case ch if ch == 'C' || ch == 'c' => commu = Some(false)
          case ch if ch == 'I' || ch == 'i' => idemp = Some(false)
          case ch if ch == 'B' || ch == 'b' =>
            worker.fail("The absorber property cannot be negated.")
          case ch if ch == 'D' || ch == 'd' =>
            worker.fail("The identity property cannot be negated.")
          case it =>
            worker.fail("Found a negation of a property, but did not "+
                "find a known property letter after it.")
        }
        if (worker.peek() == '[') {
          worker.fail("Property negations do not take arguments, but "+
              "found an opening square bracket after the negated property.")
        }
        true
      
      case ch if ch == 'A' || ch == 'a' =>
        worker.consume()
        if (worker.peekAndConsume("[")) {
          assoc = parseAtom("an associativity specification") match {
            case None =>
              worker.fail("Expected to parse an associativity specification "+
                  "but did not find one.")
            case it =>
              it
          }
          worker.consumeWhitespace()
          if (! worker.peekAndConsume("]")) {
            worker.fail("Missing closing square bracket for specification.")
          }
        } else {
          assoc = Some(true)
        }
        true
        
      case ch if ch == 'C' || ch == 'c' =>
        worker.consume()
        if (worker.peekAndConsume("[")) {
          commu = parseAtom("a commutativity specification") match {
            case None =>
              worker.fail("Expected to parse a commutativity specification "+
                  "but did not find one.")
            case it =>
              it
          }
          worker.consumeWhitespace()
          if (! worker.peekAndConsume("]")) {
            worker.fail("Missing closing square bracket for specification.")
          }
        } else {
          commu = Some(true)
        }
        true
        
      case ch if ch == 'I' || ch == 'i' =>
        worker.consume()
        if (worker.peekAndConsume("[")) {
          idemp = parseAtom("an idempotency specification") match {
            case None =>
              worker.fail("Expected to parse an idempotency specification "+
                  "but did not find one.")
            case it =>
              it
          }
          worker.consumeWhitespace()
          if (! worker.peekAndConsume("]")) {
            worker.fail("Missing closing square bracket for specification.")
          }
        } else {
          idemp = Some(true)
        }
        true
        
      case ch if ch == 'B' || ch == 'b' =>
        worker.consume()
        if (worker.peekAndConsume("[")) {
          absor = parseAtom("an absorber specification") match {
            case None =>
              worker.fail("Expected to parse an absorber specification "+
                  "but did not find one.")
            case it =>
              it
          }
          worker.consumeWhitespace()
          if (! worker.peekAndConsume("]")) {
            worker.fail("Missing closing square bracket for specification.")
          }
        } else {
          worker.fail("An absorber requires that an atom be specified, "+
              "but the specification was not found.  It must immediately "+
              "follow the B in square brackets.")
        }
        true
        
      case ch if ch == 'D' || ch == 'd' =>
        worker.consume()
        if (worker.peekAndConsume("[")) {
          ident = parseAtom("an identity specification") match {
            case None =>
              worker.fail("Expected to parse an identity specification "+
                  "but did not find one.")
            case it =>
              it
          }
          worker.consumeWhitespace()
          if (! worker.peekAndConsume("]")) {
            worker.fail("Missing closing square bracket for specification.")
          }
        } else {
          worker.fail("An identity requires that an atom be specified, "+
              "but the specification was not found.  It must immediately "+
              "follow the D in square brackets.")
        }
        true
        
      case _ => false
    }) {}
    
    // Now build the algebraic property specification.
    AlgProp(loc, assoc, commu, idemp, absor, ident)
  }
  
  //======================================================================
  // Number helper methods.
  //======================================================================
  
  private def signedInteger() = {
    // Look for a negative sign.
    var isNegative = worker.peekAndConsume("-")
    
    // Now we must find an integer at the next position.
    val (radix, (integer, count)) = nnInteger()
    (radix, (if (isNegative) -integer else integer, count))
  }
  
  private def nnInteger() = {
    if (worker.peekAndConsume("0")) {
      // Found a zero.  It might just be a zero, or it might be the start of
      // a longer number.  See if we can parse something from here.
      worker.peek() match {
        case 'x' =>
          worker.consume()
          (16, hexInteger())
        case 'o' =>
          worker.consume()
          (8, octInteger())
        case 'b' =>
          worker.consume()
          (2, binInteger())
        case 'X' =>
          worker.consume()
          (16, hexInteger())
        case 'O' =>
          worker.consume()
          (8, octInteger())
        case 'B' =>
          worker.consume()
          (2, binInteger())
        case d if Character.isDigit(d) =>
          // If the next character is a digit, then treat this as a decimal
          // integer.
          (10, decInteger())
        case _ =>
          // This is just a zero.
          (10, (BigInt(0), 1))
      }
    } else if (Character.isDigit(worker.peek())) {
      (10, decInteger())
    } else {
      worker.fail(
          "Expected to find a number at the current location, but did not.")
    }
  }
  
  private def hexInteger(start: BigInt = 0) = {
    var count = 0
    var digit = hexDigit()
    var accum = start
    while (digit >= 0) {
      count += 1
      accum = accum * 16 + digit
      digit = hexDigit()
    } // Read all hex digits.
    
    // If we didn't get at least one digit, then this is not a proper hex
    // integer.
    if (count <= 0) {
      worker.fail("Expected to find a hexadecimal number, but did not find " +
          "any hexadecimal digits (0-9, a-f).  The current character is: " +
          printify(worker.peek()))
    }
    (accum, count)
  }
  
  private def decInteger(start: BigInt = 0) = {
    var count = 0
    var digit = decDigit()
    var accum = start
    while (digit >= 0) {
      count += 1
      accum = accum * 10 + digit
      digit = decDigit()
    } // Read all decimal digits.
    
    // If we didn't get at least one digit, then this is not a proper decimal
    // integer.
    if (count <= 0) {
      worker.fail("Expected to find a decimal number, but did not find " +
          "any decimal digits (0-9).  The current character is: " +
          printify(worker.peek()))
    }
    (accum, count)
  }
  
  private def octInteger(start: BigInt = 0) = {
    var count = 0
    var digit = octDigit()
    var accum = start
    while (digit >= 0) {
      count += 1
      accum = accum * 8 + digit
      digit = octDigit()
    } // Read all octal digits.
    
    // If we didn't get at least one digit, then this is not a proper octal
    // integer.
    if (count <= 0) {
      worker.fail("Expected to find a octal number, but did not find " +
          "any octal digits (0-7).  The current character is: " +
          printify(worker.peek()))
    }
    (accum, count)
  }
  
  private def binInteger(start: BigInt = 0) = {
    var count = 0
    var digit = binDigit()
    var accum = start
    while (digit >= 0) {
      count += 1
      accum = accum * 2 + digit
      digit = binDigit()
    } // Read all binary digits.
    
    // If we didn't get at least one digit, then this is not a proper binary
    // integer.
    if (count <= 0) {
      worker.fail("Expected to find a binary number, but did not find " +
          "any binary digits (0,1).  The current character is: " +
          printify(worker.peek()))
    }
    (accum, count)
  }

  private def hexDigit(): BigInt = {
    val ch = worker.peek()
    if ('0' <= ch && ch <= '9') {
      worker.consume()
      ch - '0'
    } else if ('a' <= ch && ch <= 'f') {
      worker.consume()
      ch - 'a' + 10
    } else if ('A' <= ch && ch <= 'F') {
      worker.consume()
      ch - 'A' + 10
    } else -1
  }
  
  private def decDigit(): BigInt = {
    val ch = worker.peek()
    if ('0' <= ch && ch <= '9') {
      worker.consume()
      ch - '0'
    } else -1
  }
  
  private def octDigit(): BigInt = {
    val ch = worker.peek()
    if ('0' <= ch && ch <= '7') {
      worker.consume()
      ch - '0'
    } else -1
  }
  
  private def binDigit(): BigInt = {
    val ch = worker.peek()
    if (ch == '0') {
      worker.consume()
      0
    } else if (ch == '1') {
      worker.consume()
      1
    } else -1
  }
  
  //======================================================================
  // Support methods.
  //======================================================================
  
  private def parseNakedList(kind: String) = {
    // Consume the opening parenthesis.
    if (! worker.peekAndConsume("(")) {
      worker.fail("Expected to parse a list at this location, but did not " +
          "find the opening parenthesis.")
    }
    
    // Now read a comma-separated list of atoms.
    var seq = IndexedSeq[BasicAtom]()
    worker.consumeWhitespace()
    while (! worker.peekAndConsume(")")) {
      seq :+= (parseAtom("an atom in a list") match {
        case None =>
          worker.fail("Expected an atom but did not find one while parsing " +
              kind + ".")
        case Some(ba) =>
          ba
      })
      worker.consumeWhitespace()
      if (worker.peekAndConsume(",")) {
        worker.consumeWhitespace()
        // Next time around we parse the atom.
      } else if (worker.peek() == ')') {
        // No problem.  We stop next time around.
      } else {
        worker.fail("Unexpectedly encountered " + worker.peek() +
            " during parsing of a " +
            kind + ".  This should be a parenthesized, comma separated " +
            "list of atoms.")
      }
    } // Read until the closing parenthesis is found.
    
    // Done.
    seq
  }
  
  /**
   * Parse a delimited string, processing escapes.  The next thing in the
   * stream must be the opening delimiter.
   * @param delimiter   The character that delimits the string.
   * @param kind        A name for what is being parsed, for error messages.
   * @return  The parsed text as a string.
   */
  private def parseDelimitedString(delimiter: Char, kind: String) = {
    val delim = ""+delimiter
    
    // Consume the opening delimiter.
    if (! worker.peekAndConsume(delim)) {
      worker.fail("Expected to parse " + kind +
          " at this location, but did not find the opening delimiter: " +
          delim + ".")
    }
    val content = new StringBuffer()
    
    // Read up to the closing quotation mark, watching for character escapes.
    while (! worker.peekAndConsume(delim)) {
      val ch = worker.consume()
      if (ch == '\\') {
        // This is an escape.  Interpret it.
        val nch = worker.consume()
        if (worker.isAtEof) {
          worker.fail("Parsing " + kind + " was unexpectedly terminated by " +
              "the end of file.")
        }
        nch match {
          case 'n' => content.append('\n')
          case 'r' => content.append('\r')
          case 't' => content.append('\t')
          case '\\' => content.append('\\')
          case _ => content.append(nch.toChar)
        }
      } else if (ch == worker.EOF) {
        worker.fail("Parsing " + kind + " was unexpectedly terminated by " +
            "the end of file.")
      } else {
        content.append(ch.toChar)
      }
    } // Read to closing delimiter.
    
    // Done.
    content.toString()
  }
  
  /**
   * Parse a naked symbol.  This is a symbol without any type information.
   * @param kind  What this naked symbol represents, for error messages.
   * @return  The parsed symbol name as a string, with article.
   */
  private def parseNakedSymbol(kind: String) = {
    // Read the first character.  This will decide whether this is an unquoted
    // or quoted symbol.
    var ch = worker.peek()
    var text: String = ""
    if (ch == '`') {
      // Parse a quoted symbol.
      text = parseDelimitedString('`', "a quoted symbol")
    } else if (Character.isAlphabetic(ch) || ch == '_') {
      // Parse an unquoted symbol.
      var content = new StringBuilder()
      var ch = worker.consume()
      content.append(ch.toChar)
      while (Character.isLetterOrDigit(worker.peek()) || worker.peek() == '_') {
        content.append(worker.consume().toChar)
      } // Read the identifier up to the first non-identifier character.
      text = content.toString
    } else {
      // This is not a symbol.
      worker.fail("Expected " + kind + ", but encountered " + printify(ch) +
          " instead.  Symbols must either be quoted with backticks (`) " +
          "or they must start with a letter or underscore.")
    }
    text
  }

  /**
   * Parse a type declaration, if present.  This checks for the next thing
   * in the stream to be a colon and, if it is, parses the type.
   * @param defaultAtom   The type to return if no type is found.
   * @return  The type parsed or the default type.
   */
  private def parseType(defaultAtom: BasicAtom): BasicAtom = {
    // Consume any whitespace.
    worker.consumeWhitespace()
    
    // Look for a colon.  We have to watch out for the closing of a generic
    // special form.  If we see that, we assume that is correct since it
    // cannot be a type!
    if (worker.peek() != ':') return defaultAtom
    if (worker.peek(":}")) return defaultAtom
    worker.consume()
    
    // Found a colon.  Parse an atom, which will be the type.
    parseInnerAtom("a type") match {
      case None =>
        worker.fail("Found a colon indicating type information, but did " +
            "not find a following atom to be the type.  Did you forget to " +
            "include the type?")
      case Some(ba) =>
        return ba
    }
  }
  
  /**
   * Convert a character into something printable in an error message.
   * @param ch  The character.
   * @return  The printable.
   */
  private def printify(ch: Int) = {
    if (Character.isWhitespace(ch)) {
      "whitespace (U+%04x)".format(ch)
    } else if (Character.isISOControl(ch)) {
      "control code U+%04x".format(ch)
    } else if (ch == worker.EOF) {
      "end of file"
    } else {
      ""+ch.toChar+" (U+%04x)".format(ch)
    }
  }
}
