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
package sjp.elision

/**
 * Basic definitions used in this package.
 * 
 * = Punch List =
 * This is the current punch list for Elision.  This list gets picked up by
 * Eclipse.
 * 
 * TOOO: Rule application.
 * TODO: Make Bindings a BasicAtom.
 * TODO: Revisit every atom class wrt case class or not.
 * TODO: Extract sequence rewriting. (?)
 * TODO: Implement pairs and smaps.
 * 
 * TODO: BasicAtom cleanup / doc.
 * TODO: Bindings cleanup / doc.
 * TODO: Literal cleanup / doc.
 * TODO: Variable cleanup / doc.
 * TODO: Lambda cleanup / doc.
 * TODO: Apply cleanup / doc.
 * TODO: Context cleanup / doc.
 * TODO: OperatorDefinition cleanup / doc.
 * TODO: TypeUniverse cleanup / doc.
 * TODO: MatchIterator cleanup / doc.
 * TODO: Outcome cleanup / doc.
 * TODO: Operator cleanup / doc.
 * TODO: OperatorDefinition cleanup / doc.
 * TODO: AtomList cleanup / doc.
 * TODO: SequenceMatcher cleanup / doc.
 * TODO: RewriteRule cleanup / doc.
 * TODO: AtomParser cleanup / doc.
 * 
 * TODO: Complete Operator definition implementation (matching). DEFER.
 * TODO: Ruleset change actions. DEFER.
 * TODO: Infix. DEFER.
 * TODO: AC matching. DEFER.
 * TODO: C matching. DEFER.
 * TODO: A matching. DEFER.
 * 
 * = Design Goals =
 *  * Every atom that can be programmatically created can be written using
 *    the toParseString, and the result can be parsed by AtomParser.
 *  * Every atom that can be programmatically created has a toString method
 *    that generates valid Scala code to reproduce the atom.
 *  * Avoid global data and singletons.
 *  * Simple API.
 */
import scala.collection.immutable.HashMap
import org.parboiled.errors.ParsingException
import sjp.elision.parse.AtomParser
package object core {
  // Bindings are used to store variable / value maps used during matching, and
  // as the result of a successful match.
  type Bindings = HashMap[String, BasicAtom]
  
  /**
   * Magically add a mkParseString, roughly equivalent to mkString, to every
   * sequence of objects that extend BasicAtom.  Try that with Java!
   * @param seq		The sequence to get the new method.
   */
  implicit def giveMkParseString[A <: BasicAtom](seq: Seq[A]): {
    def mkParseString(pre: String, mid: String, post: String): String
  } = new {
    /**
     * Make a string from a sequence.
     * @param pre		The prefix text, if any.
     * @param mid		The text to insert between each item.
     * @param post	The suffix text, if any.
     * @return	The new string.
     */
    def mkParseString(pre: String, mid: String, post: String) =
      seq.map(_.toParseString).mkString(pre, mid, post)
  }
  
  /**
   * Attempt to parse the given string and return an atom.
   * @param str			The string to parse.
   * @param context	The context.
   * @param trace		Whether to trace the parse.
   * @return	The result of parsing, which may be None.
   */
  def parse(str: String, context: Context, trace: Boolean = false) = {
    val ap = new AtomParser(context, trace)
    try {
      ap.tryParse(str) match {
        case None => None
        case Some(atom) => { println(atom.toParseString) ; Some(atom) }
      }
    } catch {
      case th: ParsingException => println(th.getMessage)
    }
  }

  // Turn a string into a properly-escaped symbol.  Symbols must start with a
  // letter or underscore and consist of only letters, numbers, and the
  // underscore.  If this is not the case, the symbol text must be enclosed in
  // backticks, and any contained backticks and quotation marks must be escaped.
  // Fred				Johnny12				Cat_Dog				_Dog
  // ``					` Jason`				`65$`
  def toESymbol(str: String): String = {
    // The empty string is a special case.
    if (str.isEmpty) return "``"
    // True and false are also special cases.
    if (str == "true" || str == "false") return "`" + str + "`"
    // Build up the return value.
    var buf = new scala.collection.mutable.StringBuilder
    // Look for badness.  The first character is special.
    var bad = false
    val sh = str.head
    if (sh != '_' && !sh.isLetter) {
      bad = true
      if (sh == '`') buf ++= "\\"
    }
    buf ++= sh.toString
    str.tail.map {
      ch =>
        if (ch != '_' && !ch.isLetterOrDigit) {
          bad = true
          if (ch == '`') buf ++= "\\"
        }
        buf ++= ch.toString
    }
    if (bad) "`" + buf.toString + "`" else buf.toString
  }

  // Turn a string into a properly-escaped double-quoted string.  The only
  // illegal characters in a double-quoted string are the double quotation
  // mark and the newline.  These must be escaped.
  def toEString(str: String) = {
    var buf = new scala.collection.mutable.StringBuilder
    buf ++= "\""
    for (ch <- str) {
      if (ch == '"' || ch == '\n') buf ++= "\\"
      buf ++= ch.toString
    }
    buf ++= "\""
    buf.toString
  }
  
  /**
   * Issue a warning.  This should be wired to whatever error reporting
   * mechanism you want to use.
   * @param text	The text of the warning.
   */
  def warn(text: String) {
    println("WARNING: " + text)
  }
}
