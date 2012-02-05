/* Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 */
package sjp.elision

/**
 * Basic definitions used in this package.
 * 
 * TODO: Complete Rule implementation (matching).
 * TODO: Complete Operator definition implementation (matching).
 * TODO: Ruleset change actions.
 * TODO: Infix.
 * TODO: AC matching.
 * TODO: C matching.
 * TODO: A matching.
 * TOOO: Rule application.
 * TODO: Ruleset management.
 * TODO: Make Bindings a BasicAtom.
 * 
 * = Design Goals =
 * 
 */
import scala.collection.immutable.HashMap
import org.parboiled.errors.ParsingException
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
   * @return	The result of parsing, which may be null.
   */
  def parse(str: String, context: Context, trace: Boolean = false) = {
    val ap = new AtomParser(context, trace)
    try {
      val node = ap.tryParse(str)
      println(node.toParseString)
      node
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
}