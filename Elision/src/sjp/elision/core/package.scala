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
 * TODO: Need lambdas.
 * TODO: Need records.
 * TODO: Do we want to capture NaN, -NaN, Infty, -Infty?
 * TODO: Need parser.
 * TODO: Complete Rule implementation.
 * TODO: Ruleset change actions.
 */
import scala.collection.immutable.HashMap
package object core {
  // Bindings are used to store variable / value maps used during matching, and
  // as the result of a successful match.
  type Bindings = HashMap[String, BasicAtom]

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