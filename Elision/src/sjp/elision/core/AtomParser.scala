/* Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 */
package sjp.elision.core
import scala.util.parsing.combinator.RegexParsers

/**
 * @author ysp
 *
 */
class AtomParser extends RegexParsers {
	val symbol: Parser[String] = """([a-zA-Z_][a-zA-Z0-9_]*)|(`[^`]*`)""".r
	val string: Parser[String] = """\"[^\"]*\"""".r
}