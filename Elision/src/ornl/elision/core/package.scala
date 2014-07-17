/*======================================================================
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 * The Elision Term Rewriter
 * 
 * Copyright (c) 2012 by UT-Battelle, LLC.
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
 * Collection of administrative costs for redistribution of the source code or
 * binary form is allowed. However, collection of a royalty or other fee in excess
 * of good faith amount for cost recovery for such redistribution is prohibited.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER, THE DOE, OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
======================================================================
* */
package ornl.elision

import scala.collection.immutable.HashMap
import scala.collection.mutable.HashSet
import org.parboiled.errors.ParsingException
import ornl.elision.util.PrintConsole
import ornl.elision.util.PropertyManager
import ornl.elision.util.Debugger
import ornl.elision.util.Loc
import ornl.elision.context.Context
import ornl.elision.context.Executor

/**
 * The core classes and definitions that make up the Elision runtime.
 * 
 * The purpose of this package is to define all basic atoms.
 * 
 * == Design Goals ==
 *  - Avoid global data and singletons.
 *  - Simple API.
 */
package object core {
  import scala.collection.immutable.Map
  
  /** Provide a fancy name for the type universe. */
  val `^TYPE` = TypeUniverse
  
  /**
   * Maintain an executor to use.  The provided default executor always
   * throws an exception when invoked from a native handler, so you must
   * replace this with something rational as soon as reasonable.
   */
  implicit var knownExecutor: Executor = new Executor {
    val console = PrintConsole 
    val context = new Context()
    val settings = Map[String,String]()
    override def parse(name: String, text: String): Dialect.Result =
      Dialect.Failure(
        Loc.internal,
        "This default executor cannot parse text; override this with a full " +
        "executor implementation to properly support parsing from within " +
        "native operators.")
  }

  /**
   * Turn a string into a properly-escaped symbol.  Symbols must start with a
   * letter or underscore and consist of only letters, numbers, and the
   * underscore.  If this is not the case, the symbol text must be enclosed in
   * backticks, and any contained backticks and quotation marks must be escaped.
   * {{{
   * Fred   Johnny12   Cat_Dog   _Dog
   * ``     ` Jason`   `65$`
   * }}}
   * 
   * @param str	The symbol text.
   * @return	The symbol with special characters escaped.
   */
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
          ch match {
            case '`' => buf ++= """\`"""
		        case '\n' => buf ++= """\n"""
		        case '\t' => buf ++= """\t"""
		        case '\r' => buf ++= """\r"""
		        case '\\' => buf ++= """\\"""
		        case _ => buf ++= ch.toString
          }
        } else {
          buf ++= ch.toString
        }
    }
    if (bad) "`" + buf.toString + "`" else buf.toString
  }

  /**
   * Turn a string into a properly-escaped double-quoted string.  The following
   * transformations are performed.
   * {{{
   * double quotation mark   -> \"
   * newline                 -> \n
   * tab                     -> \t
   * carriage return         -> \r
   * backslash               -> \\
   * }}}
   * The resulting string is enclosed in double quotation marks.
   * 
   * @param str	The string.
   * @return	The string with special character escaped.
   */
  def toEString(str: String) = ornl.elision.util.toQuotedString(str)

  //======================================================================
  // WARNING: Implicit modification of containers!
  //======================================================================
    
  /**
   * Magically add a `mkParseString`, roughly equivalent to `mkString`, to
   * every sequence of objects that extend [[ornl.elision.core.BasicAtom]].
   * Try that with Java!
   * 
   * @param seq		The sequence to get the new method.
   */
  implicit def giveMkParseString[A <: BasicAtom](seq: Seq[A]): {
    def mkParseString(pre: String, mid: String, post: String): String
  } = new {
    /**
     * Make a string from a sequence.
     * 
     * @param pre		The prefix text, if any.
     * @param mid		The text to insert between each item.
     * @param post	The suffix text, if any.
     * @return	The new string.
     */
    def mkParseString(pre: String, mid: String, post: String) =
      seq.map(_.toParseString).mkString(pre, mid, post)
  }
  
  import ornl.elision.util.OmitSeq
  
  /**
   * Invoking the `omit` method on an indexed sequence automagically transforms
   * it into an omit sequence with the original sequence as its backing store.
   * 
   * @param seq	The original sequence.
   */
  implicit def toOmitSeq[A](seq: IndexedSeq[A]): {
    def omit(index: Int): OmitSeq[A]
  } = new {
    /**
	   * Omit a single element from this list, returning a new list.  This is
	   * done "in place" so it should be fast.  As omits mount, lookup time
	   * can suffer, approaching linear time.
	   * 
	   * @param index	The zero-based index to omit.
	   * @return	The new list.
	   */
    def omit(index: Int) = OmitSeq(seq, index)
  }

  //======================================================================
  // WARNING: Here be IMPLICITS!
  //======================================================================
  
  /** Convert a Scala symbol to a variable. */
  implicit def symToVariable(symbol: Symbol) = Variable(ANY, symbol.name)
  
  /** Convert a variable to a Scala symbol. */
  implicit def variableToSym(variable: Variable) = Symbol(variable.name)
  
  /** Convert an integer to an integer literal. */
  implicit def intToLiteral(value: Int) = Literal(value)

  /** Convert an integer to an integer literal. */
  implicit def intToLiteral(value: BigInt) = Literal(value)
  
  /** Convert an integer literal to an integer. */
  implicit def literalToInt(value: IntegerLiteral) = value.value
  
  /** Convert a string to a string literal. */
  implicit def strToLiteral(string: String) = Literal(string)
  
  /** Convert a string literal to a string. */
  implicit def literalToStr(value: StringLiteral) = value.value
  
  /** Convert a Scala Boolean to a Boolean literal. */
  implicit def boolToLiteral(bool: Boolean) = Literal(bool)
  
  /** Convert a Boolean literal to a Scala Boolean. */
  implicit def literalToBool(value: BooleanLiteral) = value.value
  
  /**
   * Where needed, convert a bindings object into a bindings atom (wrap).
   * 
   * @param binds	The bindings.
   * @return	The new bindings atom.
   */
  implicit def wrapBindingsAtom(binds: Bindings) = new BindingsAtom(binds)
  
  /**
   * Where needed, convert a bindings atom to a simple bindings object (unwrap).
   * 
   * @param atom	The bindings atom.
   * @return	The bindings.
   */
  implicit def unwrapBindingsAtom(atom: BindingsAtom) = atom.mybinds
}
