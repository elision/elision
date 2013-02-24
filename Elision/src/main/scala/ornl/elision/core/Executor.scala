/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
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
package ornl.elision.core
import ornl.elision.util.ElisionException
import scala.util.Properties
import ornl.elision.util.Console
import ornl.elision.util.{PropertyManager, Cache, CacheException}

/**
 * An executor is a class that can convert a string into a sequence of atoms.
 * This can be done by parsing the usual Elision representation of atoms, or
 * by some other means.
 */
trait Executor extends PropertyManager with Cache {
  
  /** A parse result. */
  abstract sealed class ParseResult
  
  /**
   * This property manager supports BigInts, Strings, and any BasicAtom.
   */
  val clazzes = Set(
      classOf[java.lang.Boolean],
      classOf[Boolean],
      classOf[java.lang.Integer],
      classOf[BigInt],
      classOf[String],
      classOf[BasicAtom])
  
  /**
   * The parse was successful.
   * 
   * @param nodes	The atoms parsed.
   */
  case class ParseSuccess(nodes: List[BasicAtom]) extends ParseResult
  
  /**
   * The parse failed.
   * 
   * @param err	The reason for the parsing failure.
   */
  case class ParseFailure(err: String) extends ParseResult
  
  /**
   * Get a console native handlers can use.
   */
  def console: Console
  
  /**
   * Get the context used by this executor instance.
   */
  def context: Context
  
  /**
   * Parse the given string and return a sequence of basic atoms.  The
   * sequence may be empty, and it may be lazily constructed.
   * 
   * If operators are present in the stream, and applied, any side effects
   * will have been executed by the time this method returns.
   * 
   * @param text		The text to parse.
   * @return	The sequence of atoms.
   */
  def parse(text: String): ParseResult
  
  /**
   * Handle atoms specially; print them as parse strings.
   * 
   * @param atom  An atom.
   * @return  The parse string.
   */
  def writeProperty(atom: BasicAtom) = atom.toParseString
}
