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
package ornl.elision.context

import java.lang.Boolean
import java.lang.Integer
import ornl.elision.core.BasicAtom
import ornl.elision.util.ElisionException
import ornl.elision.util.{Console, PropertyManager}
import ornl.elision.util.Loc
import ornl.elision.core.Dialect
import scala.io.Source

/**
 * A requested setting is not present.
 * 
 * @param msg A human-readable message describing the error.
 */
class MissingSettingException(msg: String)
extends ElisionException(Loc.internal, msg)

/**
 * An executor is a class that can convert a string into a sequence of atoms.
 * This can be done by parsing the usual Elision representation of atoms, or
 * by some other means.
 * 
 * An executor also holds some other information.  Specifically it holds two
 * similar items.
 * 
 *  - A set of __properties__ that hold user-configurable items to control
 *    Elision.  That is, these are user-visible and mutable.
 *  - The collection of __settings__ that were parsed from the command line,
 *    or left at their defaults.  These are (typically) regarded as immutable
 *    and are user-visible.  These are held for later reference.
 * 
 * There is also a cache held by the `Context` for items that should be saved
 * when / if the context is saved.
 * 
 * Note that the settings must be specified at construction time.
 */
trait Executor extends PropertyManager {
  
  /**
   * The settings, from the command line parser.
   */
  val settings: Map[String,String]
  
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
   * @param name    Name of the data source.  This can be a filename,
   *                `"(console)"`, or `""` for an internal source.
   * @param text		The text to parse.
   * @return	The sequence of atoms.
   */
  def parse(name: String, text: String) =
    Dialect.parse('elision, name, Source.fromString(text))
    
  /**
   * Get the value of a setting, which must be defined.  If the setting 
   */
  def getSetting(name: String) = settings.get(name) match {
    case None =>
      throw new MissingSettingException("The setting "+
          ornl.elision.util.toQuotedString(name)+
          " is required by Elision, but is not defined.  The current " +
          "operation cannot continue.")
    case Some(value) =>
      value
  }
}
