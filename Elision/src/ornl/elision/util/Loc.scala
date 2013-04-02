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
package ornl.elision.util

/**
 * Provide information about the source of an atom's definition.
 * 
 * This is primarily useful for atoms that have originated from a parse, and
 * not so much for atoms that have been created programmatically - but still,
 * you may wish to pass along information.
 * 
 * See the companion object for common idioms.
 * 
 * The `source` corresponding to the empty string is taken to be "internal,"
 * and indicates that the location should be suppressed in display.
 * 
 * @param source  The source (a filename, the console, or internal) of the atom.
 * @param line    First line of the atom's declaration.
 * @param column  First column number of the atom's declaration.
 * @param text    The optional text of the atom's declaration.
 */
class Loc(val source: String, val line: Int, val column: Int,
    val text: Option[String]) {
  override def toString = if (line >= 0) {
    "[%s:%s:%s]" format (source, line, column)
  } else if (source == "") {
    ""
  } else {
    "[%s]" format (source)
  } 
}

/**
 * Creation and matching of location objects.
 */
object Loc {
  /**
   * Make a location object.
   * 
   * @param source  The source (a filename, the console, or internal) of the atom.
   * @param line    First line of the atom's declaration.
   * @param column  First column number of the atom's declaration.
   * @param text    The optional text of the atom's declaration.
   */
  def apply(source: String, line: Int, column: Int, text: Option[String]) =
    new Loc(source, line, column, text)
  
  /**
   * Obtain the parts of a location object for matching.
   * @param loc A location object.
   * @return  The source, line, column, and text.
   */
  def unapply(loc: Loc) = Some(loc.source, loc.line, loc.column, loc.text)
  
  /**
   * Obtain a location object for an atom created at the console.
   */
  def console = Loc("(console)", -1, -1, None)
  
  /**
   * Obtain a location object for an atom created programmatically (internally).
   */
  def internal = Loc("", -1, -1, None)
}
