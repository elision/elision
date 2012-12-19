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

/**
 * Generate text, in some form, from an Elision atom.  To use this, specify a
 * known output format, the atom to write, and (optionally) the appendable to
 * get the output.
 * 
 * You can register additional output formats via the `register` method.
 * 
 * @author Stacy Prowell (prowellsj@ornl.gov)
 */
object Generator {
  
  /** The map that holds the known generators, indexed by tag. */
  private val _map = scala.collection.mutable.OpenHashMap[String, Generator]()

  /**
   * Register a generator to handle a given tag.  Note that you can replace
   * the generator for a tag with this method.
   * 
   * @param tag   The tag.
   * @param gen   The generator.
   */
  def register(tag: String, gen: Generator) {
    _map(tag) = gen
  }
  
  /**
   * Obtain the generator for the given tag, if there is one.
   * 
   * @param tag   The tag.
   * @return  The specified generator, if known.
   * @throws  java.util.NoSuchElementException
   *          The tag is now known.
   */
  def apply(tag: String) = _map(tag)
  
  /**
   * Generate output from an atom using the specified generator.
   * 
   * @param tag     The tag.
   * @param atom    The atom.
   * @param buf     The buffer to get the result.  If not provided, one is
   *                created.
   * @param limit   The nesting limit of this atom.  If the limit is zero, then
   *                an ellipsis is printed instead of the atom.  Otherwise
   *                the limit is decreased for each parenthesized and bracketed
   *                pair, until zero is reached.  If the limit is negative,
   *                then there is effectively no limit.
   * @return        The appendable for chaining.
   * @throws  java.util.NoSuchElementException
   *          The tag is now known.
   */
  def apply(tag: String, atom: BasicAtom,
      buf: Appendable = new StringBuffer(), limit: Int = -1) = {
    _map(tag)(atom, buf, limit)
  }
  
  register("scala", ornl.elision.generators.ScalaGenerator)
  register("elision", ornl.elision.generators.ElisionGenerator)
}


/**
 * The general form for a generator.
 */
abstract class Generator {
  /**
   * Generate output from an atom.
   * 
   * @param atom    The atom.
   * @param buf     The buffer to get the result.  If not provided, one is
   *                created.
   * @param limit   The nesting limit of this atom.  If the limit is zero, then
   *                an ellipsis is printed instead of the atom.  Otherwise
   *                the limit is decreased for each parenthesized and bracketed
   *                pair, until zero is reached.  If the limit is negative,
   *                then there is effectively no limit.
   * @return        The appendable for chaining.
   */
  def apply(atom: BasicAtom,
      buf: Appendable = new StringBuffer(), limit: Int = -1): Appendable;
}