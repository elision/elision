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
import ornl.elision.ElisionException

/**
 * The cache contains the wrong type of item for the requested key.
 * 
 * @param msg		The human-readable message explaining the problem.
 */
class CacheException(msg: String) extends ElisionException(msg)

/**
 * An executor is a class that can convert a string into a sequence of atoms.
 * 
 * This can be done by parsing the usual Elision representation of atoms, or
 * by some other means.
 */
trait Executor {
  
  /** A parse result. */
  abstract sealed class ParseResult
  
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
  
  /** Cache for use by native methods. */
  private val _cache = scala.collection.mutable.HashMap[String,Any]()
  
  /**
   * Provide typed access to the content of the cache.  This is intended for
   * use by native operators.  To avoid conflicts, name your cache entries
   * starting with your operator name.  This causes a performance hit, so
   * in general avoid using the cache.  Find somewhere else to shove your
   * data!
   * 
   * If the key is present, but of the wrong type, an exception is thrown.
   * This is a `CacheException`.
   * 
   * @param key			The key for the item to retrieve.
   * @param default	The value to return if the specified key is not present.
   * 								If this is returned, it is also stored, so be sure to type
   * 								it correctly.
   * @return	The requested value, or the default value.
   */
  def fetchAs[TYPE](key: String, default: TYPE)
  (implicit mTYPE: scala.reflect.Manifest[TYPE]): TYPE = {
    _cache.get(key) match {
      case None =>
        _cache(key) = default
        default
      case Some(item) =>
        if (mTYPE >:> Manifest.classType(key.getClass))
          throw new CacheException(
              "The cache entry for key " + toESymbol(key) +
              " is of the wrong type.  Expected " + mTYPE.toString +
              " but got " + Manifest.classType(key.getClass) + ".")
        else
          item.asInstanceOf[TYPE]
    }
  }
  
  /**
   * Stash a value in the cache for later lookup with `fetchAs`.  Read the
   * documentation for `fetchAs` before you use the cache!
   * 
   * @param key		The key.
   * @param value	The value.
   * @return	This executor.
   */
  def stash[TYPE](key: String, value: TYPE)
  (implicit mTYPE: scala.reflect.Manifest[TYPE]) = {
    _cache(key) = value
    this
  }
  
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
}
