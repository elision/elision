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
======================================================================*/
package ornl.elision.util

import scala.reflect.runtime.universe.{TypeTag, typeOf, weakTypeTag, typeTag, runtimeMirror}

/**
 * The cache contains the wrong type of item for the requested key.
 * 
 * @param msg   The human-readable message explaining the problem.
 */
class CacheException(msg: String) extends ElisionException(Loc.internal, msg)

/**
 * Provide a simple, name-indexed, general purpose cache for any value.
 * The expected type must be specified when the item is extracted from
 * the cache.
 */
trait Cache {
  /** Cache for use by native methods. */
  private val _cache = scala.collection.mutable.OpenHashMap[String,Any]()
  
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
   * @param key     The key for the item to retrieve.
   * @param default The value to return if the specified key is not present.
   *                If this is returned, it is also stored, so be sure to type
   *                it correctly.
   * @return  The requested value, or the default value.
   */
  def fetchAs[TYPE: TypeTag](key: String, default: TYPE) = {
    _cache.get(key) match {
      case None =>
        _cache(key) = default
        default
        
      case Some(item) =>
        try {
          item.asInstanceOf[TYPE]
        } catch {
          case except: Throwable =>
            val cls = item.getClass()
            val itemtype =
              runtimeMirror(cls.getClassLoader).classSymbol(cls).toType
            throw new CacheException(
              "The cache entry for key " + toQuotedString(key) +
              " is of the wrong type.  Expected a subclass of " +
              typeTag[TYPE].tpe +
              " but got " + itemtype + ".")
        }
    }
  }
  
  /**
   * Little method to get the type tag for an object.  Is there a better way?
   * @param TYPE  The actual type of the object.
   * @param thing The object.
   * @return  The type tag for TYPE.
   */
  private def getTypeTag[TYPE: TypeTag](thing: TYPE) = typeTag[TYPE]
  
  /**
   * Stash a value in the cache for later lookup with `fetchAs`.  Read the
   * documentation for `fetchAs` before you use the cache!
   * 
   * @param key   The key.
   * @param value The value.
   * @return The stored value.
   */
  def stash[TYPE](key: String, value: TYPE) = {
    _cache(key) = value
    value
  }
}