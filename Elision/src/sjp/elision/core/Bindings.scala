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
package sjp.elision.core
import scala.collection.immutable.HashMap

/**
 * Bindings are used to store variable / value maps used during matching, and
 * as the result of a successful match.
 * 
 * The bindings class is a proxy to Scala's HashMap.  This is not possible
 * to implement correctly (so far as I can tell) using the current (2.9)
 * collection proxy classes, becuase the type decoration is lost when
 * invoking methods that return a map.
 * 
 * @param self	The backing map.
 */
class Bindings(val self: HashMap[String, BasicAtom])
extends HashMap[String, BasicAtom] {
  override def size = self.size
  override def foreach[U](f: ((String, BasicAtom)) =>  U): Unit =
    self.foreach(f)
  override def get(key: String): Option[BasicAtom] = self get key
  override def iterator: Iterator[(String, BasicAtom)] = self.iterator
  def +(kv: (String, BasicAtom)): Bindings = new Bindings(self + kv)
  override def -(key: String): Bindings = new Bindings(self - key)
  
  private var _patcache: OmitSeq[BasicAtom] = null
  private var _subcache: OmitSeq[BasicAtom] = null
  
  /**
   * Cache a list of patterns and subjects here.  This is useful during the
   * associative and commutative matching cycle.  Once they are set, they
   * cannot be modified (subsequent attempts are ignored).
   * 
   * @param patterns	The pattern sequence.
   * @param subjects	The subject sequence.
   * @return This binding instance, for chaining.
   */
  def set(patterns: OmitSeq[BasicAtom], subjects: OmitSeq[BasicAtom]) = {
    if (_patcache == null) {
      _patcache = patterns
      _subcache = subjects
    }
    this
  }
  
  /**
   * Get the cached patterns, if any.
   * 
   * @return	The cached patterns.
   */
  def patterns: Option[OmitSeq[BasicAtom]] =
    if (_patcache == null) None else Some(_patcache)
  
  /**
   * Get the cached subjects, if any.
   * 
   * @return	The cached subjects.
   */
  def subjects: Option[OmitSeq[BasicAtom]] =
    if (_subcache == null) None else Some(_subcache)
} 

/**
 * Simplified construction of bindings.
 */
object Bindings {
  val EmptyBinds = new Bindings(new HashMap[String,BasicAtom]())
  def apply(map: HashMap[String,BasicAtom]) = new Bindings(map)
  def apply() = EmptyBinds
}
