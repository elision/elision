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
package ornl.elision.core

import scala.collection.immutable.HashMap
import scala.collection.mutable.{OpenHashMap => MutableHashMap}
import scala.collection.mutable.SynchronizedMap

/**
 * Bindings are used to store variable / value maps used during matching, and
 * as the result of a successful match.
 * 
 * == Purpose ==
 * The bindings class is a proxy to Scala's `HashMap`.  This is not possible
 * to implement correctly (so far as I can tell) using the current (2.9)
 * collection proxy classes, because the type decoration is lost when
 * invoking methods that return a map.
 * 
 * This is not an atom!  If an atom to hold the bindings is needed, then
 * an instance of [[ornl.elision.core.BindingsAtom]] is created.  This is
 * typically done implicitly by Elision at the point it is needed, since
 * object creation is costly.
 * 
 * == Use ==
 * See the companion object for methods to create bindings, or for the
 * singleton object representing empty bindings.  Since bindings are
 * immutable(ish), it is wasteful to repeatedly create empty bindings!
 * 
 * Yes, bindings are actually slightly mutable.  There is a matching cache
 * accessible via `set`.  Beware!
 * 
 * @param self	The backing map.
 */
class Bindings(val self: HashMap[String, BasicAtom])
extends HashMap[String, BasicAtom] with Mutable {
  override def size = self.size
  override def foreach[U](f: ((String, BasicAtom)) =>  U): Unit =
    self.foreach(f)
  override def get(key: String): Option[BasicAtom] = self get key
  override def iterator: Iterator[(String, BasicAtom)] = self.iterator
  def +(kv: (String, BasicAtom)): Bindings = new Bindings(self + kv)
  def ++(other: Bindings): Bindings = new Bindings(self ++ other.self)
  override def -(key: String): Bindings = new Bindings(self - key)

  // @@@ JUST FOR DEBUGGING!!!
  var rewrites: MutableHashMap[BasicAtom, (BasicAtom, Boolean)] = new 
  MutableHashMap[BasicAtom, (BasicAtom, Boolean)]() with
  SynchronizedMap[BasicAtom, (BasicAtom, Boolean)];

  
  /** This is a cache used during associative / commutative matching. */
  private var _patcache: OmitSeq[BasicAtom] = null
  
  /** This is a cache used during associative / commutative matching. */
  private var _subcache: OmitSeq[BasicAtom] = null

  /** Construct an empty binding. */
  def this() = this(HashMap[String,BasicAtom]())
  
  /** Creates a copy of this Bindings. */
  def cloneBinds : Bindings = {
    this.synchronized {
      new Bindings(this.self)
    }
  }
  
  /**
   * Cache a list of patterns and subjects here.  This is useful during the
   * associative and commutative matching cycle.  These are accessed via
   * the `patterns` and `subjects` methods, and once accessed are
   * immediately forgotten!
   * 
   * @param patterns	The pattern sequence.
   * @param subjects	The subject sequence.
   * @return This binding instance, for chaining.
   */
  def set(patterns: OmitSeq[BasicAtom], subjects: OmitSeq[BasicAtom]) = {
    this.synchronized {
      if (_patcache == null) {
        _patcache = patterns
        _subcache = subjects
      }
      this
    }
  }
  
  /**
   * Get the cached patterns, if any.  These are immediately forgotten once
   * retrieved!
   * 
   * @return	The cached patterns.
   */
  def patterns: Option[OmitSeq[BasicAtom]] = {
    this.synchronized {
      val pc = _patcache
      if (pc == null) return None else {
        _patcache = null
        return Some(pc)
      }
    }
  }
  
  /**
   * Get the cached subjects, if any.  These are immeidately forgotten once
   * retrieved!
   * 
   * @return	The cached subjects.
   */
  def subjects: Option[OmitSeq[BasicAtom]] = {
    this.synchronized {
      val sc = _subcache
      if (sc == null) return None else {
        _subcache = null
        return Some(sc)
      }
    }
  }
} 

/**
 * Simplified construction of bindings.
 */
object Bindings {
  /**
   * An empty bindings object.  Since bindings are immutable, you should use
   * this to avoid object construction when you just need an empty set of
   * bindings.
   */
  val EmptyBinds = new Bindings(new HashMap[String,BasicAtom]())
  
  /**
   * Create bindings from the provided map.
   * 
   * @param map	The map to transform into a bindings object.
   */
  def apply(map: HashMap[String,BasicAtom]) = new Bindings(map)
  
  /**
   * Create a bindings from the provided pairs.
   * 
   * @param pairs	The pairs to include in the bindings.
   */
  def apply(pairs: (String,BasicAtom)*) =
    if (pairs.length == 0) EmptyBinds
    else new Bindings(HashMap(pairs:_*))
  
//  /**
//   * Get an empty bindings object.  This helps enforce the "don't create
//   * bindings objects unnecessarily" rule, and just returns the
//   * `EmptyBinds`.
//   */
//  def apply() = EmptyBinds
}
