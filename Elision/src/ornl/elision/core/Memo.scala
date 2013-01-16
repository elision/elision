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

// KIRK: Using a weak hashmap REALLY slows things down.
import scala.collection.mutable.{OpenHashMap => HashMap}
import scala.collection.mutable.SynchronizedMap
import scala.collection.mutable.HashSet
import ornl.elision.util.PropertyManager
import scala.collection.mutable.BitSet

/**
 * Provide an online and offline memoization system for rewriting.  There are
 * multiple ways this could be implemented; at present it is rather primitive.
 * Please see https://github.com/elision/elision/wiki/Memoization for some
 * details.
 */
object Memo {

  //======================================================================
  // Figure out where to read and store the persistent cache.  This will be
  // turned into a database transaction eventually.
  //======================================================================

  /** Access to system properties. */
  private val _prop = new scala.sys.SystemProperties
  
  /**
   * The maximum depth of atoms to memoize, or -1 for no limit.
   */
  private var _maxdepth: BigInt = -1
  
  /** Set whether the cache is being used or not. */
  private var _usingcache = true

  /** Declare the Elision property for turning memoization on/off. */
  knownExecutor.declareProperty("cache",
      "Whether to use memoization caching or not.",
      _usingcache,
      (pm: PropertyManager) => {
        _usingcache = pm.getProperty[Boolean]("cache")
      })
                                
  /** Declare the Elision property for the maximum depth of atoms to memoize. */
  knownExecutor.declareProperty("maxcachedepth",
      "The maximum depth of atoms to memoize.",
      _maxdepth,
      (pm: PropertyManager) => {
        _maxdepth = pm.getProperty[BigInt]("maxcachedepth")
      })

  /** The user's home folder. */
  private val _home = {
    val root = System.getenv("ELISION_ROOT")
    if (root != null) {
      root
    } else {
      _prop("user.home")
    }
  }
  
  /** Figure out the location to store the cache. */
  protected val _offlinecache = {
    val ec = System.getenv("ELISION_CACHE")
    if (ec != null) {
      ec
    } else {
      val fname = (if (_prop("path.separator") == ":") ".elision-cache"
        else "elision-cache")
      _home + _prop("file.separator") + fname
    }
  }
  
  //======================================================================
  // Statistics.
  //======================================================================
  
  /** Record every cache hit. */
  private var _hits = 0L
  
  /** Record every cache miss. */
  private var _misses = 0L
  
  /**
   * Get information about the cache and its use.
   * 
   * @return Number of hits, number of misses, and size of online cache.
   */
  def getStats = (_hits, _misses, _cache.size)
  
  /**
   * Print information about the Elision cache.
   */
  def showStats {
    println("""
        |Elision Cache
        |=============
        |Hits:    %10d
        |Misses:  %10d
        |Size:    %10d
        |""".stripMargin.format(_hits, _misses, _cache.size + _normal.size))
  }
  
  //======================================================================
  // Limits.
  //======================================================================
  
  /** Number of cache levels.  Do not change this! */
  private val _LIMIT = 10
  /** Cache size that triggers write.  Careful!  Low values are bad. */
  private val _SIZE = 10000
  
  //======================================================================
  // The online cache.
  //======================================================================
  
  /**
   * Provide the online cache.  The cache stores keys in the form of pairs,
   * with the basic atom and then the set of active rulesets.  The value is
   * the completely rewritten atom and the cache level.  No knowledge of the
   * rewrite limit is stored!
   */
  private var _cache = 
    new HashMap[((Int,BigInt),BitSet),(BasicAtom,Int)]() 
  
  /**
   * This set holds atoms that are in their "normal form" state and do not
   * get rewritten.
   */
  private var _normal = 
    new HashMap[((Int,BigInt),BitSet),Unit]() 
  
  /**
   * Track whether anything has been added at a particular cache level.  If
   * so, set the dirty flag to true.
   */
  private var _dirty = new Array[Boolean](_LIMIT)
  
  //======================================================================
  // Cache access.
  //======================================================================
  
  /**
   * Test the cache for an atom.
   * 
   * @param atom      The unrewritten atom to search for.
   * @param rulesets  The rulesets that will be used to rewrite.
   * @return  The optional fully-rewritten atom and a flag indicating whether
   *          the input atom is already in normal form.
   */
  def get(atom: BasicAtom, rulesets: BitSet): Option[(BasicAtom, Boolean)] = {

    // Return nothing if caching is turned off.
    if (! _usingcache) return None
   
    // Never check for atoms whose depth is greater than the maximum.
    //if (_maxdepth >= 0 && atom.depth > _maxdepth) return None

    // Constants, variables, and symbols should never be
    // rewritten. So, if we are trying to get one of those just
    // return the original atom.
    if (atom.isInstanceOf[Literal[_]] ||
        atom.isInstanceOf[Variable]) {
      return Some((atom, true))
    }
    
    // We are doing caching. Actually look in the cache.
    var r: Option[(BasicAtom, Boolean)] = None
    val t0 = System.currentTimeMillis()
    if (_normal.contains(((atom.hashCode, atom.otherHashCode),rulesets))) {
      r = Some((atom, false))
    } else {
      _cache.get(((atom.hashCode, atom.otherHashCode), rulesets)) match {
        case None =>
          // Cache miss.
          r = None
        case Some((value, level)) =>
          // Cache hit.
          r = Some((value, true))
      }
    }

    // Return the cache lookup result.
    val t1 = System.currentTimeMillis()
    if (t1 - t0 > 2000) {
      println("** Memo: lookup time = " + (t1-t0) + "(ms) size=" + _normal.size);
    }
    return r
  }
  
  /**
   * Put something in the cache.
   * 
   * @param atom      The unrewritten atom.
   * @param rulesets  The rulesets used to rewrite.
   * @param value     The final rewritten atom.
   * @param level     The lowest level of the rewrite.
   */
  def put(atom: BasicAtom, rulesets: BitSet, value: BasicAtom, level: Int) {

    // Do nothing if caching is turned off.
    if (! _usingcache) return
    
    // Never cache atoms whose depth is greater than the maximum.
    //if (_maxdepth >= 0 && atom.depth > _maxdepth) return

    // Store the item in the cache.
    val t0 = System.nanoTime
    val lvl = 0 max level min (_LIMIT-1)
    _normal.synchronized {
      _normal(((value.hashCode, value.otherHashCode), rulesets)) = Unit
    }
    if (!(atom eq value)) {
      _cache.synchronized {
        _cache(((atom.hashCode, atom.otherHashCode), rulesets)) = (value, level)
      }
    }
    val t1 = System.currentTimeMillis()
    if ((t1.toDouble-t0.toDouble) > 2000) {
      println("** Memo: add time = " + (t1.toDouble-t0.toDouble) + "(ms)")
    }
  }
}
