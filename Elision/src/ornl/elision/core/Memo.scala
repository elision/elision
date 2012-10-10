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

import com.strangegizmo.cdb._
import scala.collection.mutable.{OpenHashMap => HashMap}
import scala.collection.mutable.HashSet

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

  /** Declare the Elision property for turning memoization on/off. */
  knownExecutor.declareProperty("cache",
                                "Whether to use memoization caching or not.",
                                true)

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
  private var _cache = new HashMap[(BasicAtom,Set[String]),(BasicAtom,Int)]()
  
  /**
   * This set holds atoms that are in their "normal form" state and do not
   * get rewritten.
   */
  private var _normal = new HashSet[(BasicAtom,Set[String])]()
  
  /**
   * Track whether anything has been added at a particular cache level.  If
   * so, set the dirty flag to true.
   */
  private var _dirty = new Array[Boolean](_LIMIT)
  
  //======================================================================
  // Write the database.
  //======================================================================
  
  /**
   * Write the online cache to the constant store, and then replace the online
   * cache.  This operation should block access to the cache, and should be
   * infrequent because of that.
   * 
   * Only data at cache levels marked as "dirty" is written by this process.
   */
  private def createDB() {
    // Start writing the file(s).
    val makers = new Array[CdbMake](_LIMIT)
    for (index <- 0 until _LIMIT)
      if (_dirty(index)) makers(index).start(_offlinecache+"_"+index)
    
    // Write the content of the online caches.
    for (pair <- _cache.keySet) {
      _cache.get(pair) match {
        case None =>
        case Some((newatom, level)) =>
          makers(level).add(
              (pair._1.toParseString + ";" + pair._2.toString).getBytes(),
              newatom.toParseString.getBytes())
      }
    } // Add all entries.
    
    // Done.  Finish all caches.
    for (index <- 0 until _LIMIT)
      if (_dirty(index)) makers(index).finish()
  }
  
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
  def get(atom: BasicAtom, rulesets: Set[String]) = {

    // Constants, variables, and symbols should never be
    // rewritten. So, if we are trying to get one of those just
    // return the original atom.
    if (atom.isInstanceOf[Literal[_]] ||
        atom.isInstanceOf[Variable]) {
      Some((atom, true))
    }

    // Return nothing if caching is turned off.
    else if (!knownExecutor.getProperty[Boolean]("cache")) {
      None
    }
   
    // We are doing caching. Actually look in the cache.
    else {
      if (_normal.contains((atom,rulesets))) {
        //println("** Elision: Cache read rewrite " + atom.toParseString + " -> " + atom.toParseString + " for rulsests " + rulesets)
        Some((atom, false))
      } else {
        _cache.get((atom, rulesets)) match {
          case None =>
            // Cache miss.
            None
          case Some((value, level)) =>
            // Cache hit.
            //println("** Elision: Cache read rewrite " + atom.toParseString + " -> " + value.toParseString + " for rulsests " + rulesets)
            Some((value, true))
        }
      }
    }
  }
  
  /**
   * Put something in the cache.
   * 
   * @param atom      The unrewritten atom.
   * @param rulesets  The rulesets used to rewrite.
   * @param value     The final rewritten atom.
   * @param level     The lowest level of the rewrite.
   */
  def put(atom: BasicAtom, rulesets: Set[String], value: BasicAtom, level: Int) {

    // Do nothing if caching is turned off.
    if (!knownExecutor.getProperty[Boolean]("cache")) {
      return
    }

    //println("** Elision: Cache add rewrite " + atom.toParseString + " -> " + value.toParseString + " for rulsests " + rulesets)
    val lvl = 0 max level min (_LIMIT-1)
    if (atom eq value) {
      _normal.add(atom, rulesets)
    } else {
      _cache((atom, rulesets)) = (value, level)
    }
  }
}
