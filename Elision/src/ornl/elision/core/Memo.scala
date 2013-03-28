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

import java.util.HashMap
import ornl.elision.util.PropertyManager
import scala.collection.mutable.BitSet
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Queue
import ornl.elision.util.Debugger

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
  
  /** Maximum cache size. */
  private val _maxsize = 4096
  
  /** The replacement policy being used. */
  private val _replacementPolicy = "LFU"

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
    knownExecutor.console.panicln("""
        |Elision Cache
        |=============
        |Hits:          %10d
        |Misses:        %10d
        |Cache Size:    %10d
        |Norm Size:     %10d
        |""".stripMargin.format(_hits, _misses, _cache.size, _normal.size))
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
   * Keeps a count of how many times things in _cache have been accessed since
   * the last iteration of the replacement policy algorithm. 
   */  
  private var _cacheCounter =
    new HashMap[((Int,BigInt),BitSet), Long]()
  
  /**
   * Queue for implementing a FIFO replacement policy.
   */  
  private var _cacheFIFO = new Queue[((Int,BigInt),BitSet)]
    
  /**
   * This set holds atoms that are in their "normal form" state and do not
   * get rewritten.
   */
  private var _normal = 
    new HashMap[((Int,BigInt),BitSet),Unit]() 
  
  /** 
   * Keeps a count of how many times things in _normal have been accessed 
   * since the last iteration of the replacement policy algorithm. 
   */  
  private var _normalCounter =
    new HashMap[((Int,BigInt),BitSet), Long]()
  
  /**
   * Queue for implementing a FIFO replacement policy.
   */  
  private var _normalFIFO = new Queue[((Int,BigInt),BitSet)]  
    
    
  /**
   * Track whether anything has been added at a particular cache level.  If
   * so, set the dirty flag to true.
   */
  private var _dirty = new Array[Boolean](_LIMIT)
  
  //======================================================================
  // Cache access.
  //======================================================================
  
  def clear = {
    this.synchronized {
      _cache.clear
      _normal.clear
      _cacheCounter.clear
      _normalCounter.clear
      _hits = 0
      _misses = 0
    }
    
  }
  /**
   * Test the cache for an atom.
   * 
   * @param atom      The unrewritten atom to search for.
   * @param rulesets  The rulesets that will be used to rewrite.
   * @return  The optional fully-rewritten atom and a flag indicating whether
   *          the input atom is already in normal form.
   */
  def get(atom: BasicAtom, rulesets: BitSet): Option[(BasicAtom, Boolean)] = {
    if(_replacementPolicy.equals("LFU") || _replacementPolicy.equals("LRU"))
      get_LFU(atom,rulesets)
    else if(_replacementPolicy.equals("FIFO"))
      get_FIFO(atom,rulesets)
    else
      get_old(atom,rulesets)
  }
  
  def get_FIFO(atom: BasicAtom, rulesets: BitSet): Option[(BasicAtom, Boolean)] = {

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
    
    var hasKey = false
    _normal.synchronized {
      hasKey = _normal.containsKey(((atom.hashCode, atom.otherHashCode),rulesets))
      if (hasKey) {
        _hits = _hits + 1
        r = Some((atom, false))
      }
    } 
    if(!hasKey) {
      _cache.synchronized {
        _cache.get(((atom.hashCode, atom.otherHashCode), rulesets)) match {
          case null =>
            // Cache miss.
            _misses = _misses + 1
            r = None
          case (value, level) =>
            // Cache hit.
            _hits = _hits + 1        
            r = Some((value, true))
        }
      }
    }

    // Return the cache lookup result.
    Debugger("memo") {
      val t1 = System.currentTimeMillis()
      if (t1 - t0 > 2000) {
        Debugger.debugln("lookup time = " + (t1-t0) +
            "(ms) size=" + _cache.size);
      }
    }
    return r
  }
  
  def get_LFU(atom: BasicAtom, rulesets: BitSet): Option[(BasicAtom, Boolean)] = {

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
    
    var hasKey = false
    _normal.synchronized {
      hasKey = _normal.containsKey(((atom.hashCode, atom.otherHashCode),rulesets))
      if (hasKey) {
        _hits = _hits + 1
        _incNormalCounter(((atom.hashCode, atom.otherHashCode),rulesets))
        r = Some((atom, false))
      }
    }
    if(!hasKey) {
      _cache.synchronized {
        _cache.get(((atom.hashCode, atom.otherHashCode), rulesets)) match {
          case null =>
            // Cache miss.
            _misses = _misses + 1
            r = None
          case (value, level) =>
            // Cache hit.
            _hits = _hits + 1     
            
            _incCacheCounter(((atom.hashCode, atom.otherHashCode), rulesets))        
            r = Some((value, true))
        }
      }
    }

    // Return the cache lookup result.
    val t1 = System.currentTimeMillis()
    Debugger("memo") {
      if (t1 - t0 > 2000) {
        Debugger.debugln("lookup time = " + (t1-t0) +
            "(ms) size=" + _cache.size);
      }
    }
    return r
  }
  
  
  def get_old(atom: BasicAtom, rulesets: BitSet): Option[(BasicAtom, Boolean)] = {

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
    
    var hasKey = false
    _normal.synchronized {
      hasKey = _normal.containsKey(((atom.hashCode, atom.otherHashCode),rulesets))
      if(hasKey) {
        _hits = _hits + 1
        r = Some((atom, false))
      }
    }
    if(!hasKey) {
      _cache.synchronized {
        _cache.get(((atom.hashCode, atom.otherHashCode), rulesets)) match {
          case null =>
            // Cache miss.
            _misses = _misses + 1
            r = None
          case (value, level) =>
            // Cache hit.
            _hits = _hits + 1
            r = Some((value, true))
        }
      }
    }

    // Return the cache lookup result.
    Debugger("memo") {
      val t1 = System.currentTimeMillis()
      if (t1 - t0 > 2000) {
        Debugger.debugln("lookup time = " + (t1-t0) +
            "(ms) size=" + _cache.size);
      }
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
    if(_replacementPolicy.equals("LFU") || _replacementPolicy.equals("LRU"))
      put_LFU(atom,rulesets,value,level)
    else if(_replacementPolicy.equals("FIFO"))
      put_FIFO(atom,rulesets,value,level)
    else
      put_old(atom,rulesets,value,level)
  }
  
  
  def put_FIFO(atom: BasicAtom, rulesets: BitSet, value: BasicAtom, level: Int) {

    // Do nothing if caching is turned off.
    if (! _usingcache) return
    
    // Never cache atoms whose depth is greater than the maximum.
    //if (_maxdepth >= 0 && atom.depth > _maxdepth) return

    // Store the item in the cache.
    val t0 = System.currentTimeMillis()
    val lvl = 0 max level min (_LIMIT-1)
    
    _normal.synchronized {
      _replacementPolicyNormal
      _normal.put(((value.hashCode, value.otherHashCode), rulesets), Unit)
      _normalFIFO.enqueue(((value.hashCode, value.otherHashCode), rulesets))
    }
    if (!(atom eq value)) {
      _cache.synchronized {
        _replacementPolicyCache
        _cache.put(((atom.hashCode, atom.otherHashCode), rulesets), (value, level))
        _cacheFIFO.enqueue(((atom.hashCode, atom.otherHashCode), rulesets))
      }
    }
    Debugger("memo") {
      val t1 = System.currentTimeMillis()
      if (t1 - t0 > 2000) {
        Debugger.debugln("add time = " + (t1-t0) +
            "(ms) size=" + _cache.size);
      }
    }
  }
  
  
  def put_LFU(atom: BasicAtom, rulesets: BitSet, value: BasicAtom, level: Int) {

    // Do nothing if caching is turned off.
    if (! _usingcache) return
    
    // Never cache atoms whose depth is greater than the maximum.
    //if (_maxdepth >= 0 && atom.depth > _maxdepth) return

    // Store the item in the cache.
    val t0 = System.currentTimeMillis()
    val lvl = 0 max level min (_LIMIT-1)

    _normal.synchronized {
      _replacementPolicyNormal
      _normal.put(((value.hashCode, value.otherHashCode), rulesets), Unit)
      _incNormalCounter(((value.hashCode, value.otherHashCode),rulesets))
    }
    if (!(atom eq value)) {
      _cache.synchronized {
        _replacementPolicyCache
        _cache.put(((atom.hashCode, atom.otherHashCode), rulesets), (value, level))
        _incCacheCounter(((atom.hashCode, atom.otherHashCode), rulesets)) 
      }
    }
    Debugger("memo") {
      val t1 = System.currentTimeMillis()
      if (t1 - t0 > 2000) {
        Debugger.debugln("add time = " + (t1-t0) +
            "(ms) size=" + _cache.size);
      }
    }
  }
  
  
  def put_old(atom: BasicAtom, rulesets: BitSet, value: BasicAtom, level: Int) {

    // Do nothing if caching is turned off.
    if (! _usingcache) return
    
    // Never cache atoms whose depth is greater than the maximum.
    //if (_maxdepth >= 0 && atom.depth > _maxdepth) return

    // Store the item in the cache.
    val t0 = System.currentTimeMillis()
    val lvl = 0 max level min (_LIMIT-1)
    _normal.synchronized {
      _normal.put(((value.hashCode, value.otherHashCode), rulesets), Unit)
    }
    if (!(atom eq value)) {
      _cache.synchronized {
        _cache.put(((atom.hashCode, atom.otherHashCode), rulesets), (value, level))
      }
    }
    Debugger("memo") {
      val t1 = System.currentTimeMillis()
      if (t1 - t0 > 2000) {
        Debugger.debugln("add time = " + (t1-t0) +
            "(ms) size=" + _cache.size);
      }
    }
  }
  
  
  /** Implementation of a replacement policy for _cache. */
  def _replacementPolicyCache {
    if(_replacementPolicy.equals("LFU"))
      _replacementPolicyCacheLFU
    else if(_replacementPolicy.equals("LRU"))
      _replacementPolicyCacheLRU
    else if(_replacementPolicy.equals("FIFO"))
      _replacementPolicyCacheFIFO
  }
  
  def _replacementPolicyCacheFIFO {
    if(_cache.size < _maxsize)
       return
    
    // remove all items in the front half of the queue.
    while(_cacheFIFO.size > _maxsize/2) {
      val key = _cacheFIFO.dequeue
      _cache.remove(key)
    }
  }
  
  def _replacementPolicyCacheLRU {
    if(_cache.size < _maxsize)
       return
       
    // I haz a buckit. This will keep track of the items that haven't been 
    // accessed since the last checkup.
    var bucket = new ListBuffer[((Int,BigInt),BitSet)]
    
    val keyIterator = _cache.keySet.iterator
    while(keyIterator.hasNext) {
      val key = keyIterator.next
      val counterHasKey = _cacheCounter.containsKey(key)
      
      val count = if(counterHasKey) {
          _cacheCounter.get(key)
        }
        else {
          0
        }
      
      if(count == 0) {
        bucket += key
      }
      else {
        _cacheCounter.put(key,0)
      }
    }
    
    // remove the offending items.
    for(key <- bucket) {
      _cache.remove(key)
      _cacheCounter.remove(key)
    }
  }
  
  
  
  def _replacementPolicyCacheLFU {
    if(_cache.size < _maxsize)
       return
    
    var lowestCount = Long.MaxValue
    
    // I haz a buckit. This will keep track of the items with the lowest count.
    var bucket = new ListBuffer[((Int,BigInt),BitSet)]
    
    // find the items with the lowest count and put their keys in the bucket.
    // An iterator was used here because there wasn't a very convenient way to 
    // do for each loops in scala over a java Set.
    val keyIterator = _cache.keySet.iterator
    while(keyIterator.hasNext) {
      val key = keyIterator.next
      val counterHasKey = _cacheCounter.containsKey(key)
      
      val count = if(counterHasKey) {
          _cacheCounter.get(key)
        }
        else {
          0
        }
 
      if(count < lowestCount) {
        lowestCount = count
        bucket.clear
      }
      if(count == lowestCount) {
        bucket += key 
      }
    } // endwhile
    
    // remove all items from the cache that are in our final bucket.
    for(key <- bucket) {
      _cache.remove(key)
    }
    
    // replace the counter with a new one.
    _cacheCounter.clear
  }
  
  
  
  
  
  /** Implementation of a replacement policy for _normal. */
  def _replacementPolicyNormal {
    if(_replacementPolicy.equals("LFU"))
      _replacementPolicyNormalLFU
    else if(_replacementPolicy.equals("LRU"))
      _replacementPolicyNormalLRU
    else if(_replacementPolicy.equals("FIFO"))
      _replacementPolicyNormalFIFO
  }
  
  
  def _replacementPolicyNormalFIFO {
    if(_normal.size < _maxsize)
       return
    
    // remove all items in the front half of the queue.
    while(_normal.size > _maxsize/2) {
      val key = _normalFIFO.dequeue
      _normal.remove(key)
    }
  }
  
  
  def _replacementPolicyNormalLRU {
    if(_normal.size < _maxsize)
       return
       
    // I haz a buckit. This will keep track of the items that haven't been 
    // accessed since the last checkup.
    var bucket = new ListBuffer[((Int,BigInt),BitSet)]
    
    val keyIterator = _normal.keySet.iterator
    while(keyIterator.hasNext) {
      val key = keyIterator.next
      val counterHasKey = _normalCounter.containsKey(key)
      
      val count = if(counterHasKey) {
          _normalCounter.get(key)
        }
        else {
          0
        }
      
      if(count == 0) {
        bucket += key
      }
      else {
        _normalCounter.put(key,0)
      }
    }
    
    // remove the offending items.
    for(key <- bucket) {
      _normal.remove(key)
      _normalCounter.remove(key)
    }
    
  }
  
  def _replacementPolicyNormalLFU {
    if(_normal.size < _maxsize)
       return
    
    var lowestCount = Long.MaxValue
    
    // I haz a buckit. This will keep track of the items with the lowest count.
    var bucket = new ListBuffer[((Int,BigInt),BitSet)]
    
    // find the items with the lowest count and put their keys in the bucket.
    // An iterator was used here because there wasn't a very convenient way to 
    // do for each loops in scala over a java Set.
    val keyIterator = _normal.keySet.iterator
    while(keyIterator.hasNext) {
      val key = keyIterator.next
      val counterHasKey = _normalCounter.containsKey(key)
      
      val count = if(counterHasKey) {
          _normalCounter.get(key)
        }
        else {
          0
        }
 
      if(count < lowestCount) {
        lowestCount = count
        bucket.clear
      }
      if(count == lowestCount) {
        bucket += key
      }
    } // endwhile
    
    // remove all items from the cache that are in our final bucket.
    for(key <- bucket) {
      _normal.remove(key)
    }
    
    // replace the counter with a new one.
    _normalCounter.clear
  }
  
  
  
  
  /** safely increments the counter for a key in _cacheCounter. */
  def _incCacheCounter(key : ((Int,BigInt),BitSet)) {
    val counterHasKey = _cacheCounter.containsKey(key)
    
    val curCount = if(counterHasKey) {
        _cacheCounter.get(key)
      }
      else {
        0
      }
    _cacheCounter.put(key, curCount + 1)
  }
  
  /** safely increments the counter for a key in _normalCounter. */
  def _incNormalCounter(key : ((Int,BigInt),BitSet)) {
    val counterHasKey = _normalCounter.containsKey(key)
    
    val curCount = if(counterHasKey) {
        _normalCounter.get(key)
      }
      else {
        0
      }
    _normalCounter.put(key, curCount + 1)
  }
}
