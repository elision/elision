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
import scala.util.Properties

/**
 * The cache contains the wrong type of item for the requested key.
 * 
 * @param msg		The human-readable message explaining the problem.
 */
class CacheException(msg: String) extends ElisionException(msg)

/**
 * Manage a collection of properties.
 */
trait PropertyManager {
  val _prop2val = scala.collection.mutable.HashMap[String,Any]()
  val _prop2desc = scala.collection.mutable.HashMap[String,String]()
  
  /**
   * Declare a property.
   * 
   * @param name        The property name.
   * @param description Human-readable description of the property.
   * @param default     The default value of the property.
   * @return  The default value.
   */
  def declareProperty[TYPE](name: String, description: String, default: TYPE) {
    default match {
      case str: String =>
      case value: BigInt =>
      case value: Int =>
      case flag: Boolean =>
      case atom: BasicAtom =>
      case _ =>
        throw new CacheException("Unsupported data type for properties.  " +
        		"Properties must be strings, integers, or Booleans, and may " +
        		"be any BasicAtom, but got " +
        		Manifest.classType(name.getClass) + ".")
    }
    _prop2val(name) = default
    _prop2desc(name) = description
    default
  }
  
  /**
   * Get the value of a property.  Properties are intended to be used to
   * control various functions of the executor and modular extensions to
   * it, and not just for storing arbitrary data during execution.
   * 
   * Properties must be declared before they can be set or read.  See
   * `declareProperty`.  The type of the property may not be changed once
   * it has been declared.
   * 
   * When getting a property value, specify the `TYPE` so the returned value
   * is correctly typed.
   * 
   * @param key     The property name.
   * @param default The default value of the property.
   * @return  The requested property value.
   */
  def getProperty[TYPE](name: String)
  (implicit mTYPE: scala.reflect.Manifest[TYPE]): TYPE = {
    _prop2val.get(name) match {
      case None =>
        throw new CacheException("No such property: " + toEString(name) + ".")
      case Some(item) =>
        item.asInstanceOf[TYPE]
    }
  }
  
  /**
   * Set the value of a property.  See the documentation of `getProperty` for
   * information about the use of properties.
   * 
   * @param key     The property name.
   * @param value   The property value.
   * @return  The new property value.
   */
  def setProperty[TYPE](name: String, value: TYPE)
  (implicit mTYPE: scala.reflect.Manifest[TYPE]) = {
    _prop2val.get(name) match {
      case None =>
        throw new CacheException("No such property: " + toEString(name) + ".")
      case Some(item) =>
        _prop2val(name) = value
        value
//        if (Manifest.classType(item.getClass) <:< mTYPE) {
//          _prop2val(name) = value
//          value
//        } else {
//          throw new CacheException(
//              "The property " + toEString(name) +
//              " is of the wrong type.  Expected " + mTYPE.toString +
//              " but got " + Manifest.classType(item.getClass) + ".")
//        }
    }
  }
  
  /**
   * Get all declared properties.
   * 
   * @return  The returned value is a set of entries of the form (name,
   *          description, value).
   */
  def getProperties = {
    var list = List[(String, String, Any)]()
    for (name <- _prop2val.
        keySet.
        toList.
        sortWith(_.toLowerCase < _.toLowerCase).
        filter(!_.startsWith("_"))) {
      val description = _prop2desc(name)
      val value = _prop2val(name)
      list :+= ((name, description, value))
    } // Collect all properties.
    list
  }
}

/**
 * Indicate that it a history is maintained.  Limited access to the history
 * is granted by implementing this trait.
 */
trait HasHistory {  
  /**
   * Add a line to the history, if one is being maintained.  If the processor
   * maintains a history it should override this to enable adding the given
   * line to the history, if that is desired.  This is used by the system to
   * add informational lines to the history.  The default implementation does
   * nothing.
   * 
   * @param line  The line to add to the history.
   */
  def addHistoryLine(line: String) {}
  
  /**
   * Get an iterator over the history.  By default this returns the empty
   * iterator, so override this to return the appropriate iterator if your
   * processor supports history.
   */
  def getHistoryIterator: Iterator[String] = Set().iterator
  
  /**
   * Get the file that holds the persistent history.  By default this returns
   * the string `(no history file)`, so you should override this if you have
   * a history file.
   */
  def getHistoryFilename: String = "(no history file)"
}

/**
 * Indicate that it is possible to enable and disable tracing of parsing at
 * runtime.  An executor may (or may not) implement this.
 * 
 * Typically enabling and disabling tracing require rebuilding the parser, so
 * this trait is abstract.
 */
trait TraceableParse {
  /**
   * Specify whether to trace the parser.
   * 
   * @param enable  If true, trace the parser.  If false, do not.
   */
  def trace_=(enable: Boolean): Unit
  
  /**
   * Determine whether tracing is enabled.
   */
  def trace: Boolean
}

/**
 * Specify that it is possible to enable and disable printing the execution
 * time of each atom that is parsed.  To use this you must implement the
 * `reportElapsed` method that reports the elapsed time, and make sure that the
 * `startTimer` and `stopTimer` methods are called at the appropriate places.
 * 
 * Do not use `reportElapsed` directly.  Implement it, and then invoke
 * `showElapsed` at the appropriate place in your code.  The `showElapsed`
 * method does nothing if timing is disabled, and invokes `reportElapsed` if
 * it is.
 * 
 * Even when timing is disabled, the elapsed time information is still kept
 * by `startTimer` and `stopTimer`, and can be checked by `getLastTimeMillis`,
 * `getLastTime`, or `getLastTimeString`.
 */
trait Timeable {
  /** Whether timing is enabled. */
  private var _timing = false
  
  /** Most recent time at start. */
  private var _starttime = 0L
  
  /** Most recent `start`..`stop` interval. */
  private var _elapsed = 0L
  
  /**
   * Specify whether to time each atom's "execution."
   * 
   * @param enable  If true, enable timing.  If false, do not.
   */
  def timing_=(enable: Boolean) {
    _timing = enable
  }
  
  /** Determine whether timing is enabled. */
  def timing = _timing
  
  /** Start the timer. */
  def startTimer { _starttime = java.lang.System.currentTimeMillis() }

  /** Stop the timer. */
  def stopTimer { _elapsed = java.lang.System.currentTimeMillis() - _starttime }
  
  /**
   * Get the most recent duration.  This is set by `stopTimer` and is reported
   * in milliseconds.
   */
  def getLastTimeMillis = _elapsed
  
  /**
   * Get the most recent duration.  This is set by `stopTimer` and is reported
   * as a triple of minutes, seconds, and milliseconds.
   */
  def getLastTime = {
    (_elapsed / 60000, (_elapsed % 60000 / 1000), _elapsed % 1000)
  }
  
  /**
   * Get the most recent duration.  This is set by `stopTimer` and is reported
   * as a string of the form:
   * `MMMM:SS:mmm`
   * Where `MMMM` is the elapsed minutes, `SS` is the elapsed seconds, and
   * `mmm` is the elapsed milliseconds.
   */
  def getLastTimeString = {
    val (min,sec,mil) = getLastTime
    "%4d:%02d.%03d".format(min,sec,mil)
  }
    
  /**
   * If timing is enabled, generate a report on the most recent elapsed
   * interval measured by `startTimer`..`stopTimer`.
   */
  def showElapsed { if (_timing) reportElapsed }
  
  /**
   * The report method.  Implement this to generate a report.  Nothing
   * is provided; use one of the `getLastTime` methods to get elapsed time
   * data.  This is not intended for multithreaded use.
   */
  protected def reportElapsed: Unit
}

/**
 * An executor is a class that can convert a string into a sequence of atoms.
 * 
 * This can be done by parsing the usual Elision representation of atoms, or
 * by some other means.
 */
trait Executor extends PropertyManager {
  
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
              "The cache entry for key " + toEString(key) +
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
   * @return The stored value.
   */
  def stash[TYPE](key: String, value: TYPE)
  (implicit mTYPE: scala.reflect.Manifest[TYPE]) = {
    _cache(key) = value
    value
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
