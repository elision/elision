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
package ornl.elision.util

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
 * 
 * @author Stacy Prowell (prowellsj@ornl.gov)
 */
trait Timeable {
  /** Stack of prior start values. */
  private var stack = List[Long]()
  
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
  def startTimer {
    stack = _starttime :: stack
    _starttime = java.lang.System.currentTimeMillis()
  }

  /** Stop the timer. */
  def stopTimer {
    _elapsed = java.lang.System.currentTimeMillis() - _starttime
    stack match {
      case Nil =>
      case head :: tail =>
        _starttime = head
        stack = tail
    }
  }
  
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
