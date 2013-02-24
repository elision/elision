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

object Timeable {
  /**
   * Convert a number of milliseconds into a triple consisting of minutes,
   * seconds, and milliseconds.
   * 
   * @param millis  The millisecond count.
   * @return  The triple of minutes, seconds, and milliseconds.
   */
  def asTriple(millis: Long) = {
    (millis / 60000, (millis % 60000 / 1000), millis % 1000)
  }
  
  /**
   * Convert a number of milliseconds into a string of the form:
   * `MMMM:SS:mmm`
   * Where `MMMM` is the elapsed minutes, `SS` is the elapsed seconds, and
   * `mmm` is the elapsed milliseconds.
   * 
   * @param millis  The millisecond count.
   * @return  The time string.
   */
  def asTimeString(millis: Long) = {
    val (min,sec,mil) = asTriple(millis)
    "%4d:%02d.%03d".format(min,sec,mil)
  }
}

/**
 * Allow a class to implement timing of operations.
 * 
 * To use this you must implement the `reportElapsed` method that reports the
 * elapsed time, and make sure that the `startTimer` and `stopTimer` methods
 * are called at the appropriate places.
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
  
  /** Cumulative value across all start / stop periods. */
  private var _cumulative = 0L
  
  /**
   * Set whether timing is enabled.  This is different from starting and
   * stopping the timer.  If timing is not enabled, then starting, stopping,
   * and reporting are all ignored.
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
    _cumulative += _elapsed
    stack match {
      case Nil =>
      case head :: tail =>
        _starttime = head
        stack = tail
    }
  }
  
  /**
   * Start the timer for the duration of the given closure's evaluation, and
   * then stop it.  If timing is not enabled, this still executes the closure.
   * 
   * @param closure The closure to execute. 
   */
  def time(closure: => Unit) {
    startTimer
    closure
    stopTimer
  }
  
  /**
   * Start the timer for the duration of the given closure's evaluation, and
   * then stop it.  If timing is not enabled, this still executes the closure.
   * 
   * @param closure The closure to execute.
   * @return  The value generated by the closure.
   */
  def time[RV](closure: => RV) = {
    startTimer
    val value = closure
    stopTimer
    value
  }
  
  /**
   * Get the most recent duration.  This is set by `stopTimer` and is reported
   * in milliseconds.
   */
  def getLastTimeMillis = _elapsed
  
  /**
   * Get the total elapsed time across all measured intervals.  This is updated
   * at each `stopTimer` event and is reported in milliseconds.
   */
  def getCumulativeTimeMillis = _cumulative
  
  /**
   * Get the most recent duration.  This is set by `stopTimer` and is reported
   * as a triple of minutes, seconds, and milliseconds.
   */
  def getLastTime = Timeable.asTriple(_elapsed)
  
  /**
   * Get the most recent duration.  This is set by `stopTimer` and is reported
   * as a string of the form:
   * `MMMM:SS:mmm`
   * Where `MMMM` is the elapsed minutes, `SS` is the elapsed seconds, and
   * `mmm` is the elapsed milliseconds.
   */
  def getLastTimeString = Timeable.asTimeString(_elapsed)
    
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