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
 * This is a class for managing and printing debug output during execution
 * in a manner that "agrees" with Elision.
 * 
 * Note: This class uses [ornl.elision.util.Console] to do its work.  Please
 * do not attempt to use this class to debug `Console`!
 * 
 * To use this class set up your debugging modes via `setDebugModes`, and
 * then use `debug` to print debugging messages.  The first argument is passed
 * by name, so you can use this to avoid construction costs by deferring method
 * invocation.
 * 
 * The tag is the second element to the debug statement to enable omitting it;
 * if omitted, the tag is treated as the empty string.
 * 
 * {{{
 * import ornl.elision.Debugger._
 * import ornl.elision.Debugger.Mode._
 * 
 * enableDebugModes("simple", ON)
 * enableDebugModes("timer", ON|TICK)
 * enableDebugModes("start", ON|START)
 * enableDebugModes("stop", ON|STOP)
 * 
 * debug("Debug point reached.", "simple", 4)
 * ...
 * debug("Starting timer.", "start")
 * ...
 * debug("Reporting elapsed time.", "stop")
 * }}}
 * 
 * Alternately, include the things you want to do in an `ifdebug` block.
 * 
 * {{{
 * import ornl.elision.Debugger._
 * 
 * ifdebug("simple") {
 *   println("The simple mode is enabled.")
 *   for (i <- 1 upto 1000) println(i)
 * }
 * }}}
 * 
 * There are several things you can do; see [ornl.elision.Debugger.Mode] for
 * the various debug modes you can enable.
 */
object Debugger {
  
  /**
   * This enumeration provides the different modes for debug reporting.
   */
  object Mode extends Enumeration {
    /** Enable a tag's debugging output to the console. */
    val ON = Value
    /** Suppress newline after message. STACK and PAUSE affect this. */
    val NONL = Value
    /** Print the class / method for each debugging message. */
    val CLASS = Value
    /** Print a stack trace after the message. */
    val STACK = Value
    /** Pause and wait for user input, if possible, after the message. */
    val PAUSE = Value
    /** Print elapsed time since prior tick.  Does nothing on first tick. */
    val TICK = Value
    /** Save the current time. */
    val START = Value
    /** Compute the time since start and print it.  Does not reset start time. */
    val STOP = Value
    /** Suppress printing of the tag (and the id, if it is positive). */
    val NOTAG = Value
    
    /** Convert a mode into a bit mask. */
    implicit def mode2bit(mode: Mode.Value) = (1 << mode.id)
    
    /** Convert a string into a mode. */
  }
  import Mode._
  
  /**
   * The console to get debugging output.  By default this is a simple print
   * console.
   */
  var console = PrintConsole
  
  /**
   * The modes for each defined tag.  The modes must come from the defined
   * constants in this object [ornl.elision.Debugger.Mode].
   */
  private var _modes = scala.collection.mutable.Map[String,Int]()
  
  /** The last tick time. */
  private var _tick = new Timeable() {
    def reportElapsed() {
      println(getLastTimeString)
    }
  }
  _tick.startTimer
  
  /** The last start time. */
  private var _start = new Timeable() {
    def reportElapsed() {
      println(getLastTimeString)
    }
  }
  _start.startTimer
  
  /**
   * Set the debugging level for the given tag.
   * 
   * @param tag   The tag whose options are to be set.
   * @param verb  The modes, which should be an inclusive OR of the
   *              constants defined in [ornl.elision.Debugger.Mode].  The
   *              constants are automatically converted to integers to make
   *              this possible.
   */
  def setDebugModes(tag: String, mode: Int) {
    _modes(tag) = mode
  }
  
  /**
   * Enable the specified debugging mode(s).  No other modes are modified.
   * This does not automatically enable the tag.
   * 
   * @param tag   The tag whose options are to be set.
   * @param modes The modes, which should be an inclusive OR of the
   *              constants defined in [ornl.elision.Debugger.Mode].  The
   *              constants are automatically converted to integers to make
   *              this possible.
   */
  def enableDebugModes(tag: String, modes: Int) {
    _modes(tag) = _modes.getOrElse(tag, 0) | modes
  }
  
  /**
   * Disable the specified debugging mode(s).  No other modes are modified.
   * This does not automatically disable the tag.
   * 
   * @param tag   The tag whose options are to be set.
   * @param modes The modes, which should be an inclusive OR of the
   *              constants defined in [ornl.elision.Debugger.Mode].  The
   *              constants are automatically converted to integers to make
   *              this possible.
   */
  def disableDebugModes(tag: String, modes: Int) {
    if (modes != 0) _modes(tag) = (_modes.getOrElse(tag, 0) | modes) % modes
  }
  
  /**
   * Perform some action if the specified debugging tag is enabled, and only
   * if the tag is enabled.  Use this by specifying the tag, and then giving
   * the actions in brackets after.
   * 
   * The drawback is that you cannot use an `else`, and if you bind up vars or
   * vals in the block, they are out of scope outside the block.
   * 
   * {{{
   * ifdebug("tim") {
   *   println("Tim is go!")
   * }
   * }}}
   * 
   * @param tag     The debugging tag.
   * @param action  The action to perform.  This is only evaluated iff the tag
   *                is enabled.
   */
  def ifdebug(tag: String = "")(action: =>Unit) {
    if ((_modes.getOrElse(tag,0) & ON) != 0) {
      action
    }
  }
  
  /**
   * Emit a debugging tag based on the enabled modes.
   * 
   * @param message A closure to generate the message to write.
   * @param tags    A tag for this message.
   * @param id      An optional id number.  This is printed if positive.
   */
  def debug(message: => String, tag: String = "", id: Int = -1) {
    // Get the mode for this message.
    val mode = _modes.getOrElse(tag, 0)
    if ((mode & ON) == 0) return
    
    // Maybe print the tag and id.
    if ((mode & NOTAG) == 0) {
      console.panic("DEBUG["+tag+(if (id > 0) "("+id+")" else "")+"] ")
    }
    
    // The message is enabled.  Process any other special before it.
    if ((mode & TICK) != 0) {
      // Get the current clock time.
      _tick.stopTimer
      console.panic("TICK:" + _tick.getLastTimeString + " ")
      _tick.startTimer
    }
    if ((mode & STOP) != 0) {
      _start.stopTimer
      console.panic("INTERVAL:" + _start.getLastTimeString + " ")
    }
    if ((mode & START) != 0) {
      // Start a timer.
      _start.startTimer
    }
    if ((mode & CLASS) != 0) {
      // Assume our caller is the correct context.
      console.panic("["+(new Exception().getStackTrace())(1)+"] ")
    }
    
    // Maybe print a newline.
    // NONL STACK NL?
    //    T T      T
    //    T F      F
    //    F T      T
    //    F F      T
    if ((mode & (NONL|STACK)) != mode2bit(NONL)) {
      // Print a newline.
      console.panicln(message)
    } else {
      // No newline.
      console.panic(message)
    }
    
    // If the user wants a stack trace, give them a stack trace.
    if ((mode & STACK) != 0) {
      // Grab the stack trace and print all its elements.
      val trace = new Exception().getStackTrace()
      console.panicln(console.line("-="))
      console.panicln("STACK TRACE:")
      for (elt <- trace) {
        console.panicln(elt.toString())
      } // Print the stack trace.
      console.panicln(console.line("=-"))
    }
    
    // Handle any cleanup.
    if ((mode & PAUSE) != 0) {
      // Try to force an interactive pause using the current executor.
      console.doPause()
    }
  }
}
