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

import scala.actors.Actor
import scala.actors.Actor._

/**
 * A simple console that uses `print` to write to the standard output.
 */
object PrintConsole extends Console {
  def write(text: String) { print(text) }
}

/**
 * Provide communication with a console.  By default this uses the standard
 * output.  To modify this, override the `send` method.
 */
trait Console {
  
  /** Whether to suppress most output. */
  private var _quiet = 0
  
  /** The end of line character.  TODO This should be set dynamically. */
  private final val _ENDL = "\n"
  
  /**
   * Specify whether to be quiet.  How quiet it determined by levels.
   * * 0 : all output
   * * 1 : suppress most output except warnings, errors, and requested
   * * 2 : suppress warnings but not errors or requested output
   * * 3 : suppress warnings and errors, but not requested output
   * * 4 : suppress everything
   * 
   * @param suppress	The quiet level.
   */
  def quiet_=(level: Int) {
    _quiet = level
  }
  
  /**
   * See if quiet is enabled.
   */
  def quiet = _quiet
  
  /**
   * Send the given text to the appropriate destination.  This is the method
   * that controls where output goes, and which must be implemented.  *You
   * should not use this method.*  You want `send`, `sendln`, `warn` or
   * `error`, instead.
   * 
   * @param text  The text to send.
   */
  protected def write(text: String): Unit
  
  /**
   * Write followed by a newline.  This method is private, since there is no
   * use for it outside this class.
   * 
   * @param text  The text to send.
   */
  private def writeln(text: String) { write(text) ; write(_ENDL) }
  
  /**
   * Write a message, unless quiet is enabled.
   * 
   * @param msg		The message.
   */
  def emit(msg: String) { if (_quiet < 1) write(msg) }
  
  /**
   * Write a message, unless quiet is enabled.
   * 
   * @param msg   The message.
   */
  def emitln(msg: String) { if (_quiet < 1) writeln(msg) }
  
  /**
   * Emit a warning message, with the WARNING prefix.
   *
   * @param msg   The message.
   */
  def warn(msg: String) { if (_quiet < 2) writeln("WARNING: " + msg) }
  
  /**
   * Emit an error message, with the ERROR prefix.
   * 
   * @param msg		The message.
   */
  def error(msg: String) { if (_quiet < 3) writeln("ERROR: " + msg) }
  
  /**
   * Send explicitly requested output.
   * 
   * @param text  The text to send.
   */
  def send(text: String) { if (_quiet < 4) write(text) }
  
  /**
   * Send explicitly requested output, followed by a newline.
   * 
   * @param text  The text to send.
   */
  def sendln(text: String) { if (_quiet < 4) writeln(text) }
}
