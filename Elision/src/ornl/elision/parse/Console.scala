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
package ornl.elision.parse

import scala.actors.Actor
import scala.actors.Actor._

/**
 * Provide communication with a console.  By default this uses the standard
 * output.  To modify this, override the `send` method.
 */
trait Console {
  
  /** Whether to suppress most output. */
  private var _quiet = false
  
  /** The end of line character.  TODO This should be set dynamically. */
  private final val _ENDL = "\n"
  
  /**
   * Specify whether to be quiet.
   * 
   * @param suppress	If true, suppress most output.
   */
  def quiet_=(suppress: Boolean) {
    _quiet = suppress
  }
  
  /**
   * Send the given text to the appropriate destination.  If you want to change
   * where output goes, override this method.
   * 
   * @param text	The text to send.
   */
  def send(text: String) { print(text) }
  
  /**
   * Send the given text to the appropriate destination, followed by the end
   * of line.  This method uses `send`, so you should not need to override it.
   * 
   * @param text	The text to send.
   */
  def sendln(text: String) { send(text) ; send(_ENDL) }
  
  /**
   * Write a message, unless quiet is enabled.
   * 
   * @param msg		The message.
   */
  def emitln(msg: String) { if (!_quiet) sendln(msg) }
  
  /**
   * Emit an error message, with the ERROR prefix.
   * 
   * @param msg		The message.
   */
  def error(msg: String) { sendln("ERROR: " + msg) }
  
  /**
   * Emit a warning message, with the WARNING prefix.
   *
   * @param msg		The message.
   */
  def warn(msg: String) { sendln("WARNING: " + msg) }
}
