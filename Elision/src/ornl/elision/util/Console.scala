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

import org.fusesource.jansi.Ansi
import org.fusesource.jansi.AnsiConsole

/**
 * A simple console that uses `print` to write to the standard output.
 */
object PrintConsole extends Console {
  def write(text: String) { 
    System.out.print(text) 
  }
}

/**
 * A simple console that uses `print` to write to to AnsiConsole's standard
 * output, which is able to interpret ANSI escape sequences.
 */
object AnsiPrintConsole extends Console {
  private val _prop = new scala.sys.SystemProperties
  private val _ENDL = _prop("line.separator")
  
  private val resetColorEsc = "\u001B[0m"
  private var restoreColorEsc = resetColorEsc
  
  def write(text: String) { 
    // Only use Jansi's AnsiConsole on Windows. Assume that consoles in other systems
    // can handle unix-style ansi coloring just fine.
    if(isWindows) {
        AnsiConsole.out.print(text) 
    }
    else {
        Console.out.print(text)
    }
    
    
    // save the last coloring escape sequence.
    val escIndex = text.lastIndexOf("\u001B[")
    if(escIndex >= 0) {
      var escSeq = text.substring(escIndex)
      val escEnd = escSeq.indexOf("m")
      if(escEnd > 0) {
        restoreColorEsc = escSeq.substring(0, escEnd+1)
      }
    }
  }
  
  
  /**
   * Pauses the AnsiConsole correctly
   * and preserves color formatting.
   * 
   * @return  true if the user doesn't enter 'q'.
   */
  private def ansiPause() : Boolean = {
    write(resetColorEsc + "--More--" + restoreColorEsc)
    
    if(isWindows) {
      AnsiConsole.out.flush
    }
    else {
      Console.out.flush();
    }
    
    val ch = scala.io.Source.stdin.reader.read.toChar
    
    if (ch != '\n') write(_ENDL)
    ch != 'q'
  }
  
  /**
   * Emits text to the AnsiConsole while also checking the page
   * size. It does no line wrapping because the SyntaxFormatter already
   * did this for us.
   * 
   * @param text  The text to emit, already processed by a SyntaxFormatter.
   */
  private def ansiWriteText(text: String) {
    if (height <= 0) {
      write(text)
    }
    else {
      // Break the string into lines at the newlines.
      val lines = text.split("\n")
      
      if(lines.length < height) {
        write(text)
      }
      else {
        var _count = 0
        for(line <- lines) {
          write(line + "\n")
          _count += 1
          if(_count >= height) {
            _count = 0
            if(!doPause) {
              return 
            } // if quit
          } // if do page
        } // end for
      } // page case
    } 
    
  }
  
  /**
   * Checks if we're running in Windows.
   * @return true iff we're running in Windows.
   */
  private def isWindows() : Boolean = {
    System.getProperty("os.name").startsWith("Windows")
  }   
  
  
  writeText_=(ansiWriteText)
  pause_=(ansiPause)
}

/**
 * Provide communication with a console.  By default this uses the standard
 * output.  To modify this, override the `write` method.
 * 
 * The console supports "quiet" levels.  These work with the methods to
 * determine what will be written to the console.   
 */
trait Console {
  
  /** Access to system properties. */
  private val _prop = new scala.sys.SystemProperties
  
  /** Whether to suppress most output. */
  private var _quiet = 0
  
  /** Total number of errors. */
  private var _errors = 0
  
  /** Total number of warnings. */
  private var _warnings = 0
  
  /** The end of line character. */
  private final val _ENDL = _prop("line.separator")
  
  /**
   * The default pause operation.  Wait for a user-entered character.
   * 
   * @return  True if output should continue; false if the user wants to
   *          terminate the output.
   */
  def defaultPause(): Boolean = {
    write("--More--")
    val ch = scala.io.Source.stdin.reader.read.toChar
    if (ch != '\n') write(_ENDL)
    ch != 'q'
  }
  
  /** The pause closure to use. */
  private var _pause: () => Boolean = defaultPause _
  
  /** The number of lines to emit before invoking the pager.  Zero to disable. */
  private var _height = 0
  
  /** The number of columns before wrapping is assumed.  Zero to disable. */
  private var _width = 0
  
  /** Current column of output, as best we can tell. */
  private var _column = 0
  
  /** Get the error count. */
  def errors = _errors
  
  /** Get the warning count. */
  def warnings = _warnings
  
  /** Reset the error and warnings counts to zero. */
  def reset = {
    _errors = 0
    _warnings = 0
    this
  }
  
  /**
   * Specify the length of a page of text.  If output (sent at one time)
   * exceeds this, then the pager is invoked.
   * 
   * @param size  The number of lines on the screen, minus one.
   */
  def height_=(size: Int) { _height = size }
  
  /**
   * Set the number of columns of output until wrapping is assumed.
   */
  def width_=(size: Int) { _width = size }
  
  /**
   * Obtains the length of a page of text.  If output (sent at one time)
   * exceeds this, then the pager is invoked.
   * @return  The number of lines on the screen, minus one.
   */
  def height : Int = { _height }
  
  /**
   * Obtains the number of columns of output until wrapping is assumed.
   * @return  The number of columns.
   */
  def width : Int = { _width }
  
  /**
   * Specify a closure to invoke when a screen has filled.  By default this is
   * the pager specified by `defaultPause`.  The pager should return a Boolean
   * value that is true to continue, and false if output should be terminated.
   * 
   * @param pager The pager to invoke.
   */
  def pause_=(pager: () => Boolean) { _pause = pager }
  
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
   * Get a "line" string based on the current width.
   * 
   * @param ch  A string to repeat to make up the line.  By default this
   *            is `-`.  As many full instances of this string will be
   *            concatenated as the line allows.
   * @return  The requested line.
   */
  def line(ch: String = "-") = ch*((if (_width > 0) _width else 80)/ch.length)
  
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
   * Private method to emit text while checking the page size.
   * 
   * @param text  The text to emit.
   */
  private def _defaultWriteText(text: String) {
    if (_height <= 0) write(text)
    else {
      // Break the string into lines, first at the newlines.
      val linesA = text.split("\n")
      var lines = Seq[String]()
      if (_width > 0) {
        // Now further break the lines at their wrap points.
        for (line <- linesA) {
          var remain = line
          if (remain.length == 0) lines :+= remain
          else {
            while (remain.length > _width) {
              val pair = remain.splitAt(_width)
              lines :+= pair._1
              remain = pair._2
            } // Break the lines at wrap points.
            if (remain.length > 0) lines :+= remain
          }
        } // Divide up all the lines.
      } else {
        lines = linesA
      }
      // If the lines fit you must emit.
      if (lines.length < _height) write(text)
      else {
        // The lines do not fit.  Emit lines, run the pager, and then continue.
        var count = 0
        for (line <- lines) {
          write(line + "\n")
          count += 1
          if (count >= _height) {
            count = 0
            if (!_pause()) return
          }
        } // Emit all lines.
      } // Not a screenful case.
    } // Not paging case.
  }
  
  /** The writeText closure to use. */
  private var _writeText : (String) => Unit = _defaultWriteText _
  
  /** Invokes the closure used to write text to this Console. */
  private def writeText(text : String) = { _writeText(text) }
  
  /**
   * Specify a closure to invoke when text is written to the Console.  
   * By default this is the writer specified by `_defaultWriteText`. 
   * The writer should accept a String of text to be printed as its parameter.
   * 
   * @param writer  The writer to invoke.
   */
  def writeText_=(writer : (String) => Unit) = {
    _writeText = writer
  }
  
  /**
   * Perform whatever "pause" is configured for this console, and return
   * whether the user elected to continue.
   * 
   * @return  True if the user wants to continue; false if not.
   */
  def doPause() = _pause()
  
  /**
   * Write followed by a newline.  This method is private, since there is no
   * use for it outside this class.
   * 
   * @param text  The text to send.
   */
  private def writeln(text: String) { writeText(text + _ENDL) }
  
  /**
   * Write a message.  The message is only written if the quiet level is
   * zero.
   * 
   * @param msg		The message.
   */
  def emit(msg: String) { if (_quiet < 1) writeText(msg) }
  
  /**
   * Write a message.  The message is only written if the quiet level is
   * zero.
   * 
   * @param msg   The message.
   */
  def emitln(msg: String) { if (_quiet < 1) writeln(msg) }
  
  /**
   * Write a message.  The message is only written if the quiet level is
   * zero.
   * 
   * @param form  The format string.
   * @param args  The arguments required by the format string.
   */
  def emitf(form: String, args: Any*) {
    if (_quiet < 1) {
      writeln(form.format(args:_*))
    }
  }
  
  /**
   * Emit a warning message, with the WARNING prefix.  This is only done
   * if the quiet level is one or lower.  Suppressed warnings are still
   * counted.
   *
   * @param msg   The message.
   */
  def warn(msg: String) {
    _warnings += 1
    if (_quiet < 2) writeln("WARNING: " + msg)
  }
  
  /**
   * Emit a warning message, with the WARNING prefix.  This is only done
   * if the quiet level is one or lower.  A newline is always written after
   * the message.  Suppressed warnings are still counted.
   *
   * @param form  The format string.
   * @param args  The arguments required by the format string.
   */
  def warnf(form: String, args: Any*) {
    if (_quiet < 2) {
      warn(form.format(args:_*))
    }
  }
  
  /**
   * Emit an error message, with the ERROR prefix.  This is only done if
   * the quiet level is two or lower.  Suppressed errors are still counted.
   * 
   * @param msg		The message.
   */
  def error(msg: String) {
    _errors += 1
    if (_quiet < 3) writeln("ERROR: " + msg)
  }
  
  /**
   * Emit an error message, with the ERROR prefix.  This is only done if
   * the quiet level is two or lower.  A newline is always written after
   * the message.  Suppressed errors are still counted.
   *
   * @param form  The format string.
   * @param args  The arguments required by the format string.
   */
  def errorf(form: String, args: Any*) {
    if (_quiet < 3) {
      error(form.format(args:_*))
    }
  }
  
  /**
   * Emit a warning message, with the WARNING prefix.  This is only done
   * if the quiet level is one or lower.  Suppressed warnings are still
   * counted.
   *
   * @param loc   A location relevant to this message.
   * @param msg   The message.
   */
  def warn(loc: Loc, msg: String) {
    _warnings += 1
    if (_quiet < 2) writeln("WARNING"+loc.toShortString+": "+msg)
  }
  
  /**
   * Emit a warning message, with the WARNING prefix.  This is only done
   * if the quiet level is one or lower.  A newline is always written after
   * the message.  Suppressed warnings are still counted.
   *
   * @param loc   A location relevant to this message.
   * @param form  The format string.
   * @param args  The arguments required by the format string.
   */
  def warnf(loc: Loc, form: String, args: Any*) {
    if (_quiet < 2) {
      warn(form.format(args:_*))
    }
  }
  
  /**
   * Emit an error message, with the ERROR prefix.  This is only done if
   * the quiet level is two or lower.  Suppressed errors are still counted.
   * 
   * @param loc   A location relevant to this message.
   * @param msg   The message.
   */
  def error(loc: Loc, msg: String) {
    _errors += 1
    if (_quiet < 3) writeln("ERROR"+loc.toShortString+": "+msg)
  }
  
  /**
   * Emit an error message, with the ERROR prefix.  This is only done if
   * the quiet level is two or lower.  A newline is always written after
   * the message.  Suppressed errors are still counted.
   *
   * @param loc   A location relevant to this message.
   * @param form  The format string.
   * @param args  The arguments required by the format string.
   */
  def errorf(loc: Loc, form: String, args: Any*) {
    if (_quiet < 3) {
      error(form.format(args:_*))
    }
  }
  
  /**
   * Send explicitly requested output.  This is only done if the quiet
   * level is three or lower.
   * 
   * @param text  The text to send.
   */
  def send(text: String) { if (_quiet < 4) writeText(text) }
  
  /**
   * Send explicitly requested output, followed by a newline.  This is
   * only done if the quiet level is three or lower.
   * 
   * @param text  The text to send.
   */
  def sendln(text: String) { if (_quiet < 4) writeln(text) }
  
  /**
   * Send explicitly requested output, followed by a newline.  This is
   * only done if the quiet level is three or lower.  A newline is always
   * written after the message.
   *
   * @param form  The format string.
   * @param args  The arguments required by the format string.
   */
  def sendf(form: String, args: Any*) {
    if (_quiet < 4) {
      sendln(form.format(args:_*))
    }
  }
  
  /**
   * Send output no matter what the quiet level.  Please do not abuse this.
   * 
   * @param text  The text to send.
   */
  def panic(text: String) { writeText(text) }
  
  /**
   * Send output no matter what the quiet level.  Please do not abuse this.
   * 
   * @param text  The text to send.
   */
  def panicln(text: String) { writeln(text) }
  
  /**
   * Send output no matter what the quiet level.  Please do not abuse this.
   * 
   * @param form  The format string.
   * @param args  The arguments required by the format string.
   */
  def panicf(form: String, args:  Any*) {
    panic(form.format(args:_*))
  }
}
