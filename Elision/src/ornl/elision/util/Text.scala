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
 * Text manipulation routines.  The routines here assume fixed width font and
 * fixed width output.  Wrapping is accomplished via the greedy algorithm.
 * In the future someone might want to see about implementing Knuth's algorithm
 * that minimizes the square of the excess space, as this is prettier.
 * 
 * To use this make an instance, configure it, and start adding text.
 */
class Text {
  
  /** Access to system properties. */
  private val _prop = new scala.sys.SystemProperties
  
  /** The end of line character. */
  private final val _nl = _prop("line.separator")

  /** A buffer. */
  private var _buf = new StringBuilder
  
  /** Break lines at these characters.  Consume them. */
  private val _breakAt = " \t"

  /**
   * Add text to the end of the buffer.
   * 
   * @param text  The text to add.
   * @return  This object.
   */
  def add(text: String): Text = {
    _buf.append(text)
    this
  }

  /**
   * Add text to the end of the buffer, followed by a newline.
   * 
   * @param text  The text to add.
   * @return  This object.
   */
  def addln(text: String): Text = {
    _buf.append(text)
    _buf.append(_nl)
    this
  }
  
  /**
   * Clear the buffer.
   * 
   * @return  This object.
   */
  def clear: Text = {
    _buf.clear
    this
  }
  
  /**
   * Perform word wrapping on the stored text, and return it as a list of
   * lines.  This uses the greedy algorithm, and breaks lines "at" white
   * space.  A line break consumes the whitespace at that position, so that
   * multiple spaces are consumed (for instance, if one is using two spaces
   * to break sentences.)
   * 
   * @param width   The line width.  No more than this number of characters
   *                will appear on a single line.  This value must be positive.
   * @param pad     If true, each line is padded with whitespace to the
   *                required width.  The default is not to do this.
   * @return  The list of lines.
   */
  def wrap(width: Int, pad: Boolean = false): List[String] = {
    require(width > 0)
    var ch: Char = '\0'         // Current character.
    var lines = List[String]()  // Accumulated lines.
    var index = 0               // Index into the buffer.
    var length = 0              // Current line length.
    var point = -1              // Rightmost break point found.
    var linestart = 0           // Buffer position of line start.
    var consumingws = false     // Are we consuming whitespace?
    
    // Add a line to the list.  This local function exists to handle
    // padding, if requested.
    def addLine(text: String) {
      lines :+= (if (pad) {
        text + (" "*(width - text.length))
      } else {
        text
      })
    }
    
    // Local function to accumulate a line.
    def nextLine {
      // We have a full line.  If we have a breakpoint, then the new line
      // runs from the line start to the current point.  If not, then we
      // must simply break right here, no matter what.
      if (point < 0) {
        // No break point was found.  Break the line right here.
        addLine(_buf.substring(linestart, index))
      } else {
        // The line is the text between line start and the point.
        addLine(_buf.substring(linestart, point))
        index = point
      }
      // We have the new line.  Move past any whitespace to get the start of
      // the next line, *unless* we broke the line because of a newline.
      if (ch == '\n') {
        // Advance past the newline.
        index += 1
      } else {
        while (index < _buf.length && _breakAt.contains(_buf(index))) {
          index += 1
        } // Move to start of next line.
      }
      linestart = index
      point = -1
      length = 0
    }
    
    // Consider the content, character by character.
    while (index < _buf.length) {
      ch = _buf(index)
      // If the current character is a newline, break right now.  We check this
      // before we check the length; otherwise we would prematurely push the
      // newline to the start of the next line.
      while (ch == '\n') {
        point = index
        nextLine
        if (index >= _buf.length) return lines
        ch = _buf(index)
      } // Consume newlines.
      
      if (_breakAt.contains(ch)) {
        // We have a whitespace character, but it may be one of several in a
        // row, and we want to break at the first one.  So skip subsequent
        // characters.  Note that we let this setting persist across lines.
        if (!consumingws) {
          // Yes, this is a point at which we can break the line.
          point = index
          consumingws = true
        }
      } else {
        // This is not whitespace.  If we were consuming whitespace characters,
        // we aren't any more.
        consumingws = false
      }
      
      // If we have a full line, break now.
      if (length >= width) {
        nextLine
        if (index >= _buf.length) {
          // We are done.
          return lines
        }
      }
      ch = _buf(index)
      
      // Advance the index and length.
      length += 1
      index += 1
    } // Process the buffer.
    
    // Accumulate whatever is left as the final line.
    point = -1
    nextLine
    return lines
  }
}
