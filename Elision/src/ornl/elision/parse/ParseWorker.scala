/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2014 by Stacy Prowell (sprowell@gmail.com).
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

import java.util.Arrays
import scala.io.Source
import java.io.InputStream
import java.io.Reader
import java.io.InputStreamReader
import ornl.elision.util.Loc
import ornl.elision.util.ElisionException

/**
 * Raise a parse exception.
 * @param name    The source name.
 * @param line    The line number.
 * @param column  The column number.
 * @param msg     The human-readable message.
 */
class ParseException(loc: Loc, msg: String)
extends ElisionException(loc, msg) {}

/**
 * Provide parsing primitives to support recursive descent parsing from a
 * stream.
 * 
 * @param name    The name of the source, such as a filename.
 * @param reader  A reader providing characters.
 * @author Stacy Prowell (sprowell@gmail.com)
 */
class ParseWorker(name: String, reader: Reader) {
  if (reader == null) {
    throw new NullPointerException("The reader is null.")
  }

  /** The size of chunk to read at one go. */
  val BLOCKSIZE = 1024
  
  /** The end of file indicator. */
  val EOF = (-1).toChar
  
  /** True if we have consumed up to the end of file. */
  private var atEof = false
  
  /** Line number of next character to be consumed. */
  private var thisLine = 1
  
  /** Column number of next character to be consumed. */
  private var thisColumn = 1
  
  /** The number of times we look ahead prior to consuming. */
  private var lookcount = 0
  
  /** The number of times we try to consume after the end of file. */
  private var eofcount = 0
  
  /**
   * We keep around at most two blocks.  This just tells us which block we
   * are currently in.
   */
  private var block = 0
  
  /**
   * The pointer to the next character to consume.  This is relative to the
   * current block.
   */
  private var next = 0
  
  /** The blocks themselves.  These are just simple arrays. */
  private val blocks = Array.ofDim[Char](2, BLOCKSIZE)
  
  /**
   * Default comment parser.  This handles C and C++ style comments.  It is
   * easily extended.  Write your own and if you don't find a comment, invoke
   * this method.
   * @return  True if a comment is parsed, and false if no comment is found
   *          at the current position.
   */
  def parseComment(): Boolean = {
    if (peekAndConsume("//")) {
      while (!isAtEof && peek() != '\n') consume()
      consume()
      true
    } else if (peekAndConsume("/*")) {
      while (! peekAndConsume("*/")) {
        consume()
        if (isAtEof) {
          fail("The end of file was reached while a multi-line " +
              "comment was open.  Did you forget to terminate a comment?")
        }
      }
      true
    } else {
      false
    }
  }
  
  /**
   * A method to parse and consume a comment.  This method must return true
   * if a comment is successfully consumed, and false if no comment is found.
   * It should fail if a comment is found but cannot be parsed.  The default
   * is provided by the parseComment method in this class.
   */
  var comment: () => Boolean = parseComment
  
  /**
   * Peek ahead and get the next character that will be consumed.
   * @return  The next character to be consumed, or possibly the EOF.
   */
  def peek() = look(0)
  
  /**
   * Peek ahead and get the next few characters that will be consumed.  If
   * the end of file is encountered, then these will be EOF.  The look ahead
   * must be less than the BLOCKSIZE.  A lookahead of zero is permitted, but
   * is useless.
   * @param n The number of characters to look ahead.
   * @return  A string composed of the characters.
   */
  def peek(n: Int) = {
    if (n >= BLOCKSIZE) {
      throw new IllegalArgumentException("Lookahead must be less than " +
          BLOCKSIZE + " characters, but an attempt was made to read " +
          n + " characters ahead.")
    }
    val buf = new StringBuilder()
    // This replaces the single line using a range as ranges seem to be slow
    // at present.
    var index = 0
    while (index < n) {
      buf.append(look(index))
      index += 1
    } // Grab the next n characters.
    buf.toString()
  }
  
  /**
   * Peek ahead and see if the next few characters to be read match the
   * given string.  The string must be shorter than the BLOCKSIZE.  The
   * empty string is permitted, but is useless.
   * @param s The string.
   * @return  True if the next characters to be consumed are the string.
   */
  def peek(s: String): Boolean = {
    if (s.length >= BLOCKSIZE) {
      throw new IllegalArgumentException("Lookahead must be less than " +
          BLOCKSIZE + " characters, but an attempt was made to read " +
          s.length + " characters ahead.")
    }
    // This replaces the shorter code using a range as ranges appear to be
    // slow.
    var index = 0
    while (index < s.length) {
      if (s(index) != look(index)) return false
      index += 1
    } // Check the string against the lookahead.
    return true
  }
  
  /**
   * Consume and return a single character.
   * @return  The character that is consumed.
   */
  def consume(): Char = {
    val ch = blocks(block)(next)
    consume(1)
    ch
  }
  
  /**
   * Consume and discard characters.  The number must be less than the
   * BLOCKSIZE.  Zero is permitted, but is useless.  All character consumption
   * comes here in order to correctly track the line and column number.  This
   * is also the only method that sets the end of file indicator.
   * 
   * @param n The number of characters to consume and discard.
   */
  def consume(n: Int) {
    if (n >= BLOCKSIZE) {
      throw new IllegalArgumentException("At most " +
          BLOCKSIZE + " characters can be consumed at once, but an " +
          "attempt was made to consume " + n + " characters at once.")
    }
    lookcount = 0
    
    // We cannot simply advance next by n because we have to track the line
    // and column indices.  Thus we have to iterate.
    if (atEof) {
      eofcount += 1
      if (eofcount > 200) {
        fail("Forward progress in parser stalled at end of file.  " +
            "Possible internal error.")
      }
    }
    // Replaced the use of a range since ranges appear to be slow.
    var count = 0
    while (count < n) {
      if (blocks(block)(next) == EOF) {
        atEof = true
        return
      }
      thisColumn += 1
      if (blocks(block)(next) == '\n') {
        thisLine += 1
        thisColumn = 1
      }
      next += 1
      if (next >= BLOCKSIZE) {
        next -= BLOCKSIZE
        block ^= 1
        readOther()
      }
      count += 1
    } // Consume the requested number of characters.
  }
  
  /**
   * Consume all whitespace starting at the current position, and stopping at
   * the first non-whitespace character, which will be the next character
   * available to consume.
   */
  def consumeWhitespace() {
    while (Character.isWhitespace(peek())) consume()
    while (comment()) {
      while (Character.isWhitespace(peek())) consume()
    } // Consume all comments.
  }
  
  /**
   * Peek ahead at the next few characters to consume and if they are equal
   * to the given string, consume them.  Otherwise leave the stream unchanged.
   * The string must be shorter than the BLOCKSIZE.  The empty string is
   * permitted, but is useless.
   * @param s The string.
   * @return  True if the string matched and the characters were consumed, and
   *          false otherwise.
   */
  def peekAndConsume(s: String) = {
    if (peek(s)) {
      consume(s.length)
      true
    } else {
      false
    }
  }
  
  /**
   * Get the current location.  This is the location of the next character
   * to be consumed.
   * @return  The location.
   */
  def loc = {
    new Loc(name, thisLine, thisColumn, None)
  }
  
  /**
   * Determine if we have consumed all the characters in the stream.
   * @return  True iff all characters have been consumed.
   */
  def isAtEof = atEof
  
  /**
   * Fail the current parse.  This generates a parse exception.
   * @param msg The human-readable message.
   */
  def fail(msg: String) = {
    throw new ParseException(loc, msg)
  }
  
  /**
   * Look ahead to the character at the specified offset.  Lookahead is
   * limited by the currently set block size.
   * @param n  The zero-based offset of the character.
   * @return The requested character.
   */
  private def look(n: Int) = {
    if (n >= BLOCKSIZE) {
      throw new IllegalArgumentException("Lookahead must be less than " +
          BLOCKSIZE + " characters, but an attempt was made to read " +
          n + " characters ahead.")
    }
    lookcount += 1
    if (lookcount > 200) {
      fail("Forward progress in parser has stalled.  Possible internal error.")
    }
    if (n + next < BLOCKSIZE) {
      blocks(block)(n + next)
    } else {
      blocks(block ^ 1)(n + next - BLOCKSIZE)
    }
  }
  
  /**
   * Read an populate the other block of this buffer.  Note: This is the only
   * method that manipulates the actual stream!
   */
  private def readOther() {
    var count = reader.read(blocks(block ^ 1))
    if (count < 0) {
      count = 0
    }
    if (count < BLOCKSIZE) {
      Arrays.fill(blocks(block ^ 1), count, BLOCKSIZE-1, EOF)
    }
  }
  
  // Immediately populate both blocks.  Note that we move to block one and then
  // read the other block (block zero).  Then we move to block zero and read
  // the other block (block one).  This isn't really important - the blocks
  // have no special ordering - but it seems "nicer."
  block = 1
  readOther()
  block = 0
  readOther()
}
