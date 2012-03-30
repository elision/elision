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
package sjp.elision.repl

/**
 * Determine the state of the current provided line.
 * 
 * The line state indicates whether the line has properly closed all
 * open parens, braces, brackets, and quotation marks.
 */
class LineState {
  /** Current unclosed parenthesis depth. */
  var parenDepth = 0
  
  /** Current unclosed bracket depth. */
  var bracketDepth = 0
  
  /** Current unclosed brace depth. */
  var braceDepth = 0
  
  /** Whether we are in a quoted string. */
  var inString = false
  
  /** Whether we are in a quoted symbol. */
  var inSymbol = false
  
  /** Whether we are in an escape. */
  var inEscape = false
  
  /** Most recent return value from process method. */
  var state = false
  
  /**
   * Reset this state object.
   */
  def reset() {
    parenDepth = 0
    bracketDepth = 0
    braceDepth = 0
    inString = false
    inSymbol = false
    inEscape = false
  }
  
  /**
   * Process the current line, updating the fields as appropriate.
   * 
   * @param line	The text to process.
   * @return	True iff the line is "complete," and false otherwise.
   */
  def process(line: String): Boolean = {
    // Process each character in the line.
    for (ch <- line) {
      // We deal with strings and symbols first.  We are only even in an
      // escape iff we are in either a symbol or a string.
    	if (inEscape) inEscape = false	// A single character ends the escape.
    	else if (inString) ch match {
    	  case '\\' => inEscape = true	// Start of an escape.
    	  case '"' => inString = false	// End of the string.
    	  case _ =>
    	} else if (inSymbol) ch match {
    	  case '\\' => inEscape = true	// Start of an escape.
    	  case '`' => inSymbol = false	// End of symbol.
    	  case _ => 
    	} else ch match {
    	  // We are not in a string or symbol, so we match parens and such now,
    	  // and look for the start of strings or symbols.
    	  case '(' => parenDepth += 1
    	  case ')' =>
    	    parenDepth -= 1
    	    if (parenDepth < 0) {
    	      return true
    	    }
    	  case '{' => braceDepth += 1
    	  case '}' =>
    	    braceDepth -= 1
    	    if (braceDepth < 0) {
    	      return true
    	    }
    	  case '[' => bracketDepth += 1
    	  case ']' =>
    	    bracketDepth -= 1
    	    if (bracketDepth < 0) {
    	      return true
    	    }
    	  case '"' => inString = true
    	  case '`' => inSymbol = true
    	  case _ =>
    	}
    }
    // Check for completeness.
    state = !(inString || inSymbol || parenDepth > 0 || braceDepth > 0 ||
        bracketDepth > 0)
    return state
  }
}
