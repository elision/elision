/*======================================================================
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 * The Elision Term Rewriter
 * 
 * Copyright (c) 2012 by UT-Battelle, LLC.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 
 * Collection of administrative costs for redistribution of the source code or
 * binary form is allowed. However, collection of a royalty or other fee in excess
 * of good faith amount for cost recovery for such redistribution is prohibited.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER, THE DOE, OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
======================================================================*/
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
package ornl.elision.repl

/**
 * Determine the state of the current provided line.
 * 
 * == Purpose ==
 * This is used to allow data entry at the prompt to span multiple lines.
 * The number of open parentheses, brackets, and braces is tracked, along
 * with whether a string is open.
 * 
 * The line state indicates whether the line has properly closed all
 * open parens, braces, brackets, and quotation marks.
 */
class LineState {
  /** Current unclosed parenthesis depth. */
  private var _parenDepth = 0
  
  /** Current unclosed bracket depth. */
  private var _bracketDepth = 0
  
  /** Current unclosed brace depth. */
  private var _braceDepth = 0
  
  /** Whether we are in a quoted string. */
  private var _inString = false
  
  /** Whether we are in a quoted symbol. */
  private var _inSymbol = false
  
  /** Whether we are in an escape. */
  private var _inEscape = false
  
  /** Most recent return value from process method. */
  private var _state = false
  
  /**
   * Reset this state object.
   */
  def reset() {
    _parenDepth = 0
    _bracketDepth = 0
    _braceDepth = 0
    _inString = false
    _inSymbol = false
    _inEscape = false
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
    	if (_inEscape) _inEscape = false	// A single character ends the escape.
    	else if (_inString) ch match {
    	  case '\\' => _inEscape = true	// Start of an escape.
    	  case '"' => _inString = false	// End of the string.
    	  case _ =>
    	} else if (_inSymbol) ch match {
    	  case '\\' => _inEscape = true	// Start of an escape.
    	  case '`' => _inSymbol = false	// End of symbol.
    	  case _ => 
    	} else ch match {
    	  // We are not in a string or symbol, so we match parens and such now,
    	  // and look for the start of strings or symbols.
    	  case '(' => _parenDepth += 1
    	  case ')' =>
    	    _parenDepth -= 1
    	    if (_parenDepth < 0) {
    	      return true
    	    }
    	  case '{' => _braceDepth += 1
    	  case '}' =>
    	    _braceDepth -= 1
    	    if (_braceDepth < 0) {
    	      return true
    	    }
    	  case '[' => _bracketDepth += 1
    	  case ']' =>
    	    _bracketDepth -= 1
    	    if (_bracketDepth < 0) {
    	      return true
    	    }
    	  case '"' => _inString = true
    	  case '`' => _inSymbol = true
    	  case _ =>
    	}
    }
    // Check for completeness.
    _state = !(_inString || _inSymbol || _parenDepth > 0 || _braceDepth > 0 ||
        _bracketDepth > 0)
    return _state
  }
}
