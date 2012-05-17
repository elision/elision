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
  
  /** Whether we are in a comment. */
  private var _inComment = false
  
  /** Whether this is a single line comment. */
  private var _slComment = false
  
  /** The previous character. */
  private var _prior:Char = '\0'
  
  /** Most recent return value from process method. */
  private var _state = false
  
  /** Count adjacent quotation marks for a verbatim block. */
  private var _adjacent = 0
  
  /** Are we in a verbatim block. */
  private var _verb = false
  
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
    _inComment = false
    _prior = '\0'
    _adjacent = 0
    _verb = false
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
      // If we are in a comment, watch for the end of the comment.
      if (_inComment) {
        if (_slComment && ch == '\n') {
          // This is the end of the comment.
          _inComment = false
        } else if (!_slComment && ch == '/' && _prior == '*') {
          // This is the end of the comment.
          _inComment = false
        }
      }
      // Watch for triple quotation marks to toggle verbatim mode.
      else if (!_inEscape && ch == '"') {
        _adjacent += 1
        if (_adjacent == 3) {
          // Toggle verbatim mode.
          _verb = !_verb
          _adjacent = 0
        }
      } else {
        _adjacent = 0      
	      // If we are in verbatim mode, then we just accumulate characters and
	      // wait for the verbatim block to be over.
	      if (_verb) {}
	      // We deal with strings and symbols.  We are only in an escape if we are
	      // in either a symbol or a string.
	      else if (_inEscape) _inEscape = false	// A single character ends the escape.
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
	    	  // and look for the start of strings or symbols.  This is also where
	    	  // we watch for the start of a comment.
	    	  case '/' if _prior == '/' =>
	    	    _inComment = true
	    	    _slComment = true
	    	  case '*' if _prior == '/' =>
	    	    _inComment = true
	    	    _slComment = false
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
      // Save this character as the prior character.
      _prior = ch
    }
    // Check for completeness.
    _state = !(_verb || _inString || _inSymbol || _inComment ||
        _parenDepth > 0 || _braceDepth > 0 || _bracketDepth > 0)
    return _state
  }
}
