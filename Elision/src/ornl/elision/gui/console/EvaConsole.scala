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

package ornl.elision.gui.console

import scala.swing._
import scala.concurrent.ops._
import scala.actors.Actor
import sys.process._
import java.io._

import ornl.elision.actors.ReplActor
import ornl.elision.gui.EvaConfig
import ornl.elision.gui.GUIActor
import ornl.elision.gui.GUIColors
import ornl.elision.gui.mainGUI
import ornl.elision.gui.copypaste._
import ornl.elision.gui.elision.EliReplThread
import ornl.elision.gui.elision.EliRegexes
import ornl.elision.gui.elision.EliTreeVisLevel
import ornl.elision.syntax
import ornl.elision.util.Console

/** 
 * An EditorPane-based Elision Console used by Eva. This should eventually  
 * be registered as the console for Eva's Elision REPL instance.
 */
class EvaConsole(val console : ConsolePanel) extends EditorPane with Actor with Console {
  /** The border width. */
  val inset = 3
  
  /** flag for applying Elision formatting to output */
  var applyFormatting = false
  
  /** The position of the last output character in textArea. */
  var _anchorPos : Int = 0
  
  /** The contents of textArea since the last time it received output. */
  var _readOnlyOutput = ""
  
  def write(_newTxt: String) { 
    this ! _newTxt
  }
  
  /** A closure for pausing the repl. */
  def evaPause(): Boolean = {
    write("--More--")
    ReplActor.waitForGUI("gui pager has paused.")
    true
  }
  pause_=(evaPause)
  
  
  listenTo(this.mouse.clicks)
  listenTo(this.keys)
  reactions += {
      // Right-click menu
      case e : swing.event.MouseClicked =>
        val btn = e.peer.getButton
        if(btn == java.awt.event.MouseEvent.BUTTON3) {
          val copypasta = new TextRClickMenu(this)
          copypasta.show(console.peer, e.point.x, e.point.y)
        }
      
      case e : swing.event.KeyTyped =>
        // pressing enter sends the current input text to the Elision repl.
        if(e.char == '\n') {
          sendToInputStream
        }
        
        // backspace should not got past the prompt string.
        if(e.char == '\b' && getLength < _anchorPos) {
          text = """<div style="font-family:Lucida Console;font-size:12pt">""" + _readOnlyOutput
          _caretLeftBound
        }
        
      case e : swing.event.KeyPressed => 
        // The up/down arrow keys can be used to cycle through the input history.
        if(e.key == swing.event.Key.Up) {
          getHistory(-1)
        }
        if(e.key == swing.event.Key.Down) {
          getHistory(1)
        }
        
        // Prevent the user from moving the caret past the prompt string.
        if(e.key == swing.event.Key.Left) {
          _caretLeftBound
        }
        
        // keyboard menu shortcuts
        if(e.key == swing.event.Key.F1)
          GUIActor ! "helpOpen"
        if(e.key == swing.event.Key.Escape)
          GUIActor ! ("enableRuleMaker", false)
              
      
      case e : swing.event.KeyReleased => 
        // make sure that when the user uses up/down to cycle through the input history that the caret is 
        // moved to the end of the text area string at the end.
        if(e.key == swing.event.Key.Up) {
          _sendCaretToEnd
        }
        if(e.key == swing.event.Key.Down) {
          _sendCaretToEnd
        }
        
        // home moves the caret to the beginning of the allowed input area.
        if(e.key == swing.event.Key.Home || e.key == swing.event.Key.Left) {
          _caretLeftBound
        }
        caret.visible = true
  }
  
  
  /** Sets the caret's position to the beginning of the allowed input area. */
  def _caretLeftBound : Unit = {
    caret.position = math.max(_anchorPos, caret.position)
  }
  
  /** Sets the caret's position to the end of the allowed input area. */
  def _sendCaretToEnd : Unit = {
    caret.position = getLength
  }
  
    
    
  def act() = {
    loop {
      react {
        case _newTxt : String =>
          // Received a new chunk of output. Format it and append it to past output.
          var newTxt = _newTxt
          
          // Inject our new text with HTML tags for formatting.
          if(applyFormatting) 
            newTxt = ConsolePanel.formatter.htmlFormat(newTxt, ConsolePanel.maxCols)
          else 
            newTxt = ConsolePanel.formatter.minHtmlFormat(newTxt, ConsolePanel.maxCols) 
          
          // append our processed new text to our previous output.
          _updateReadOnlyText(newTxt)
          _enforceMaxLines
          
          // apply a constant-width font to our entire EditorPane.
          text = """<div style="font-family:Lucida Console;font-size:12pt">""" + _readOnlyOutput
          
          // update our anchor point and caret position.
          _anchorPos = getLength
          caret.position = _anchorPos

        case _ =>
      } // endactorreact
    } // endactorloop
  }
  
  /**
   * If the current number of lines in the text area is greater than maxlines, 
   * it erases the oldest lines until the number of lines until it is no longer greater
   * than maxLines.
   */
  def _enforceMaxLines : Unit = {
    if(EvaConfig.replMaxLines < ConsolePanel.infiniteMaxLines) return
    while(lineCount(_readOnlyOutput) > EvaConfig.replMaxLines) { 
      if(!_chompFirstLine) 
        return
    } // endwhile
  }
  
  /** 
   * Counts the number of lines in the EditorPane by counting the number of <br/> tags. 
   * @param txt  The string we are counting new line tags in.
   * @return    The number of new line tags in txt.
   */
  def lineCount(txt : String) : Int = {
    syntax.SyntaxFormatter.htmlNewLineRegex.findAllIn(txt).size
  }
  
  /** 
   * Removes the first <br/> tag and everything before it in the EditorPane's readOnlyOutput. 
   * @return    true if the readOnlyOutput currently has at least 1 new line tag. Otherwise false.
   */
  def _chompFirstLine() : Boolean = {
    syntax.SyntaxFormatter.htmlNewLineRegex.findFirstMatchIn(_readOnlyOutput) match {
      case Some(myMatch : scala.util.matching.Regex.Match) =>
        _readOnlyOutput = _readOnlyOutput.drop(myMatch.end)
        true
      case _ => 
        false
    } // endmatch
  }
  
  
  /** 
   * Appends new already-processed text to our readOnlyOutput. 
   * @param newTxt    The text being appended to readOnlyOutput.
   */
  def _updateReadOnlyText(newTxt : String) : Unit = {
    _readOnlyOutput += newTxt
  }
  
  
  /** gets the text of the EditorPane */
  def getText() : String = {
    val doc = peer.getDocument
    doc.getText(0,doc.getLength)
  }
  
  
  /** gets the length of the EditorPane's text. */
  def getLength() : Int = {
    peer.getDocument.getLength
  }
  
  
  /**
   * Cycles one step up or down through the input history and sets the current input text to the obtained historical input.
   * @param direction    An integer in range [-1,1] telling the history which direction to cycle. 1 means go back 1, -1 means go forward 1.
   */
  def getHistory(direction : Int) : Unit = {
    val i = if(direction != 0) direction/math.abs(direction) else 0// normalize i so that we can only increment by magnitudes of 1 at a time.
    
    var newInput = ""
    
    // wait for the actual REPL to return with the history entry.
    GUIActor.waitOnREPL(() => 
      GUIActor ! ("Repl",("getHistory", direction)), 
      "requesting history from REPL.")
    
    ConsolePanel.reGetHistory match {
      case None => 
      case str : String =>
        newInput = str
    }
    
    // replace the input string with the history entry.
    text = """<div style="font-family:Lucida Console;font-size:12pt">""" + _readOnlyOutput + newInput
    caret.visible = false
  }
  
  
  
  /**
   * This sends whatever is the current input text to the Repl's actor so that 
   * it can be processed by the Repl.
   * The input is also saved into the input history.
   */
  def sendToInputStream : Unit = {
    import java.lang.System

    // create the inputString to send to the REPL
    val srcString : String = getText
    var inputString = srcString.substring(_anchorPos)
    
    // get rid of the new line we just put in our input string
    val nlIndex = inputString.indexOf('\n')
    if(nlIndex != -1) {
      val (inpStr1, inpStr2) = inputString.splitAt(nlIndex)
      inputString = inpStr1 + inpStr2.drop(1)
    }
    
    // apply formatting to the fixed input string
    val formattedInputString = ConsolePanel.formatter.htmlFormat(inputString, 
                                                        ConsolePanel.maxCols)
    
    // add the input string to the readOnlyOutput
    _updateReadOnlyText(formattedInputString)
    _anchorPos = getLength
    
    // store the input string in the history list.
    if(inputString != "") {
      GUIActor ! ("Repl", ("addHistory", inputString))
    }
    
    // send the input String to the Repl's actor
    this ! "\n"
    GUIActor ! ("ReplInput", inputString)
  }
  
  
  
  
  
  border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
  font = ConsolePanel.font
  editorKit = new javax.swing.text.html.HTMLEditorKit
  text = """<div style="font-family:Lucida Console;font-size:12pt">"""
}

