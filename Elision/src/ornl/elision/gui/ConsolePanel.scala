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

package ornl.elision.gui

import scala.swing._
import scala.concurrent.ops._
import scala.actors.Actor
import sys.process._
import java.io._

import ornl.elision.syntax



/**
 *  This panel displays Eva's current REPL in a scrollable EditorPane. 
 */
class ConsolePanel extends BoxPanel(Orientation.Vertical) {
  background = mainGUI.bgColor
  preferredSize = new Dimension(Integer.MAX_VALUE, 300)
    
  /** Used for setting border spacings in this panel */
  val inset = 3
  

  /** The EditorPane containing the REPL */
  val console = new EditorPane { 
    border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
    font = ConsolePanel.font
    editorKit = new javax.swing.text.html.HTMLEditorKit
    text = """<div style="font-family:Lucida Console;font-size:12pt">"""
  }
    console.listenTo(console.mouse.clicks)
    console.reactions += {
        case e : swing.event.MouseClicked =>
            val btn = e.peer.getButton
            if(btn == java.awt.event.MouseEvent.BUTTON3) {
                val copypasta = new TextRClickMenu(console)
                copypasta.show(console.peer, e.point.x, e.point.y)
            }
    }
  ConsolePanel.textArea = console
    
    
  /** The ScrollPane containing the console. */
  val scrollingConsolePanel = new ScrollPane {
    background = mainGUI.bgColor
    border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
    
    horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
    verticalScrollBarPolicy = ScrollPane.BarPolicy.Always

    preferredSize = new Dimension(Integer.MAX_VALUE, 300)
    
    contents = console
  }
  
  contents += scrollingConsolePanel
  
  
  listenTo(this)
  reactions += {
    case re : event.UIEvent =>
      ConsolePanel.maxCols = (scrollingConsolePanel.size.getWidth/ConsolePanel.charWidth).toInt - 4
      GUIActor ! ("guiColumns", ConsolePanel.maxCols - 1)
  }
    

  // execute the Elision REPL to run in another thread.
  
  /** Used for REPL output */
  val tos = new EditorPaneOutputStream(console, mainGUI.config.replMaxLines , new ByteArrayOutputStream)
  
  /** Used for REPL input */
  val tis = new EditorPaneInputStream(tos)
  
  /** Used for REPL output */
  val ps = new PrintStream(tos)
  
  
  /** The thread for the currently running REPL. */
    var replThread : ReplThread = null
    
  /** Changes which REPL the ConsolePanel is running and provides a thread for it. */
  def changeMode(mode : String) : Unit = {
    mode match {
      case "Elision" =>
        /** The REPL thread instance */
        replThread = new elision.EliReplThread
        replThread.start
      case "Welcome" => // ignore messages.
      case _ =>
        System.out.println("ConsolePanel error: Eva is not in a recognized mode.")
    }
  }
  
  
  override def paint(g : Graphics2D) : Unit = {
    // Sometimes paint will throw an exception when Eva's mode is switched. 
    // We'll just ignore these exceptions.
    try {
      super.paint(g)
    }
    catch {
      case _ : Throwable => 
    }
  }
  
  tos.start
  java.lang.System.setOut(ps)
}

/** 
 * Contains some helpful constants used by the ConsolePanel, 
 * EditorPaneOutputStream, and EditorPaneInputStream. 
 */
object ConsolePanel {
  /** If the Repl panel's maximum lines is set below this, then it will not enforce a maximum on the lines it retains.*/
  val infiniteMaxLines = 10
  
  /** The maximum number of columns to enforce in the EditorPane*/
  var maxCols = 80
    
  /** The maximum number of rows a single print command is allowed before being truncated. */
  val printMaxRows = 20
  
  /** Just a reference to the ConsolePanel's EditorPane */
  var textArea : EditorPane = null
  
  /** The Elision syntax formatter used for highlighting in the repl. */
  val formatter = new syntax.SyntaxFormatter(elision.EliRegexes, true, true)
    
  /** The console's font. */
  val font = new java.awt.Font("Lucida Console", java.awt.Font.PLAIN, 12 )
  
  /** Holds the Elision Repl's response for the ConsolePanel's last getHistory message. */
  var reGetHistory : Any = None
  
  /** A constant for the width of a Lucida Console font character. */
  val charWidth = {
    val bi = new java.awt.image.BufferedImage(100,100, java.awt.image.BufferedImage.TYPE_INT_ARGB)
    val gfx = bi.createGraphics
    gfx.setFont(font)
    val fm = gfx.getFontMetrics
    fm.charWidth('m')
  }
  
  /** gets the text of the EditorPane */
  def getText() : String = {
    val doc = textArea.peer.getDocument
    doc.getText(0,doc.getLength)
  }
  
  /** gets the length of the EditorPane's text. */
  def getLength() : Int = {
    textArea.peer.getDocument.getLength
  }
}






/**
 * An OutputStream object that is convenient for redirecting stdout and stderr to a EditorPane object.
 * @param textArea    The EditorPane object we want the stream to output to.
 * @param maxLines    The maximum number of lines we want to have in textArea at any one time. 
 * @param baos      A ByteArrayOutputStream for processing output byte by byte.
 */
class EditorPaneOutputStream( var textArea : EditorPane, var maxLines : Int, val baos : ByteArrayOutputStream) extends FilterOutputStream(baos) with Actor {
  
  /** flag for applying Elision formatting to output */
  var applyFormatting = false
    
  /** flag for enforcing a maximum number of lines of output to be printed at one time. */
  var reduceLines = true
  
  /** The position of the last output character in textArea. */
  var _anchorPos : Int = 0
  
  /** The contents of textArea since the last time it received output. */
  var _readOnlyOutput = ""
  
  /**
   * Writes an array of bytes to the output stream.
   * @param ba  The bytes to be written.
   */
  override def write(ba : Array[Byte]) : Unit = {
    try {
      _write(new String(ba))
    } catch {
      case ioe : Throwable => ioe.printStackTrace
    }
  }
  
  /**
   * Writes an array of bytes to the output stream.
   * @param ba  The bytes to be written.
   * @param off  The offset to begin writing from in ba.
   * @param len  The number of bytes we want to write.
   */
  override def write(ba : Array[Byte], off : Int, len : Int) : Unit = {
    try {
      _write(new String(ba,off,len))
    } catch {
      case ioe : Throwable => ioe.printStackTrace
    }
  }
  
  /** 
   * Does the actually processing work for writing new text to the EditorPane. 
   * This includes applying HTML tags for formatting and keeping track 
   * of where the boundary between output and input space is. 
   * @param _newTxt    is the string being appended to the EditorPane.
   */
  private def _write(_newTxt : String) : Unit = {
    this ! _newTxt
  }
    
    
  def act() = {
    loop {
      react {
        case ("bumpCaret", true) =>
          // Moves the input caret to the beginning of the input area of the 
          // text if it is behind it.
          try {
            textArea.caret.position = math.max(_anchorPos, textArea.caret.position)
          }
          catch {
            case _ : Throwable =>
          }
        case _newTxt : String =>
          // Received a new chunk of output. Format it and append it to past output.
          try {
            var newTxt = _newTxt
            
            /*
            if(newTxt == "\n") 
              newTxt = """<br/>"""
            else {
              // Inject our new text with HTML tags for formatting.
              if(applyFormatting) 
                newTxt = ConsolePanel.formatter.htmlFormat(newTxt, ConsolePanel.maxCols)
              else 
                newTxt = ConsolePanel.formatter.minHtmlFormat(newTxt, ConsolePanel.maxCols) 
              
              if(applyFormatting && reduceLines) 
                newTxt = _reduceTo9Lines(newTxt)
            } // endifelse
            */
            
            // Inject our new text with HTML tags for formatting.
            if(applyFormatting) 
              newTxt = _reduceTo9Lines(ConsolePanel.formatter.htmlFormat(newTxt, ConsolePanel.maxCols))
            else 
              newTxt = ConsolePanel.formatter.minHtmlFormat(newTxt, ConsolePanel.maxCols) 
            
          //  if(applyFormatting && reduceLines) 
          //    newTxt = _reduceTo9Lines(newTxt)
            
            // append our processed new text to our previous output.
            _updateReadOnlyText(newTxt)
            _enforceMaxLines
            
            // apply a constant-width font to our entire EditorPane.
            textArea.text = """<div style="font-family:Lucida Console;font-size:12pt">""" + _readOnlyOutput
            
            // update our anchor point and caret position.
            _anchorPos = ConsolePanel.getLength
            textArea.caret.position = _anchorPos
              
          } 
          catch {
            case ioe : Exception => ioe.printStackTrace
          }
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
    if(maxLines < ConsolePanel.infiniteMaxLines) return
    while(lineCount(_readOnlyOutput) > maxLines) { 
      if(!_chompFirstLine) 
        return
    } // endwhile
  }
  
  /** 
   * Reduces a new line of HTML-formatted output so that it doesn't exceed 9 lines. 
   * If it does, it is cut off and appended with "..." below it. 
   * @param txt    The HTML string we are reducing to no more than 9 lines.
   * @return      Our string reduced to no more than 9 lines when interpretted as HTML. 
   *              If it exceeds 9 lines, "..." is appended on the line below the 9th line.
   */
  def _reduceTo9Lines(txt : String) : String = {
    if(!reduceLines)
      return txt
      
    var lineBreakCount = 1
    for(myMatch <- syntax.SyntaxFormatter.htmlNewLineRegex.findAllIn(txt).matchData) {
      if(lineBreakCount == ConsolePanel.printMaxRows) {
        var result = txt.take(myMatch.end)
        var fontStartCount = syntax.SyntaxFormatter.htmlFontStartRegex.findAllIn(result).size
        val fontEndCount = syntax.SyntaxFormatter.htmlFontEndRegex.findAllIn(result).size
        
        fontStartCount -= fontEndCount
        
        while(fontStartCount > 0) {
          result += """</font>"""
          fontStartCount -= 1
        }
        result += """...<br/>"""
        return result
      } // endif
      
      lineBreakCount += 1
    } // endfor
    txt
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
}




/** 
 * It's not actually an InputStream, but it facilitates in sending input from a textArea to the current REPL. 
 * @param taos    The TextAreaOutputStream that contains the TextArea that is housing the REPL.
 */
class EditorPaneInputStream( var taos : EditorPaneOutputStream) {

  /** A convenient reference to taos's textArea */
  val textArea = taos.textArea
  
  textArea.listenTo(textArea.keys)
  textArea.reactions += {
    case e : swing.event.KeyTyped => {
      if(e.char == '\n') {
        // pressing enter sends the current input text to the Elision repl.
        sendToInputStream
      }
      if(e.char == '\b' && ConsolePanel.getLength < taos._anchorPos) {
        textArea.text = """<div style="font-family:Lucida Console;font-size:12pt">""" + taos._readOnlyOutput
        textArea.caret.position = taos._anchorPos
      }
    }
    case e : swing.event.KeyPressed => {
      // the up/down arrow keys can be used to cycle through the input history.
      if(e.key == swing.event.Key.Up) {
        getHistory(-1)
      }
      if(e.key == swing.event.Key.Down) {
        getHistory(1)
      }
      // prevent the user from moving the caret past the prompt string
      if(e.key == swing.event.Key.Left) {
        var avoidInfLoop = false
        try {
          textArea.caret.position = math.max(taos._anchorPos, textArea.caret.position)
        } catch {
          case _ : Throwable => avoidInfLoop = true
        }
      }
      
      
      // keyboard menu shortcuts

      if(e.key == swing.event.Key.F1)
        mainGUI.evaMenuBar.helpItem.doClick   
      
      mainGUI.visPanel.curLevel match {
        case etvp : elision.EliTreeVisPanel =>
          if(e.key == swing.event.Key.Escape) {
              etvp.selectingRuleLHS = false
          }
        case _ =>
      }
            
      
    }
    case e : swing.event.KeyReleased => {
      // make sure that when the user uses up/down to cycle through the input history that the caret is 
      // moved to the end of the text area string at the end.
      if(e.key == swing.event.Key.Up) {
        try {
          textArea.caret.position = ConsolePanel.getLength
        } catch {
          case _ : Throwable => {}
        }
      }
      if(e.key == swing.event.Key.Down) {
        try {
          textArea.caret.position = ConsolePanel.getLength
        } catch {
          case _ : Throwable => {}
        }
      }
      if(e.key == swing.event.Key.Home) {
        var avoidInfLoop = false
        try {
          textArea.caret.position = math.max(taos._anchorPos, textArea.caret.position)
        } catch {
          case _ : Throwable => avoidInfLoop = true
        }
      }
      textArea.caret.visible = true
    }
    

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
    textArea.text = """<div style="font-family:Lucida Console;font-size:12pt">""" + taos._readOnlyOutput + newInput
    textArea.caret.visible = false
  }
  
  
  /**
   * This sends whatever is the current input text to the Repl's actor so that it can be processed by the Repl.
   * The input is also saved into the input history.
   */
  def sendToInputStream : Unit = {
    import java.lang.System
    
    try {
    
      // create the inputString to send to the REPL
      val srcString : String = ConsolePanel.getText // textArea.text
      var inputString = srcString.substring(taos._anchorPos)
      
      // get rid of the new line we just put in our input string
      val nlIndex = inputString.indexOf('\n')
      if(nlIndex != -1) {
        val (inpStr1, inpStr2) = inputString.splitAt(nlIndex)
        inputString = inpStr1 + inpStr2.drop(1)
      }
      
      // apply formatting to the fixed input string
      val formattedInputString = ConsolePanel.formatter.htmlFormat(inputString, ConsolePanel.maxCols)
      
      // add the input string to the readOnlyOutput
      taos._updateReadOnlyText(formattedInputString)
      taos._anchorPos = ConsolePanel.getLength
      
      // store the input string in the history list.
      if(inputString != "") {
        GUIActor ! ("Repl", ("addHistory", inputString))
      }
      
      // send the input String to the Repl's actor
      println()
      GUIActor ! ("ReplInput", inputString)

    } catch {
      case _ : Throwable =>
    }
  }
  
}



