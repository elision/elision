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

import ornl.elision.gui.EvaConfig
import ornl.elision.gui.GUIActor
import ornl.elision.gui.GUIColors
import ornl.elision.gui.mainGUI
import ornl.elision.gui.copypaste._
import ornl.elision.gui.elision.EliReplThread
import ornl.elision.gui.elision.EliRegexes
import ornl.elision.gui.elision.EliTreeVisLevel
import ornl.elision.syntax



/**
 *  This panel displays Eva's current REPL in a scrollable EditorPane. 
 */
class ConsolePanel extends BoxPanel(Orientation.Vertical) {
  background = GUIColors.bgColor
  preferredSize = new Dimension(Integer.MAX_VALUE, 300)
    
  
  

  /** The EditorPane containing the REPL */
  val console = new EvaConsole(this)    
    
  /** The ScrollPane containing the console. */
  val scrollingConsolePanel = new ScrollPane {
    background = GUIColors.bgColor
    border = new javax.swing.border.EmptyBorder(ConsolePanel.inset, 
        ConsolePanel.inset, ConsolePanel.inset, ConsolePanel.inset)
    
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
  /** The thread for the currently running REPL. */
  var replThread : ReplThread = null
    
  /** Changes which REPL the ConsolePanel is running and provides a thread for it. */
  def changeMode(mode : String) : Unit = {
    mode match {
      case "Elision" =>
        /** The REPL thread instance */
        replThread = new EliReplThread
        replThread.start
      case "Welcome" => // ignore messages.
      case _ =>
        mainGUI.consolePanel.console.emitln("ConsolePanel error: Eva is not in a recognized mode.")
    }
  }
  
  
  override def paint(g : Graphics2D) : Unit = {
    // Sometimes paint will throw an exception when Eva's mode is switched. 
    // We'll just ignore these exceptions.
    super.paint(g)
  }
  
  console.start
}

/** 
 * Contains some helpful constants used by the ConsolePanel, 
 * EditorPaneOutputStream, and EditorPaneInputStream. 
 */
object ConsolePanel {
  /** If the Repl panel's maximum lines is set below this, then it will not enforce a maximum on the lines it retains.*/
  val infiniteMaxLines = 10
  
  /** Used for setting border spacings in this panel */
  val inset = 3
  
  /** The maximum number of columns to enforce in the EditorPane*/
  var maxCols = 80
  
  /** The Elision syntax formatter used for highlighting in the repl. */
  val formatter = new syntax.SyntaxFormatter(EliRegexes, true, true)
    
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
}


