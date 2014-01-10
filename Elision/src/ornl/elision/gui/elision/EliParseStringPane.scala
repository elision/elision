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

package ornl.elision.gui.elision

import swing._
import scala.swing.BorderPanel.Position._
import swing.TabbedPane

import ornl.elision.gui._
import ornl.elision.gui.console._
import ornl.elision.gui.copypaste._
import ornl.elision.syntax

/** A scrolling pane for displaying an Elision atom's parse string with formatting and syntax coloring. */
class EliParseStringPane extends ScrollPane {
  val inset = SidePanel.inset
  horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
  verticalScrollBarPolicy = ScrollPane.BarPolicy.Always
  preferredSize = new Dimension(SidePanel.preferredWidth, SidePanel.parsePanelHeight)
  
  /** The EditorPane that displays the currently selected node's parse string */
  val textArea = new EditorPane {
    editable = false
    border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset+10)
    focusable = true
    editorKit = new javax.swing.text.html.HTMLEditorKit 
  }
  textArea.listenTo(textArea.mouse.clicks)
  textArea.reactions += {
    case e : swing.event.MouseClicked =>
      val btn = e.peer.getButton
      if(btn == java.awt.event.MouseEvent.BUTTON3) {
        val copypasta = new TextRClickMenu(textArea)
        copypasta.show(textArea.peer, e.point.x, e.point.y)
      }
  }
  contents = textArea
    
  // formatting in this panel does indentation, but not syntax coloring.
  val formatter = new syntax.SyntaxFormatter(EliRegexes, true, true)
  // formatter.showColorData = true
  
  /**
   * Displays an atom's parse string in textArea with Elision syntax formatting applied. 
   * @param text          the atom's parse string.
   * @param isComment    disables highlighting if true.
   */
  def parseStringFormat(text : String, isComment : Boolean) = {
    // determine the current character width of the textArea.
    val cols = (textArea.size.getWidth/ConsolePanel.charWidth).toInt - 2
    formatter.doesColoring = !isComment
    
    // set the textArea's text to the resulting HTML-injected parse string.
    textArea.text = """<div style="font-family:Lucida Console;font-size:12pt">""" + formatter.htmlFormat(text, cols) + """</div>"""
    
  }
}


/** A scrolling pane for displaying an Elision atom's internal properties. */
class EliAtomPropsPane extends ScrollPane {
  val inset = SidePanel.inset
  horizontalScrollBarPolicy = ScrollPane.BarPolicy.Never
  verticalScrollBarPolicy = ScrollPane.BarPolicy.Always
  
  /** The TextArea that displays the currently selected node's properties */
  val textArea = new TextArea("",15,45) {
    wordWrap = true
    lineWrap = true
    editable = false
    border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
    font = new java.awt.Font("Lucida Console", java.awt.Font.PLAIN, 12 )
    focusable = true
  }
  textArea.listenTo(textArea.mouse.clicks)
  textArea.reactions += {
    case e : swing.event.MouseClicked =>
      val btn = e.peer.getButton
      if(btn == java.awt.event.MouseEvent.BUTTON3) {
        val copypasta = new TextRClickMenu(textArea)
        copypasta.show(textArea.peer, e.point.x, e.point.y)
      }
  }
  contents = textArea 
}



