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

import swing._
import scala.swing.BorderPanel.Position._
import swing.TabbedPane

/** Used to display information about the currently selected node. */
class PropertiesPanel extends BoxPanel(Orientation.Vertical) {
	background = mainGUI.bgColor
	/** Used for setting border spacings in this panel */
	val inset = 3
	border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
	
    
    val tabs = new TabbedPane
    contents += tabs

    // Parse String tab
	
	/** The EditorPane that displays the currently selected node's parse string */
	val parseArea = new EditorPane {
		editable = false
		border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset+10)
		focusable = true
		editorKit = new javax.swing.text.html.HTMLEditorKit
	}
	
	/** The scrolling pane that contains parseArea */
	val parsePanel = new ScrollPane {
		contents = parseArea
		horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never
		preferredSize = new Dimension(PropertiesPanel.preferredWidth,PropertiesPanel.parsePanelHeight)
	}
    val parsePage = new TabbedPane.Page("Atom Parse String", parsePanel)
    tabs.pages += parsePage
	

    
    // Properties tab
	
	/** The TextArea that displays the currently selected node's properties */
	val textArea = new TextArea("",15,45) {
		wordWrap = true
		lineWrap = true
		editable = false
		border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
		font = new java.awt.Font("Lucida Console", java.awt.Font.PLAIN, 12 )
		focusable = true
	}
	
	/** The scrolling pane that contains textArea */
	val propsPanel = new ScrollPane {
		contents = textArea
		horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never
	}
    val propsPage = new TabbedPane.Page("Atom Properties", propsPanel)
    tabs.pages += propsPage

	
	
	
	
	/**
	 * Displays an atom's parse string in parseArea with Elision syntax highlighting applied. 
	 * @param text					the atom's parse string.
	 * @param disableHighlight		disables highlighting if true.
	 */
	def parseStringHighlight(text : String, disableHighlight : Boolean = true) = {
		
		// set the parseArea's text to the resulting HTML-injected parse string.
		parseArea.text = """<div style="font-family:Lucida Console;font-size:12pt">""" + EliSyntaxFormatting.applyHTMLHighlight(text,disableHighlight,40) + """</div>"""
	}
	
	
	
}


/** Contains constants used by the PropertiesPanel */
object PropertiesPanel {
	val preferredWidth = 260
	val parsePanelHeight = 200
}

