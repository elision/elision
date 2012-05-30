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
import scala.concurrent.ops._
import sys.process._
import java.io._
import java.awt.Graphics2D
import scala.util.matching._
import scala.collection.mutable.ListBuffer

import sage2D._

/** Used to display the properties of the currently selected node. */

class PropertiesPanel extends BoxPanel(Orientation.Vertical) {
	background = mainGUI.bgColor
	val inset = 3
	border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
	
	val label = new Label("Atom parse String: ")
	label.border = new javax.swing.border.EmptyBorder(0,0,inset,0)
	contents += label
	
	val htmlKit = new javax.swing.text.html.HTMLEditorKit
	
	/** The EditorPane that displays the currently selected node's parse string */
	val parseArea = new EditorPane {
		editable = false
		border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset+10)
		font = new java.awt.Font("Lucida Console", java.awt.Font.PLAIN, 12 )
		focusable = false
		editorKit = htmlKit
	}
	
	/** The scrolling pane that contains parseArea */
	val parsePanel = new ScrollPane {
		contents = parseArea
		horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never
	}
	contents += parsePanel
	
	val label2 = new Label("Atom properties: ")
	label2.border = new javax.swing.border.EmptyBorder(0,0,inset,0)
	contents += label2
	
	/** The TextArea that displays the currently selected node's properties */
	val textArea = new TextArea("",15,50) {
		wordWrap = true
		lineWrap = true
		editable = false
		border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
		font = new java.awt.Font("Lucida Console", java.awt.Font.PLAIN, 12 )
		focusable = false
	}
	
	/** The scrolling pane that contains textArea */
	val taPanel = new ScrollPane {
		contents = textArea
		horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never
	}
	contents += taPanel

	
	/**
	 * Displays an atom's parse string in parseArea with Elision syntax highlighting applied. 
	 * @param text					the atom's parse string.
	 * @param disableHighlight		disables highlighting if true.
	 */
	
	def parseStringHighlight(text : String, disableHighlight : Boolean = true) = {
		var result = text
		
		if(!disableHighlight) { // skip this whole block if we're disabling highlighting.
			// obtain the lists of data for the font tags.
			val (starts,colors,ends) = applyElisionRegexes(text)
			var insertedChars = 0
			
			while(!starts.isEmpty || !ends.isEmpty) {
				var tag = ""
				var insertLoc = 0
				
				if(!starts.isEmpty && (ends.isEmpty || starts(0) < ends(0))) {
					// insert a <font color=> tag
					tag = """<font color=""" + "\"" + colors(0) + "\"" + """>"""
					
					insertLoc = starts(0)
					starts.trimStart(1)
					colors.trimStart(1)
				} else {
					// insert a </font> tag
					tag = """</font>"""
					insertLoc = ends(0)
					ends.trimStart(1)
				}
				
				val (str1, str2) = result.splitAt(insertLoc + insertedChars)
				result = str1 + tag + str2
				insertedChars += tag.size
			}
		}
		
		parseArea.text = result
	}
	
	
	/**
	 * Consumes the parse string text to construct lists for correct start and end positions
	 * for <font color=~~~~> </font> tags to be inserted into the parse string. 
	 *
	 */
	
	private def applyElisionRegexes(txt: String) : (ListBuffer[Int], ListBuffer[String], ListBuffer[Int]) = {
		val starts = new ListBuffer[Int]
		val colors = new ListBuffer[String]
		val ends = new ListBuffer[Int]
		var text = txt
		
		var chompedChars = 0
		
		// populate our lists of font tag data by having our regular expressions consume the text until
		// no more regular expressions can consume any of the text.
		while(text != "") {
			var bestRegex : Regex = null
			var bestStart : Int = -1
			var bestEnd : Int = -1
			
			// iterate over our list of regexes and find the one the can be applied the earliest.
			for(regex <- EliRegexes.rList) {
				
				regex.findFirstMatchIn(text) match {
					case Some(myMatch : scala.util.matching.Regex.Match) =>
						if(bestStart == -1 || myMatch.start < bestStart) {
							bestRegex = regex
							bestStart = myMatch.start
							bestEnd = myMatch.end
						}
					case _ => // this regex could not be applied.
				}
			}
			if(bestRegex == null)	text = ""
			else {
				val color = EliRegexes.colorMap(bestRegex)
				starts += chompedChars + bestStart
				colors += color
				ends += chompedChars + bestEnd
				
				text = text.drop(bestEnd)
				chompedChars += bestEnd
			}
		}
		
		/*
		// this is just test code. It doesn't highlight anything meaningful.
		starts += 5
		colors += EliWebColors.keywords
		ends += 10
		
		starts += 10
		colors += EliWebColors.comments
		ends += 15
		
		starts += 20
		colors += EliWebColors.properties
		ends += 25
		// end test code.
		*/
		
		
		
		(starts,colors,ends)
	}
}


