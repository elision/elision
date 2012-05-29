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
import scala.concurrent.ops._
import sys.process._
import java.io._



/**
 *	This panel displays the Elision REPL in a scrollable TextArea.
 */

class ConsolePanel extends ScrollPane {
	background = mainGUI.bgColor
	
	val inset = 3
	border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
	
	/** The TextArea containing the REPL */
	val console = new TextArea("",20,80) {
		wordWrap = true
		lineWrap = true
		border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
		font = new java.awt.Font("Lucida Console", java.awt.Font.PLAIN, 12 )
	}
	
	/** The REPL thread instance */
	var repl = new ElisionREPLThread
	contents = console
	
	horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never

	// execute the Elision REPL to run in another thread.
	
	/** Used for REPL output */
	val tos = new TextAreaOutputStream(console,60, new ByteArrayOutputStream)
	
	/** Used for REPL input */
	val tis = new TextAreaInputStream(tos)
	
	/** Used for REPL output */
	val ps = new PrintStream(tos)
	
	java.lang.System.setOut(ps)
	//java.lang.System.setErr(ps)
	
	repl.start

}

/** A thread to run the REPL in */

class ElisionREPLThread extends Thread {
	
	override def run : Unit = {
		ornl.elision.repl.ReplActor.guiMode = true
		ornl.elision.repl.ReplActor.guiActor = GUIActor
		ornl.elision.repl.Repl.run
	}

}






/**
 * An OutputStream object that is convenient for redirecting stdout and stderr to a TextArea object.
 * @param textArea		The TextArea object we want the stream to output to.
 * @param maxLines		The maximum number of lines we want to have in textArea at any one time. 
 * @param baos			A ByteArrayOutputStream for processing output byte by byte.
 */

class TextAreaOutputStream( var textArea : TextArea, var maxLines : Int, val baos : ByteArrayOutputStream) extends FilterOutputStream(baos) {
	
	/** The position of the last output character in textArea. */
	var anchorPos : Int = 0
	
	/** The contents of the textArea since the last time it received output. */
	var readOnlyOutput = textArea.text
	
	/**
	 * Writes an array of bytes to the output stream.
	 * @param ba	The bytes to be written.
	 */
	override def write(ba : Array[Byte]) : Unit = {
		try {
			textArea.text += new String(ba)
			enforceMaxLines
		} catch {
			case ioe : IOException => ioe.printStackTrace
		}
		
		anchorPos = textArea.text.length
		readOnlyOutput = textArea.text
		
		textArea.caret.position = textArea.text.length
	}
	
	/**
	 * Writes an array of bytes to the output stream.
	 * @param ba	The bytes to be written.
	 * @param off	The offset to begin writing from in ba.
	 * @param len	The number of bytes we want to write.
	 */
	override def write(ba : Array[Byte], off : Int, len : Int) : Unit = {
		try {
			textArea.text += new String(ba,off,len)
			enforceMaxLines
		} catch {
			case ioe : IOException => ioe.printStackTrace
		}
		
		anchorPos = textArea.text.length
		readOnlyOutput = textArea.text
		
		textArea.caret.position = textArea.text.length
	}
	
	/**
	 * If the current number of lines in the text area is greater than maxlines, 
	 * it erases the oldest lines until the number of lines until it is no longer greater
	 * than maxLines.
	 */
	def enforceMaxLines : Unit = {
		if(maxLines < TextAreaOutputStream.infiniteMaxLines) return
		while(textArea.lineCount > maxLines)
			textArea.text = textArea.text.substring(1,textArea.text.length)
	}

}

object TextAreaOutputStream {
	val infiniteMaxLines = 10
}


/** 
 * It's not actually an InputStream, but it facilitates in sending input from a textArea to the REPL. 
 * @param taos		The TextAreaOutputStream that contains the TextArea that is housing the REPL.
 */

class TextAreaInputStream( var taos : TextAreaOutputStream) {

	/** A convenient reference to taos's textArea */
	val textArea = taos.textArea
	
	/** A list containing the REPL's recent input history */
	val history = new scala.collection.mutable.ListBuffer[String]
	
	/** The current index into the history collection. -1 means we aren't using anything from history as input. */
	var historyIndex : Int = -1
	
	textArea.listenTo(textArea.keys)
	textArea.reactions += {
		case e : swing.event.KeyTyped => {
			if(e.char == '\n') {
				sendToInputStream
			}
			if(e.char == '\b' && textArea.text.length < taos.anchorPos) {
				textArea.text = taos.readOnlyOutput
			}
		}
		case e : swing.event.KeyPressed => {
			// the up/down arrow keys can be used to cycle through the input history.
			if(e.key == swing.event.Key.Up) {
				getHistory(1)
			}
			if(e.key == swing.event.Key.Down) {
				getHistory(-1)
			}
			// prevent the user from moving the caret past the prompt string
			if(e.key == swing.event.Key.Left) {
				var avoidInfLoop = false
				while(textArea.caret.position < taos.anchorPos + 1 && !avoidInfLoop) {
					try {
						textArea.caret.position += 1
					} catch {
						case _ => avoidInfLoop = true
					}
				}
			}
			
			// keyboard menu shortcuts
			
			if(e.key == swing.event.Key.O && e.modifiers == swing.event.Key.Modifier.Control)
				guiMenuBar.openItem.doClick
			
		}
		case e : swing.event.KeyReleased => {
			// make sure that when the user uses up/down to cycle through the input history that the caret is 
			// moved to the end of the text area string at the end.
			if(e.key == swing.event.Key.Up) {
				try {
					textArea.caret.position = textArea.text.length
				} catch {
					case _ => {}
				}
			}
			if(e.key == swing.event.Key.Down) {
				try {
					textArea.caret.position = textArea.text.length
				} catch {
					case _ => {}
				}
			}
			textArea.caret.visible = true
		}
		

	}
	
	/**
	 * Cycles one step up or down through the input history and sets the current input text to the obtained historical input.
	 * If i is positive, it will cycle backwards once through history. If i is negative, it will cycle foward once through history.
	 * @param index		An integer in range [-1,1] telling the history which direction to cycle. 1 means go back 1, -1 means go forward 1.
	 */
	
	def getHistory(index : Int) : Unit = {
		val i = index/math.abs(index) // normalize i so that we can only increment by magnitudes of 1 at a time.
	
		historyIndex += i
		
		var newInput = ""
		
		historyIndex = math.max(-1, math.min(history.size - 1, historyIndex))
		if(historyIndex >= 0)
			newInput = history(historyIndex)
		
		textArea.text = taos.readOnlyOutput + newInput
		textArea.caret.visible = false
	}
	
	
	/**
	 * This sends whatever is the current input text to the Repl's actor so that it can be processed by the Repl.
	 * The input is also saved into the input history.
	 */
	
	def sendToInputStream : Unit = {
		import java.lang.System
		import ornl.elision.repl.Repl
		
		try {
			// get rid of the '\n' produced by pressing enter.
			val endLinePos = textArea.text.indexOf('\n',taos.anchorPos)
			textArea.text = textArea.text.take(endLinePos) + textArea.text.substring(endLinePos+1)
			
			// if for any reason the first character after the prompt string isn't a space, insert a space.
			
			if(textArea.text.charAt(taos.anchorPos) != ' ') textArea.text.take(taos.anchorPos) + " " + textArea.text.substring(taos.anchorPos + 1)
			
			// create the inputString to send to the REPL
			
			val inputString = textArea.text.substring(taos.anchorPos)
			
			// store the input string in the history list.
			
			history.prepend(inputString) //.dropRight(1))
			if(history.size > TextAreaInputStream.MAX_HISTORY) 	history.trimEnd(1)
			historyIndex = -1
			
			// send the input String to the Repl's actor
			println()
			ornl.elision.repl.ReplActor ! inputString
			
		//	java.lang.System.in.read(inputString.getBytes,0,inputString.length)
		
		//	val isr = new InputStreamReader(bais)
		//	val br = new BufferedReader(isr)
		//	br.readLine
		} catch {
			case _ =>
		}
	}
}
 
 /** Static values used by TextAreaInputStream */

 object TextAreaInputStream {
	val MAX_HISTORY : Int = 20
 }
 
 
 

 



