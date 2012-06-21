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
import swing.BorderPanel.Position._
import concurrent.ops._
import sys.process._
import java.io._
import java.awt.Color
import java.awt.Dimension

import sage2D._

import scala.actors.Actor

/** The Actor object used to receive and process communications from the REPL */
object GUIActor extends Actor {
    
    val treeBuilder = new TreeBuilder
    treeBuilder.start
    var disableTreeBuilder = false
    
    
    
	def act() = {
		loop {
			react {
                case ("Eva", cmd : String, args : Any) => 
                    // process a TreeBuilder command received from the Elision.
                    if(!disableTreeBuilder) treeBuilder.tbActor ! ("Eva", cmd, args) // processTreeBuilderCommands(cmd, args)
				/* case root : ornl.elision.core.RWTreeNode => 
					// The actor reacts to RWTreeNodes by constructing a tree visualization of it in the TreeVisPanel.
					
					mainGUI.treeVisPanel.isLoading = true
					Thread.sleep(100)
					mainGUI.treeVisPanel.treeSprite = TreeSprite.buildRWTree(root)
					
					// once the tree visualization is built, select its root node and center the camera on it.
					
					mainGUI.treeVisPanel.selectNode(mainGUI.treeVisPanel.treeSprite.root)
					mainGUI.treeVisPanel.camera.reset
					
					mainGUI.treeVisPanel.isLoading = false
				*/
				case selFile : java.io.File => 
					// The actor reacts to a File by passing the file's contents to the REPL to be processed as input.
					if(!disableTreeBuilder) mainGUI.treeVisPanel.isLoading = true
					Thread.sleep(100)
					
					// here we accumulate the text of the file into one big string.
					
					var str : String = ""
					val br = new BufferedReader(new FileReader(selFile))
					while(br.ready) {
						str += br.readLine + "\n"
					}
					br.close
					
					// now we send the accumulated string to the REPL's actor so that the REPL will process it as input.
					println("Reading REPL input from file: " + selFile.getPath)
					println()
					ornl.elision.repl.ReplActor ! str
				case "quit" => 
					System.exit(0)
				case ("replFormat", flag : Boolean) =>
					mainGUI.consolePanel.tos.applyFormatting = flag
					ornl.elision.repl.ReplActor ! ("wait", false)
				case msg => System.err.println("GUIActor received invalid message: " + msg) // discard anything else that comes into the mailbox.
			}
		}
	}
}
