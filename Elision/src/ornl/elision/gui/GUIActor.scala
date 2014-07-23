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

import concurrent.ops._
import sys.process._
import java.io._

import scala.actors.Actor

import ornl.elision.gui.elision.TreeBuilderActor

/** The Actor object used to receive and process communications from the REPL */
object GUIActor extends Actor {
    
  /** Flag for temporarily disabling the TreeBuilder. */
  var disableTreeBuilder = false
  
  /** 
   * A flag used by the GUI to determine if this actor is awaiting some sort 
   * of response from the REPL. Be careful with this. 
   */
  var waitingForReplInput = false
  
  /** Whether waitOnREPL should display messages about what it's waiting on. */
  var verbose = false
    
	def act() = {
		loop {
      //    System.err.println("Threads active: " + Thread.activeCount)
      //    System.err.println("ReplActor: " + ornl.elision.actors.ReplActor.getState)
      //    System.err.println("Console REPL thread: " + mainGUI.consolePanel.replThread.getState)
			react {
			  // Terminate the current REPL's thread.
        case "quit" => 
          mainGUI.consolePanel.console.emitln("\nQuitting " + mainGUI.mode + " mode...")
          mainGUI.consolePanel.replThread.clean
          
        // Switch to some other mode supported by Eva.
        case ("changeMode", mode : String) => 
          mainGUI.consolePanel.console.emitln("\nChanging to " + mode + " mode...")
          if(mainGUI.consolePanel.replThread != null) { 
            mainGUI.consolePanel.replThread.clean
          }
          
          mainGUI.changeMode(mode)
          waitingForReplInput = false
          mainGUI.consolePanel.console.emitln("Mode change was successful")
        
        // Process a message based on what mode Eva is currently operating in.
        case theMsg : Any => 
          reactWithMode(theMsg)
			}
		}
	}
    
  /** The actor handles received messages depending on Eva's current mode. */
  def reactWithMode(theMsg : Any) : Unit = {
    mainGUI.mode match {
      case "Elision" =>
        theMsg match {
          // forward a message to the REPL
          case ("Repl", args : Any) => 
            ornl.elision.actors.ReplActor ! args
              
          // Send a line of input to the Elision REPL.
          case ("ReplInput", inputString : String) =>
            ornl.elision.actors.ReplActor ! inputString
          
          // Print a new prompt thingy in the console.
          case ("newPrompt", prompt : String) =>
            mainGUI.consolePanel.console ! prompt
          
          // Receive a line of input history from the REPL's Actor.
          case ("reGetHistory", result : Any) =>
            console.ConsolePanel.reGetHistory = result
            waitingForReplInput = false
              
          // Tell the REPL to set its column width to cols.
          case ("guiColumns", cols : Int) =>
            ornl.elision.actors.ReplActor ! ("guiColumns", cols)
          
          // Receive a tree building message from Elision.
          case ("toGUI", msg : Any) =>
            TreeBuilderActor ! msg
              
          // Load a TreeSprite from an XML or JSON file.
          case ("OpenTree", file : java.io.File) =>
            TreeBuilderActor ! ("OpenTree", file)
              
          // Save the current TreeSprite as XML.
          case ("SaveTreeXML", file : java.io.File) =>
            TreeBuilderActor ! ("SaveTreeXML", file)
          
          // Save the current TreeSprite as JSON.
          case ("SaveTreeJSON", file : java.io.File) =>
            TreeBuilderActor ! ("SaveTreeJSON", file)
          
          // Skip visualizing the next tree. Some operations in Eva 
          // parse some atoms in Elision that we don't want to create
          // a tree for. Such operations send this message before
          // parsing the atom.
          case "IgnoreNextTree" => 
            TreeBuilderActor ! "IgnoreNextTree"
              
          // Send lines of input to Elision from a file and eventually construct a TreeSprite visualization of the results.
          case selFile : java.io.File => 
            // The actor reacts to a File by passing the file's contents to the REPL to be processed as input.
            Thread.sleep(100)
            
            // here we accumulate the text of the file into one big string.
            var str : String = ""
            val br = new BufferedReader(new FileReader(selFile))
            while(br.ready) {
              str += br.readLine + "\n"
            }
            br.close
            
            // now we send the accumulated string to the REPL's actor so that the REPL will process it as input.
            mainGUI.consolePanel.console.emitln("Reading REPL input from file: " + selFile.getPath)
            ornl.elision.actors.ReplActor ! str
              
          // Receive a message from the REPL whether to syntax color output it is about to produce, then 
          // respond to the REPL that we are ready to receive that input. 
          case ("replFormat", flag : Boolean) =>
            mainGUI.consolePanel.console.applyFormatting = flag
            ornl.elision.actors.ReplActor ! ("wait", false)
          
          // Toggle whether the visualization is loading.
          case ("loading", flag : Boolean) =>
              mainGUI.visPanel.isLoading = flag
              
          // Opens the Help dialog.
          case "helpOpen" =>
            mainGUI.evaMenuBar.helpItem.doClick
              
          // Control whether the quick rule-maker utility is currently being used.
          case ("enableRuleMaker", flag : Boolean) =>
            mainGUI.visPanel.curLevel match {
              case etvp : elision.EliTreeVisLevel =>
                 etvp.selectingRuleLHS = flag
              case _ =>
            }
          
          // Reselects the currently selected NodeSprite in the visualization.
          case "treeReselectCurrentNode" =>
            mainGUI.visPanel.curLevel match {
              case treeVisPanel : trees.TreeVisLevel => 
                treeVisPanel.selectNode(treeVisPanel.treeSprite.selectedNode)
            }
          
          // discard anything else that comes into the mailbox.
          case msg => 
            System.err.println("GUIActor received invalid Elision message: " + msg) 
        }
      case "Welcome" => // ignore messages.
      case _ =>
        System.err.println("GUIActor error: Eva is not in a recognized mode.")
    }
  }
    
    
    
    
    
    
    
    /** forces the calling thread to wait for the REPL to finish doing something. */
	def waitOnREPL(doStuff : () => Unit = null, msg : String = null) : Unit = {
		waitingForReplInput = true
		if(doStuff != null) doStuff()
		while(waitingForReplInput) {
			if(verbose && msg != null) System.err.println("waiting on the REPL: " + msg)
			Thread.sleep(20) // sleep until the GUI receives input from the REPL
		}
	}
}
