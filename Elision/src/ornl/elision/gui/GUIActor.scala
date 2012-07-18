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

/** The Actor object used to receive and process communications from the REPL */
object GUIActor extends Actor {
    
    /** A reference to the GUI's TreeBuilder. */
    val treeBuilder = new TreeBuilder
    treeBuilder.start
    
    /** Flag for temporarily disabling the TreeBuilder. */
    var disableTreeBuilder = false
    
    /** A flag used by the GUI to determine if this actor is awaiting some sort of response from the REPL. Be careful with this. */
    var waitingForReplInput = false
    
    /** Flag for telling waitOnREPL to display messages about what it's waiting on. */
    var verbose = false
    
	def act() = {
		loop {
        //    System.out.println("Threads active: " + Thread.activeCount)
			react {
                case "quit" => 
                    System.out.println("Quitting " + mainGUI.mode + " mode...")
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
                    case ("Repl", args : Any) => 
                        // forward a message to the REPL
                        ornl.elision.repl.ReplActor ! args
                    case ("ReplInput", inputString : String) =>
                        ornl.elision.repl.ReplActor ! inputString
                    case ("reGetHistory", result : Any, histSize : Int) =>
                        ConsolePanel.reGetHistory = (result, histSize)
                        waitingForReplInput = false
                    case ("Eva", cmd : String, args : Any) => 
                        // process a TreeBuilder command received from the Elision.
                        if(!mainGUI.config.disableTree) treeBuilder.tbActor ! ("Eva", cmd, args)
                    case selFile : java.io.File => 
                        // The actor reacts to a File by passing the file's contents to the REPL to be processed as input.
                        if(!mainGUI.config.disableTree) mainGUI.treeVisPanel.isLoading = true
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
                    case ("replFormat", flag : Boolean) =>
                        mainGUI.consolePanel.tos.applyFormatting = flag
                        ornl.elision.repl.ReplActor ! ("wait", false)
                    case("replReduceLines", flag : Boolean) =>
                        mainGUI.consolePanel.tos.reduceLines = flag
                        ornl.elision.repl.ReplActor ! ("wait", false)
                    case msg => System.err.println("GUIActor received invalid Elision message: " + msg) // discard anything else that comes into the mailbox.
                }
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
