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
package ornl.elision.actors

import scala.actors.Actor

/** The REPL's Actor object for communicating with a GUI. */
object ReplActor extends Actor {

  /** a flag used for forcing the ReplActor to accept a message for exiting. */
  var exitFlag = false
	
	/** flag for temporarily disabling GUI communication. */
	var disableGUIComs = true

	/** a flag used by the Repl object to determine whether the actor is waiting on input from a GUI */
	var waitingForGuiInput : Boolean = false

	/** a string for storing the most recent input from a GUI */
	var guiInput : String = "no gui input yet"

	/** a reference to the GUI's actor. Null if we are not connected to a GUI. */
	var guiActor : Actor = null
	
	/** a flag that tells the Actor to be verbose while waiting on the GUI. */
	var verbose = false
    
  /** A flag that will skip GUI tree construction for RuleLibrary rewrites. */
  var disableRuleLibraryVis = false
  
  /** The current character width for the GUI repl, if a gui is being used. */
  var guiColumns = 80
  var guiRows = 20
  
  /** A reference to an object that maintains history. */
  var history : ornl.elision.util.HasHistory = null
  
  /** A reference to a console. */
  var console : ornl.elision.util.Console = null
    
	/**
	 * The actor's act loop will wait to receive string input from the GUI. 
	 * It will discard any other input in its mailbox.
	 */
	def act() = {
		loop {
			react {
			  // Toggle whether to disable communications with the GUI.
        case ("disableGUIComs", flag : Boolean) =>
          disableGUIComs = flag
        
        // Tell this Actor and its GUI's Actor to exit.
        case (":quit", true) =>
          if(guiActor != null)  
            guiActor ! "quit"
          else 
            exit
        
        // new prompt string to be appended to the GUI's console.
        case ("newPrompt", prompt : String) =>
            guiActor ! ("newPrompt", prompt)
            
        // Forward something to the GUI's actor.
        case ("toGUI", msg : Any) =>
          if(guiActor != null && !disableGUIComs) { 
             guiActor ! ("toGUI", msg)
          }
          
        // Receive a line of input from the GUI.
				case str : String =>
					guiInput = str
					waitingForGuiInput = false
					
			  // Toggle whether we are waiting on the GUI to perform some action.
				case ("wait", flag : Boolean) =>
					waitingForGuiInput = flag
					
				// Set the console's columns to match the GUI's columns.
        case ("guiColumns", x : Int) =>
          guiColumns = x
          console.width_=(guiColumns)
          console.height_=(guiRows-1)
          
        // Obtain a line from the input history and send it back to the GUI.
        case ("getHistory", direction : Int) =>
          val entry = if(direction < 1) {
              history.getPreviousHistoryEntry
            }
            else {
              history.getNextHistoryEntry
            } 
          entry match {
            case Some(str : String) => guiActor ! ("reGetHistory", str)
            case _ => guiActor ! ("reGetHistory", None)
          }
          
        // Add a line to the input history.
        case ("addHistory", str : String) =>
          history.addHistoryLine(str)
          
        // Inform the GUI whether it should syntax color what's being printed to stdout.
        case ("syntaxcolor", flag : Boolean) =>
          guiActor ! ("replFormat", flag)
				case msg => {}
			}
		}
	}
	
  
	/** 
	 * Pause until the GUI sends a wake up message. Optionally, display a 
	 * message about what we're waiting on, but only if verbose is true.
	 * @param msg  The optional message.
	 */
	def waitForGUI(msg : String = "") {
	  if(verbose) {
	    console.emitln("waiting on the GUI: " + msg)
	  }
	  
	  // Sleep until the GUI wakes us up with a ("wait", false) message.
	  waitingForGuiInput = true
	  while(waitingForGuiInput) {
      Thread.sleep(20) 
    }
	}
	
	/** 
	 * Prompts the GUI for input. This blocks until the GUI sets the 
	 * value for guiInput and sends a ("wait", false) message to us.
	 * @param prompt   The prompt string. e.g.: "e> "
	 * @return         The line of input from the GUI.
	 */
	def readLine(prompt : String) : String = {
    this ! ("newPrompt", prompt)
    
    ReplActor.waitForGUI("gui input")
    ReplActor.guiInput
	}
	
  /** Overriden ! method checks global allowMessages flag before processing a message.*/
  override def ! (msg : Any) : Unit = {
    if(guiActor != null || exitFlag) super.!(msg)
  }
}

