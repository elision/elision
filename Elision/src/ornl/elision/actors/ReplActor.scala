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

/** The REPL's Actor object for communicating with the GUI */
object ReplActor extends Actor {

	/** a flag that tells the REPL whether it is receiving input from a GUI or from the console. */
	var guiMode : Boolean = false 
    
  /** a flag used for forcing the ReplActor to accept a message for exiting. */
  var exitFlag = false
	
	/** flag for temporarily disabling GUI communication. */
	var disableGUIComs = true

	/** a flag used by the Repl object to determine whether the actor is waiting on input from a GUI */
	var waitingForGuiInput : Boolean = false

	/** a string for storing the most recent input from a GUI */
	var guiInput : String = "no gui input yet"

	/** a reference to the GUI's actor. */
	var guiActor : Actor = null
	
	/** a flag that tells the Actor to be verbose while waiting on the GUI. */
	var verbose = false
    
  /** A flag that will skip GUI tree construction for RuleLibrary rewrites. */
  var disableRuleLibraryVis = false
  
  /** The current character width for the GUI repl, if a gui is being used. */
  var guiColumns = 80
  var guiRows = 20
  
  /** A reference to Elision's REPL. */
  var peer : ornl.elision.repl.ERepl = null
    
	/** 
	 * The actor's act loop will wait to receive string input from the GUI. 
	 * It will discard any other input in its mailbox.
	 */
	def act() = {
		loop {
			react {
        case ("Eva", cmd : String, args : Any) =>
          if(guiMode && !disableGUIComs) guiActor ! ("Eva", cmd, args)
        case ("disableGUIComs", flag : Boolean) =>
          disableGUIComs = flag
        case (":quit", true) =>
          if(guiMode)  guiActor ! "quit"
          else exit
				case str : String =>
					guiInput = str
					waitingForGuiInput = false
				case ("wait", flag : Boolean) =>
					waitingForGuiInput = flag
        case ("guiColumns", x : Int) =>
          guiColumns = x
          peer.console.width_=(guiColumns)
          peer.console.height_=(guiRows-1)
        case ("getHistory", index : Int) =>
          if(index == -1) peer._hist.previous
          if(index == 1) peer._hist.next
          peer._hist.current match {
            case str : String => guiActor ! ("reGetHistory", str, peer._hist.size)
            case _ => guiActor ! ("reGetHistory", None, peer._hist.size)
          }
        case ("addHistory", str : String) =>
          peer.addHistoryLine(str)
				case msg => {}
			}
		}
	}
	
	/** forces the calling thread to wait for the GUI to finish doing something. */
	def waitOnGUI(doStuff : () => Unit = null, msg : String = null) : Unit = {
		waitingForGuiInput = true
		if(doStuff != null) doStuff()
		while(waitingForGuiInput) {
			if(verbose && msg != null) println("waiting on the GUI: " + msg)
			Thread.sleep(20) // sleep until the REPL receives input from the GUI
		}
	}
    
  /** Overriden ! method checks global allowMessages flag before processing a message.*/
  override def ! (msg : Any) : Unit = {
    if(guiMode || exitFlag) super.!(msg)
  }
}

