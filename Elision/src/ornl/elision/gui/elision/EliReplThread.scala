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

import java.io.File
import scala.concurrent.ops._
import sys.process._
import ornl.elision.cli._
import ornl.elision.gui._
import ornl.elision.gui.console.ReplThread
import ornl.elision.util.Console
import ornl.elision.actors.ReplActor


/** A thread to run the Elision REPL in */
class EliReplThread extends ReplThread {

  /** A reference the the Console being used by our current repl. */
	var myRepl : ornl.elision.repl.ERepl = null
  
	// Work out where Elision's runtime store should live on the system.
  private val _default_root = (if (CLI.iswin) {
    // On a Windows system the settings should live under %LOCALAPPDATA%
    // in a folder specific to the application.  While the simplest thing is
    // to obtain the local appdata folder from the environment variable, this
    // is certainly not perfect, and is not what is recommended by Microsoft.
    // A better method is to use CSIDL_LOCAL_APPDATA, obtained from
    // SHGetFolderPath.  But that would require native calls.
    val env = System.getenv()
    new File(if (env.containsKey("LOCALAPPDATA")) {
      env.get("LOCALAPPDATA")
    } else {
      new File(
          new File(env.get("USERPROFILE"), "Local Settings"),
          "Application Data").getAbsolutePath()
    }, "elision").getAbsolutePath()
  } else {
    // On a non-Windows platform the settings should live under the user's
    // `$`HOME folder.
    new File(System.getenv("HOME"), ".elision").getAbsolutePath()
  })
	
	/**
   * Define some settings.
   */
  private val _settings = Seq(
      Setting("elision.root", Some("ELISION_ROOT"), None,
          Some(_default_root), "Specify the folder where Elision should " +
              "store its data."),
      Setting("elision.history", Some("ELISION_HISTORY"), None,
          Some(".elision-history.eli"),
          "Name of file where Elision will store the REPL history."),
      Setting("elision.context", Some("ELISION_CONTEXT"), None,
          Some("elision-context.eli"),
          "Name of file where Elision will store the most recent context."),
      Setting("elision.cache", Some("ELISION_CACHE"), None,
          Some(new File(_default_root, "cache").getAbsolutePath),
          "Name of the folder where Elision will cache native handlers."),
      Setting("elision.rc", Some("ELISIONRC"), None,
          Some("elision.ini"),
          "Name of file to read after bootstrapping Elision."))
	
  val state = CLI(Array.empty[String], null, _settings, false)        
  
	/** Starts a new thread in which the REPL will run in. */
	override def run : Unit = {
		ornl.elision.actors.ReplActor.guiActor = GUIActor
		runNewRepl
	}
	
	/** Creates an instance of and begins running the new REPL */
	def runNewRepl : Unit = {
		myRepl = new ornl.elision.repl.ERepl(state)
		ornl.elision.core.knownExecutor = myRepl
		
		myRepl.console = mainGUI.consolePanel.console
		ReplActor.history = myRepl
		ReplActor.console = myRepl.console
		ReplActor ! ("disableGUIComs", true)
		myRepl.setProperty[Boolean]("syntaxcolor", false)
		ReplActor.start
    
		myRepl.run()
    myRepl.clean()
	}
    
  /**  */
  def clean : Unit = {
    ornl.elision.actors.ReplActor ! ":quit"
  }

}


