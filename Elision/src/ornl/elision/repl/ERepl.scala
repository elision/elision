/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package ornl.elision.repl

import ornl.elision.parse._

/**
 * Implement an interface to run the REPL from the prompt.
 */
object ReplMain { // extends ERepl {
  /*
  def main(args: Array[String]) {
    run()
    console.emitln("")
    addHistoryLine("// Ended normally: " + new java.util.Date)
    val cfile = new java.io.FileWriter(_lastcontext)
    if (cfile != null) {
      cfile.write(context.toParseString)
      cfile.flush()
      cfile.close()
    } else {
      console.warn("Unable to save context.")
    }
  }
  */
  
  def main(args: Array[String]) {
    runRepl
  }
  
  def runRepl {
    val erepl = new ERepl
    erepl.run()
    erepl.clean()
  }
}

/**
 * Provide a REPL to experiment with the new term rewriter.
 * 
 * ==Use==
 * The REPL can be started from the command line, or programmatically by
 * invoking the `run` method.  It prints a prompt, reads a line from the
 * standard input, and executes the line.
 * 
 * Other uses are possible.  To just execute a line, use the `execute` method.
 * This avoids reading from standard input, etc.  Note that `execute` maintains
 * the history, so history references are possible.
 * 
 * ==REPL Interaction==
 * Interaction with the REPL is described in the documentation of the `run`
 * method.  The REPL provides for command line editing, a persistent history,
 * and special operations.
 */
class ERepl extends Processor {
  import ornl.elision.core._
	import scala.tools.jline.console.history.FileHistory
	import scala.tools.jline.console.ConsoleReader
	import java.io.{File, FileWriter, FileReader, BufferedReader}
  
  
	//////////////////// GUI changes
    ReplActor.start
    ReplActor.peer = this
	ReplActor ! ("disableGUIComs", true)
	//////////////////// end GUI changes
  
  //======================================================================
  // Figure out where to read and store the history, and where to store
  // the last context.
  //======================================================================

  /** Access to system properties. */
  private val _prop = new scala.sys.SystemProperties
  
  /** The user's home folder. */
  private val _home = {
	  val root = System.getenv("ELISION_ROOT")
	  if (root != null) {
	    root
	  } else {
	    _prop("user.home")
	  }
	}
  
  /** Figure out the location to store the history. */
  protected val _filename = {
	  val hce = System.getenv("ELISION_HISTORY")
	  if (hce != null) {
	    hce
	  } else {
      val fname = (if (_prop("path.separator") == ":") ".elision-history.eli"
        else "elision-history.eli")
      _home + _prop("file.separator") + fname
	  }
	}
  
  /** Figure out where to stash the context on exit. */
  protected val _lastcontext = {
    val cce = System.getenv("ELISION_CONTEXT")
    if (cce != null) {
      cce
    } else {
      val fname = (if (_prop("path.separator") == ":") ".elision-context.eli"
        else "elision-context.eli")
      _home + _prop("file.separator") + fname
    }
  }
  
  /** Figure out the startup file that is read after bootstrapping. */
  protected val _rc = {
      val rce = System.getenv("ELISIONRC")
      if (rce != null) {
        rce
      } else {
        _home + _prop("file.separator") + (
            if (_prop("path.separator") == ":") ".elisionrc"
            else "elision.ini")
      }
  }
  
  //======================================================================
  // Configure the history for this REPL.
  //======================================================================
  
  /** Get a history to use for the line editor. */
  val _hist = new FileHistory(new File(_filename))
  _hist.setIgnoreDuplicates(false)
  _hist.setMaxSize(10000)
  
  override def addHistoryLine(line: String) = {
    _hist.add(line)
    _hist.flush()
  }
  
  override def getHistoryIterator = new Iterator[String] {
    val it = _hist.entries
    def hasNext = it.hasNext
    def next = it.next.toString
  }
  
  override def getHistoryEntry(index: Int) = {
    try { 
        _hist.get(index) match {
            case null => None
            case x:Any => Some(x.toString)
        }
    }
    catch {
        case _ => None
    }
  }
  
  override def getHistoryFilename = _filename
  
  //======================================================================
  // Initialize properties for the REPL.
  //======================================================================
  
  declareProperty("showscala", "Show the Scala source for each atom.", false)
  declareProperty("usepager",
      "Use the pager when output is longer than the screen.", true)
  
  //======================================================================
  // Define the REPL control fields.
  //======================================================================
  
  /** Should execution time be printed after every command. */
  private var _timing = false
 
  //======================================================================
  // Register handlers.
  //======================================================================
  
  def showatom(prefix: String, atom: BasicAtom) {
    if (getProperty[Boolean]("showscala"))
      // This is explicitly requested output, show show it regardless of the
      // quiet setting.
      console.sendln("Scala: " + prefix + atom.toString)
    
	//////////////////// GUI changes
	if(ReplActor.guiMode) ReplActor.waitOnGUI(() => 
		ReplActor.guiActor ! ("replFormat",true)
	, "formatting on") 
//    ReplActor ! ("guiReplFormat", true, "formatting on")
	//////////////////// end GUI changes
	
    console.emitln(prefix + atom.toParseString)
	
	//////////////////// GUI changes
	if(ReplActor.guiMode) ReplActor.waitOnGUI(() => 
		ReplActor.guiActor ! ("replFormat",false)
	, "formatting off") 
//    ReplActor ! ("guiReplFormat", false, "formatting off")
	//////////////////// end GUI changes
  }
  
  this.register(
    // Register a basic handler that applies the context bindings to the
    // atom.
    new Processor.Handler {
      override def init(exec: Executor) = {
        declareProperty("showprior",
            "Show each atom prior to rewriting with the context's bindings.",
            false)
        declareProperty("applybinds",
            "Rewrite each atom with the context's bindings.", true)
        true
      }
      override def handleAtom(atom: BasicAtom) = {
        if (getProperty[Boolean]("showprior")) {
          showatom("", atom)
        }
        if (getProperty[Boolean]("applybinds")) {
          Some(atom.rewrite(context.binds)._1)
        } else {
          Some(atom)
        }
      }
    },
    
    // Register a handler that automatically defines operators (if that
    // is enabled) and stores rules.
    new Processor.Handler {
      override def init(exec: Executor) = {
        declareProperty("autoop",
            "If the current result is an operator, automatically declare it " +
            "in the operator library.", true)
        declareProperty("autorule",
            "If the current atom is a rewrite rule, automatically declare it " +
            "in the rule library.", true)
        true
      }
      override def handleAtom(atom: BasicAtom) = {
        atom match {
          case op: Operator if getProperty[Boolean]("autoop") =>
            context.operatorLibrary.add(op)
            console.emitln("Declared operator " + op.name + ".")
            None
          case rule: RewriteRule if getProperty[Boolean]("autorule") =>
            context.ruleLibrary.add(rule)
            console.emitln("Declared rule.")
            None
          case _ =>
            Some(atom)
        }
      }
    },
    
    // Register a handler to perform automated rewriting of atoms.
    new Processor.Handler {
      override def init(exec: Executor) = {
        declareProperty("autorewrite",
            "Automatically apply rules in the active rulesets to each atom" +
            "as it is evaluated.", true)
        true
      }
      override def handleAtom(atom: BasicAtom) = {
        if (getProperty[Boolean]("autorewrite")) {
          Some(context.ruleLibrary.rewrite(atom)._1)
        } else {
          Some(atom)
        }
      }
    },
    
    // Register a handler to perform round-trip testing of atoms.
    new Processor.Handler {
      override def init(exec: Executor) = {
        declareProperty("roundtrip",
            "Perform round-trip testing of atoms as they are entered.", true)
        true
      }
      override def result(atom: BasicAtom) {
        if (!getProperty[Boolean]("roundtrip")) return
        // Get the string.
        val string = atom.toParseString
        // Parse this string.
        parse(string) match {
          case ParseFailure(msg) =>
            console.error("Round trip testing failed for atom:\n  " + string +
                "\nParsing terminated with an error:\n  " + msg + "\n")
          case ParseSuccess(atoms) =>
            if (atoms.length < 1) {
              console.error("Round trip testing failed for atom:\n  " + string +
                  "\nParsing returned no atoms.")
            } else if (atoms.length > 1) {
              console.error("Round trip testing failed for atom:\n  " + string +
                  "\nParsing returned more than one atom:\n" +
                  atoms.mkParseString("  ","\n","\n"))
            } else if (atoms(0) != atom) {
              console.error("Round trip testing failed for atom:\n  " + string +
                  "\nAtom returned by parser not equal to original:\n  " +
                  atoms(0).toParseString + "\n")
            }
        }
      }
    },
    
    // Register a basic handler that discards "no show" atoms and prints
    // the result of evaluation.  This handler will also create a REPL
    // bind if that is enabled.  Because this displays the results, it
    // should be near the end of the chain.
    new Processor.Handler {
      override def init(exec: Executor) = {
        declareProperty("setreplbinds",
            "Generate $_repl numbered bindings for each atom as it is " +
            "evaluated.", true)
        declareProperty("setreplbinds.index",
            "The index of the current $_repl numbered binding.", 0)
        true
      }
      override def handleAtom(atom: BasicAtom) = {
        if (atom eq ApplyData._no_show) None
        else Some(atom)
      }
      override def result(atom: BasicAtom) = {
        // If we are binding atoms, bind it to a new REPL variable.
        if (getProperty[Boolean]("setreplbinds")) {
          // Get the current binding index.
          val index = "_repl" +
              setProperty("setreplbinds.index",
                  getProperty[Int]("setreplbinds.index")+1)
          // Commit the binding.
          context.bind(index, atom)
          showatom("$"+index+" = ", atom)
        } else {
          showatom("", atom)
        }
      }
    }
  )
  
  //======================================================================
  // Methods.
  //======================================================================
  
  /**
   * The file to load during bootstrapping.  This is the file loaded first
   * by `bootstrap` and must define everything needed by subsequent files
   * (most notably the operator and rule declaration operators, any I/O
   * operators, and any operators for including other files).
   */
  val bootstrapFile = "bootstrap/Boot.eli"
  
  /**
   * Perform bootstrapping.  This loads the file specified by `bootstrapFile`
   * and then tries to load the user's `.elisionrc` file (or another file,
   * depending on environment variables).
   * 
   * @param quiet   Quiet level when loading files.  This is 1 by default.
   * @return  True on success, and false when a failure is reported.
   */
  def bootstrap(quiet: Int = 1): Boolean = {
    // Load all the startup definitions, etc.
    console.reset
    console.quiet = quiet
    if (!read(bootstrapFile, false)) {
      // Failed to find bootstrap file.  Stop.
      console.error("Unable to load " + bootstrapFile + ".  Cannot continue.")
      return false
    }
    console.quiet = 0
    if (console.errors > 0) {
      console.error("Errors were detected during bootstrap.  Cannot continue.")
      return false
    }
    
    // User stuff.
    console.reset
    console.quiet = quiet
    console.emitln("Reading " + _rc + " if present...")
    if (read(_rc, true)) {
      if (console.errors > 0) {
        console.error("Errors were detected processing " + _rc +
            ".  Cannot continue.")
        return false
      }
    }
    console.quiet = 0
    
    // No reported errors.
    return true
  }
  
  def run() {
    // Display the banner.
    banner()

    // Start the clock.
    startTimer

    // Load all the startup definitions, etc.
    if (!bootstrap()) {
        ReplActor ! (":quit", true)
        return
    }
    
    // Report startup time.
    stopTimer
    printf("Startup Time: " + getLastTimeString + "\n")
	
    //////////////////// GUI changes
	
    // activates communications with the GUI if we are using it.
    if(ReplActor.guiMode) {
        ReplActor ! ("disableGUIComs", false)
    }
	
    //////////////////// end GUI changes
	
    // Configure the console and history.
    val cr = new ConsoleReader
    val term = cr.getTerminal
    cr.flush()
    cr.setHistory(_hist)
    
    // Start main loop.
    while(true) {
      // Hold the accumulted line.
      var line = ""
      
      // Hold the next segment read from the prompt.
      var segment = ""
        
      // A line state parser to determine when the line ends.
      val ls = new LineState
      
      // The number of blank lines seen.
      var blanks = 0
      
      // A little function to prompt for, and read, the next segment.  The
      // segment is accumulated into the line. 
      def fetchline(p1: String, p2: String): Boolean = {
        Processor.fileReadStack.clear
        Processor.fileReadStack.push("Console")
      	//////////////////// GUI changes
		segment = 	if (ReplActor.guiMode) {  
                println()
				print("" + (if (console.quiet > 0) p2 else p1))
				
				// make the Repl wait for GUI Input
				ReplActor.waitOnGUI()
				
				ReplActor.guiInput
			} 
			else {
				val line = cr.readLine(if (console.quiet > 0) p2 else p1)
				// Reset the terminal size now, if we can, and if the user wants to
				// use the pager.
				if (getProperty[Boolean]("usepager")) {
          console.height_=(
              scala.tools.jline.TerminalFactory.create().getHeight()-1)
          console.width_=(
              scala.tools.jline.TerminalFactory.create().getWidth())
				} else {
				  console.height_=(0)
				  console.width_=(0)
				}
				line
			} 
		/////////////// end GUI changes
		
		//segment = cr.readLine(if (console.quiet > 0) p2 else p1)
		
      	if (segment == null) {
      	  return true
      	}
      	segment = segment.trim()
      	
      	// Watch for blank lines that terminate the parse.
      	if (segment == "") blanks += 1 else blanks = 0
      	
      	// Capture newlines.
      	if (line != "") line += "\n"
      	line += segment
      	
      	// Process the line to determine if the input is complete.
      	ls.process(segment)
      }
      
      // Read the first segment.
      if (!fetchline("e> ", "q> ")) {
      	// Read any additional segments.  Everything happens in the while loop,
        // but the loop needs a body, so that's the zero.
        while (!fetchline(" > ", " > ") && blanks < 3) 0
	      if (blanks >= 3) {
	        console.emitln("Entry terminated by three blank lines.")
	        line = ""
	      }
      }
      
      // Watch for the end of stream or the special :quit token.
      if (segment == null || (line.trim.equalsIgnoreCase(":quit"))) {
        // turn guiMode on so that ReplActor doesn't drop the exit message. Otherwise it will never exit its thread.
        ReplActor.exitFlag = true
        ReplActor ! (":quit", true)
        return
      }
      
      // Flush the console.  Is this necessary?
      cr.flush()
      
      // Run the line.
      try {
        //////////////////// GUI changes
	
        // Create the root of our rewrite tree it contains a String of the REPL input.
        ReplActor ! ("Eva", "newTree", line) // val treeRoot = RWTree.createNewRoot(lline) 
        
        //////////////////// end GUI changes
        execute(line)
        //////////////////// GUI changes
	
        // send the completed rewrite tree to the GUI's actor
        if(ReplActor.guiActor != null && !ReplActor.disableGUIComs && line != "")
            ReplActor ! ("Eva", "finishTree", None) //ReplActor.guiActor ! treeRoot

        //////////////////// end GUI changes
      } catch {
        case ornl.elision.ElisionException(msg) =>
          console.error(msg)
        case ex: Exception =>
          console.error("(" + ex.getClass + ") " + ex.getMessage())
          if (getProperty[Boolean]("stacktrace")) ex.printStackTrace()
        case oom: java.lang.OutOfMemoryError =>
          System.gc()
          console.error("Memory exhausted.  Trying to recover...")
          val rt = Runtime.getRuntime()
          val mem = rt.totalMemory()
          val free = rt.freeMemory()
          val perc = free.toDouble / mem.toDouble * 100
          console.emitln("Free memory: %d/%d (%4.1f%%)".format(free, mem, perc))
        case th: Throwable =>
          console.error("(" + th.getClass + ") " + th.getMessage())
          if (getProperty[Boolean]("stacktrace")) th.printStackTrace()
          coredump("Internal error.", Some(th))
      }
    } // Forever read, eval, print.
  }
  
  def clean() {
    console.emitln("")
    addHistoryLine("// Ended normally: " + new java.util.Date)
    val cfile = new java.io.FileWriter(_lastcontext)
    if (cfile != null) {
      cfile.write(context.toParseString)
      cfile.flush()
      cfile.close()
    } else {
      console.warn("Unable to save context.")
    }
  }
  
}
