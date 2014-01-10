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

import java.io.File
import ornl.elision.actors.ReplActor
import ornl.elision.context.Context
import ornl.elision.context.Executor
import ornl.elision.cli.ArgSwitch
import ornl.elision.cli.Setting
import ornl.elision.cli.CLI
import ornl.elision.cli.Switch
import ornl.elision.parse.Processor
import ornl.elision.parse.ProcessorControl
import ornl.elision.util.Loc

/**
 * Implement an interface to run the REPL from the prompt.
 */
object ReplMain {
  
  /** Access to system properties. */
  private val _prop = new scala.sys.SystemProperties
  
  /** Iff true then the prior context should be loaded. */
  var _wantPrior = false
  
  /**
   * If compilation is desired set this to `Some(`fn`)`, where fn is the
   * output file name.
   */
  var _wantCompile: Option[String] = None
  
  /**
   * If true, force a call to exit at the end of main to terminate all threads.
   */
  private var _forceExit = false
  
  /**
   * Print usage information.  This is a switch handler (see
   * [[ornl.elision.cli.Switches]]) and satisfies the contract for such a
   * method.
   * 
   * @return  Always `None`.
   */
  private def _usage(): Option[String] = {
    println("Usage:")
    println("repl")
    println()
    CLI(_switches, _settings)
    System.exit(0)
    None
  }
  
  /**
   * Define the switches.
   */
  private val _switches = Seq(
      Switch(Some("help"), Some('h'), "Provide basic usage information.", _usage _),
      Switch(Some("noboot"), Some('N'), "Suppress bootstrapping.",
          () => {
            ProcessorControl.bootstrap = false
            None
          }),
      Switch(Some("exit"), Some('x'), "Force exit at end of main.",
          () => {
            _forceExit = true
            None
          }),
      Switch(Some("prior"), Some('p'), "Attempt to re-load the prior context.",
        () => {
          ProcessorControl.bootstrap = false
          _wantPrior = true
          None
        }),
      ArgSwitch(Some("compile"), Some('C'),
          "After startup, generate a compilable Scala file for the resulting " +
          "context and exit.", "FILENAME",
          (arg) => {
            _wantCompile = Some(arg)
            None
          }))
      
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
          Some("elision-history.eli"),
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
  
  /**
   * Entry point when run from the prompt.  If you just want to read the
   * settings and not start the REPL, use `prep`.
   * 
   * @param args  The command line arguments.
   */
  def main(args: Array[String]) {
    prep(args) match {
      case None =>
      case Some(sets) => runRepl(sets)
    }
    if (_forceExit) {
      System.exit(0)
    }
  }
  
  /**
   * Prepare by reading the arguments.  This handles the generic startup case
   * and provides use of the "usual" switches.
   * 
   * @param args  Command line arguments.
   * @return  An optional settings.  If `None`, then an error was found, and
   *          control should stop. 
   */
  def prep(args: Array[String]) = {
    val state = CLI(args, _switches, _settings, false)
    // Check for an error, and display it if we find one.  We then stop.
    if (state.errstr != None) {
      // There was an actual error!
      CLI.fail(args, state.errindex, state.errstr.get)
      None
    } else {
      Some(state)
    }
  }
  
  /**
   * Start the REPL.
   * 
   * @param settings  Settings overrides.
   */
  def runRepl(settings: CLI.CLIState) {
    val erepl = new ERepl(settings)
    ornl.elision.core.knownExecutor = erepl
    
    ReplActor.start
    ReplActor.history = erepl
    ReplActor.console = erepl.console
    ReplActor ! ("disableGUIComs", true)
    try {
      erepl.run()
    } catch {
      case th: Throwable =>
        erepl.console.error("(" + th.getClass + ") " + th.getMessage())
        if (erepl.getProperty[Boolean]("stacktrace")) 
          th.printStackTrace()
        erepl.coredump("Internal error.", Some(th))
    }
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
 * 
 * @param state   State data from parsing the command line.
 */
class ERepl(state: CLI.CLIState = CLI.CLIState())
extends Processor(state.settings) {
  import ornl.elision.core._
  import scala.tools.jline.console.history.FileHistory
  import scala.tools.jline.console.ConsoleReader
  import java.io.{File, FileWriter, FileReader, BufferedReader}

  //======================================================================
  // Figure out where to read and store the history, and where to store
  // the last context.
  //======================================================================

  /** Access to system properties. */
  private val _prop = new scala.sys.SystemProperties
  private val _sep = _prop.get("file.separator")
  
  /** The user's home folder. */
  private val _home = new File(settings("elision.root")).toString
  
  /** Figure out the location to store the history. */
  protected val _filename =
    new File(_home, settings("elision.history")).toString
  
  /** Figure out where to stash the context on exit. */
  protected val _lastcontext =
    new File(_home, settings("elision.context")).toString
  
  /** Figure out the startup file that is read after bootstrapping. */
  protected val _rc = new File(_home, settings("elision.rc")).toString
  
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
    } catch {
        case _: Throwable => None
    }
  }
  
  override def getHistoryFilename = _filename
  
  override def getPreviousHistoryEntry = {
    try { 
        _hist.previous
        _hist.current match {
            case null => None
            case x:Any => Some(x.toString)
        }
    } catch {
        case _: Throwable => None
    }
  }
  
  override def getNextHistoryEntry = {
    try { 
        _hist.next
        _hist.current match {
            case null => None
            case x:Any => Some(x.toString)
        }
    } catch {
        case _: Throwable => None
    }
  }
  
  //======================================================================
  // Initialize properties for the REPL.
  //======================================================================
  
  declareProperty("showscala", "Show the Scala source for each atom.", false)
  declareProperty("usepager",
      "Use the pager when output is longer than the screen.", true)
  declareProperty("syntaxcolor", "Use syntax-based coloring of atoms where " +
      "it is supported.", true)
  
  //======================================================================
  // Define the REPL control fields.
  //======================================================================
      
  // None yet.
 
  //======================================================================
  // Register handlers.
  //======================================================================
  
  def showatom(prefix: String, atom: BasicAtom) {
    if (getProperty[Boolean]("showscala")) {
      // This is explicitly requested output, show show it regardless of the
      // quiet setting.
      console.sendln("Scala: " + prefix + atom.toString)
    }
    
    if(ReplActor.guiActor != null) {
      ReplActor ! ("syntaxcolor", true)
      ReplActor.waitForGUI("formatting on")
    }
    
    if(getProperty[Boolean]("syntaxcolor")) {
      // color-format the atom's parseString and print it.
      val formatCols = console.width
      val formatRows = console.height
      val atomParseString = ConsoleStringFormatter.format(
          prefix + atom.toParseString, formatCols)
      ornl.elision.util.AnsiPrintConsole.width = formatCols
      ornl.elision.util.AnsiPrintConsole.height = formatRows
      ornl.elision.util.AnsiPrintConsole.quiet = console.quiet
      ornl.elision.util.AnsiPrintConsole.emitln(atomParseString)
    } else {
      // use the standard printing console and print without syntax coloring.
      console.emitln(prefix + atom.toParseString)
    }
    
    if(ReplActor.guiActor != null) {
      ReplActor ! ("syntaxcolor", false)
      ReplActor.waitForGUI("formatting off")
    }
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
          val na = atom.rewrite(context.binds)
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
            "in the operator library.", false)
        declareProperty("autorule",
            "If the current atom is a rewrite rule, automatically declare it " +
            "in the rule library.", false)
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
        parse("", string) match {
          case Dialect.Failure(_, msg) =>
            console.error("Round trip testing failed for atom:\n  " + string +
                "\nParsing terminated with an error:\n  " + msg + "\n")
                
          case Dialect.Success(atoms) =>
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
                  atoms(0).toParseString)
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
    // See if we are told not to bootstrap.
    if (! ProcessorControl.bootstrap) return true
    
    // Save the prior quiet value so we can restore it later.
    val priorquiet = console.quiet
    console.reset
    console.quiet = quiet
    
    // Bootstrap!
    val retval = _bootstrap()
    
    // Done.  Restore quiet level.
    console.quiet = priorquiet
    return retval
  }
  
  /**
   * Perform bootstrapping.  This loads the file specified by `bootstrapFile`
   * and then tries to load the user's `.elisionrc` file (or another file,
   * depending on environment variables).
   * 
   * @return  True on success, and false when a failure is reported.
   */
  private def _bootstrap(): Boolean = {
    // Load all the startup definitions, etc.
    if (!read(bootstrapFile, false)) {
      // Failed to find bootstrap file.  Stop.
      console.error("Unable to load " + bootstrapFile + ".  Cannot continue.")
      return false
    }
    if (console.errors > 0) {
      console.error("Errors were detected during bootstrap.  Cannot continue.")
      return false
    }
    
    // User stuff.
    console.emitln("Reading " + _rc + " if present...")
    if (read(_rc, true)) {
      if (console.errors > 0) {
        console.error("Errors were detected processing " + _rc +
            ".  Cannot continue.")
        return false
      }
    }
    
    // No reported errors.
    return true
  }
  
  def run() {
    // Display the banner.
    banner()

    // Start the clock.
    startTimer
    
    // Bootstrap.  If there are errors, then quit.
    if (! bootstrap()) {
      return
    }
    
    // May need to reload prior context.  If so, do that first.
    val list = if (ReplMain._wantPrior) {
      _lastcontext +: state.remain
    } else {
      state.remain
    }
    
    // Read any files specified on the command line.  We do this after any
    // bootstrapping so the files are read even if we tell the system not to
    // perform bootstrapping.
    for (filename <- list) {
      console.emitln("Reading " + filename + "...")
      val priorquiet = console.quiet
      console.quiet = 1
      val result = read(filename, true)
      console.quiet = priorquiet
      if (result) {
        if (console.errors > 0) {
          console.error("Errors were detected processing " + filename +
              ".  Cannot continue.")
          return
        }
      }
    } // Read the files specified on the command line.
    
    // Report startup time.
    stopTimer
    console.emitf("Startup Time: " + getLastTimeString + "\n")
    SymbolicOperator.reportTime
    
    // If a compilable context is desired, generate it and stop. */
    ReplMain._wantCompile match {
      case None =>
      case Some(fn) =>
        console.emitln("Writing compilable context as: " + fn)
        ContextGenerator.generate(fn, context)
        console.emitln("Done.")
        return
    }
  
    // activates communications with the GUI if we are using it.
    if(ReplActor.guiActor != null) {
      ReplActor ! ("disableGUIComs", false)
    }
    
    // Configure the console and history.
    val cr = new ConsoleReader
    val term = cr.getTerminal
    cr.flush()
    cr.setHistory(_hist)
    
    // Start main loop.
    while(true) {
      // Hold the accumulated line.
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

        segment = if (ReplActor.guiActor != null) {  
          // Get input from the GUI.            
          ReplActor.readLine(if (console.quiet > 0) p2 else p1)
        } else {
          // Get input directly from the console. 
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
        while (!fetchline(" > ", " > ") && blanks < 3) {}
        if (blanks >= 3) {
          console.emitln("Entry terminated by three blank lines.")
          line = ""
        }
      }
        
      // Watch for the end of stream or the special :quit token.
      if (segment == null || (line.trim.equalsIgnoreCase(":quit"))) {
        // Tell the ReplActor to exit its thread first.
        ReplActor.exitFlag = true
        ReplActor ! (":quit", true)
        return
      }
        
      // Flush the console.  Is this necessary?
      cr.flush()
        
      // Run the line.
      try {
        execute("(console)", line)
      } catch {
        case ornl.elision.util.ElisionException(loc, msg) =>
          console.error(loc, msg)
          
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
          if (getProperty[Boolean]("stacktrace")) 
            th.printStackTrace()
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
