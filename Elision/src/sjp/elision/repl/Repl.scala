/*======================================================================
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 * The Elision Term Rewriter
 * 
 * Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com)
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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
======================================================================*/
package sjp.elision.repl

import sjp.elision.core._
import scala.collection.mutable.ListBuffer
import sjp.elision.parse.AtomParser
import jline.History
import java.io.File
import sjp.elision.ElisionException
import java.io.FileWriter

/**
 * Provide information about the current version of Elision.  This information
 * is obtained from the `configuration.xml` file expected to be in the root
 * of the current classpath (thus in the jar file).  It has the following
 * format.
 * 
 * {{{
 * <configuration
 *   name = "name of program"
 *   maintainer = "name and email of maintainer"
 *   web = "web address of program">
 *   <version
 *     major = "major version number"
 *     minor = "minor version number"
 *     build = "build date / time identifier"/>
 * </configuration>
 * }}}
 * 
 * The `loaded` field reveals if the file was successfully loaded.  If not, the
 * other fields contain default (and useless) information.  Missing fields
 * result in a value of the empty string for that field.  Extra fields not
 * specified above are ignored, but may be used in the future.
 */
object Version {
  /** Name of the program. */
  var name = "Elision"
    
  /** Name and email of the maintainer. */
  var maintainer = "<maintainer>"
    
  /** Web address for the program. */
  var web = "<web>"
    
  /** Major version number (integer string). */
  var major = "UNK"
    
  /** Minor version number (integer string). */
  var minor = "UNK"
    
  /** Tweleve digit build identifier.  Date and time. */
  var build = "UNK"
    
  /** If true then data was loaded.  If false, it was not. */
  var loaded = false

  private def init {
	  // Open the file.  We expect to find config.xml in the classpath.
	  val config_resource = getClass.getResource("/configuration.xml")
	  if (config_resource != null) {
	    // Parse the file.
	    val config = scala.xml.XML.load(config_resource.toString())
	    
	    // Pull out the data and override the local defaults.
	    name = config.\("@name").text
	    maintainer = config.\("@maintainer").text
	    web = config.\("@web").text
	    major = config.\("version").\("@major").text
	    minor = config.\("version").\("@minor").text
	    build = config.\("version").\("@build").text
	    loaded = true
	  }
  }
  init
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
object Repl {
  /** Access to system properties. */
  private val _prop = new scala.sys.SystemProperties
  
  /** The user's home folder. */
  private val _home = _prop("user.home")
  
  /** Figure out the location to store the history. */
  private val _filename = {
    val fname = (if (_prop("path.separator") == ":") ".elision"
      else "elision.ini")
    _home + _prop("file.separator") + fname
  }
  
  /** Figure out where to stash the context on exit. */
  private val _lastcontext = {
    val fname = (if (_prop("path.separator") == ":") ".elision-context.mpl2"
      else "elision.context.mpl2")
    _home + _prop("file.separator") + fname
  }
  
  /** Get a history to use for the line editor. */
  val _hist = new History(new File(_filename))
  
  /** The context to use. */
  private val _context = new Context()
  
  /** The parser to use. */
  private var _parser = new AtomParser(_context, false)
  
  /** The current set of bindings. */
  private var _binds = Bindings()
  
  /** Direct access to the context's operator library. */
  private val _library = _context.operatorLibrary
  
  /** Whether to show atoms prior to rewriting with the current bindings. */
  private var _showPrior = false
  
  /** Whether to show the Scala. */
  private var _showScala = false
  
  /** The binding number. */
  private var _bindNumber = 0
  
  /** Whether to bind the results of evaluating atoms. */
  private var _bindatoms = true
  
  /** Whether or not to trace the parser. */
  private var _trace = false
  
  /** Have we defined the operators.  This is used by the `run` method. */
  private var _opsDefined = false
  
  /** Should stack traces be issued on exception. */
  private var _stacktrace = false
  
  /** Should execution time be printed after every command. */
  private var _timing = false
  
  /**
   * Whether to suppress most printing.  Errors and warnings are not suppressed,
   * and explicitly requested output is also not suppressed.
   */
  private var _quiet = false
  
  /**
   * Whether or not rewriting is enabled.
   */
  private var _rewrite = true
  
  /** Write a message, unless quiet is enabled. */
  private def emitln(msg: String) { if (!_quiet) println(msg) }
  
  /** Emit an error message, with the ERROR prefix. */
  private def error(msg: String) { println("ERROR: " + msg) }
  
  /** Emit a warning message, with the WARNING prefix. */
  private def warn(msg: String) { println("WARNING: " + msg) }
  
  /** A special literal that we never show, or save as a binding. */
  private val _no_show = Literal(Symbol(" NO SHOW "))
  
  /**
   * The entry point when started from the command line.  Print the current
   * version number, then invoke `run`.
   * @param args	The command line arguments.
   */
  def main(args: Array[String]) {
    run
    emitln("")
    _hist.addToHistory("// Ended Normally: " + new java.util.Date)
//    emitln("Context: ")
//    emitln(_context.toParseString)
    val cfile = new FileWriter(_lastcontext)
    if (cfile != null) {
      cfile.write(_context.toParseString)
      cfile.flush()
      cfile.close()
    } else {
      warn("Unable to save context.")
    }
  }
  
  /**
   * Display the banner.
   */
  private def banner() {
    import Version._
    emitln("""|      _ _     _
							 |  ___| (_)___(_) ___  _ __
							 | / _ \ | / __| |/ _ \| '_ \
							 ||  __/ | \__ \ | (_) | | | |
							 | \___|_|_|___/_|\___/|_| |_|
							 |
							 |Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
							 |All rights reserved.""".stripMargin)
    _hist.addToHistory("// New Session: " + new java.util.Date)
    if (loaded) {
	    	emitln("Version " + major + "." + minor + ", build " + build)
	    	emitln("Web " + web)
	    	_hist.addToHistory("// Running: " + major + "." + minor +
	    	    ", build " + build)
    }
  }
  
  /**
   * Repeatedly print a prompt, obtain a line from the standard input,
   * and call `execute` to execute the line.
   * 
   * ==Interaction==
   * Interaction with the REPL is improved by using the
   * [[http://jline.sourceforge.net jLine Library]].  This allows using the
   * arrow keys to edit the line and move back and forward in the history.  It
   * also supports saving the history between uses; the history is stored
   * in the user's home folder (identified by the java property `user.home`)
   * and named `.elision`.
   * 
   * ==Commands==
   * The REPL supports several special commands.  These are processed by the
   * `execute` method, and are described in its documentation.
   */
  def run() {
    // Display the banner
    banner()
    
    // Cause the named types to be constructed.
    BOOLEAN
    
    // Define the operators.
    if (!_opsDefined) defineOps
    
    // Bootstrap now.
    val qt = _quiet
    val ba = _bindatoms
    emitln("Bootstrapping Strategies...")
    _quiet = true
    _bindatoms = false
    execute(bootstrap.Strategies.defs)
    _quiet = qt
    _bindatoms = ba
    emitln("Bootstrapping Scheme...")
    _quiet = true
    _bindatoms = false
    execute(bootstrap.Scheme.defs)
    _quiet = qt
    _bindatoms = ba

    // Show the prompt and read a line.
    val cr = new jline.ConsoleReader
    val term = cr.getTerminal
    term.initializeTerminal()
    cr.flushConsole()
    cr.setHistory(_hist)
    var starttime: Long = 0L
    var endtime: Long = 0L
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
      	segment = cr.readLine(if (_quiet) p2 else p1)
      	if (segment == null) return true
      	segment = segment.trim()
      	
		    // See if this is a history reference and, if so, fetch it.
		    if (segment.startsWith("!")) {
		      // This is a history reference.  The rest of the segment is probably
		      // a number, so try to parse it as such.
		      val num = segment.substring(1).toInt
		      try {
		      		segment = _hist.getHistory(num)
		      		println(segment)
		      } catch {
		        case ex:IndexOutOfBoundsException =>
		          error("No such history item.")
		          line = ""
		          return true
		      }
		    }
		    
      	// Watch for blank lines that terminate the parse.
      	if (segment == "") blanks += 1
      	else line += segment.trim() + " "
      	
      	// Process the line to determine if the input is complete.
      	ls.process(segment)
      }
      
      // Read the first segment.
      if (!fetchline("e> ", "q> ")) {
      	// Read any additional segments.  Everything happens in the while loop,
        // but the loop needs a body, so that's the zero.
        while (!fetchline(" > ", " > ") && blanks < 3) 0
	      if (blanks >= 3) {
	        emitln("Entry terminated by three blank lines.")
	        line = ""
	      }
      }
      
      // Watch for the end of stream or the special :quit token.
      if (line == null || (line.trim.equalsIgnoreCase(":quit"))) return
      
      // Flush the console.  Is this necessary?
      cr.flushConsole()
      
      // Record the start time.
      starttime = java.lang.System.currentTimeMillis()
      
      // Run the line.
      execute(line)
      
      // If we are reporting timing, do that now.
      if (_timing) {
        endtime = java.lang.System.currentTimeMillis()
        val elapsed = endtime - starttime
        val mins = elapsed / 60000
        val secs = (elapsed % 60000) / 1000
        val mils = elapsed % 1000
        printf("elapsed: %d:%02d.%03d\n", mins, secs, mils)
      }
    } // Forever read, eval, print.
  }

  /**
   * Execute a line.  The line can be a command known to the interpreter, or
   * a history reference.  Exceptions are captured and displayed to the
   * standard output.
   * 
   * ==Commands==
   * This method supports several special commands.
   *  - `:quit`
   *    Terminate the current session.
   *  - `help`
   *    Print help text.
   * 
   * @param line	The line to execute.
   * @param quiet	If true, suppress printing of the atoms.  Errors are still
   * 							printed.
   */
  def execute(line: String, quiet: Boolean = false) {
    // Eliminate leading and trailing whitepsace.
    var lline = line.trim
    
    // Skip blank lines.
    if (lline == "") return
    
    // Parse the line.  Handle each atom obtained.
    try {
	    val result = _parser.parseAtoms(lline)
	    result match {
	    	case _parser.Success(list) =>
	    	  // Interpret each node, and stop if we encounter a failure.
	    	  list.forall(node => handle(node.interpret))
	      case _parser.Failure(msg) => println(msg)
	    }
    } catch {
      case ElisionException(msg) =>
        error(msg)
      case ex:Exception =>
        error("(" + ex.getClass + ") " + ex.getMessage())
        if (_stacktrace) ex.printStackTrace()
    }
  }
  
  /**
   * Show an atom.  This checks the settings to decide whether to display
   * the Scala, the repl binding, etc.
   */
  private def show(atom: BasicAtom, what:Option[String] = None) {
    val pre = what match {
      case None => ""
      case Some(name) => "$" + name + " = "
    }
    if (_showScala) println("Scala: " + pre + atom)
    emitln(pre + atom.toParseString)
    checkParseString(atom)
  }
  
  /**
   * Check the atom for round-trip parsing.  That is, generate the parse
   * string and then try to parse it.  If the result is not equal to the
   * original atom, then we write an error report.
   * 
   * @param atom	The atom to check.
   */
  private def checkParseString(atom: BasicAtom) {
    // Get the parse string for the atom.
    val parseString = atom.toParseString
    
    // Parse the string.  The result should be a single atom.
    val result = _parser.parseAtoms(parseString)
    result match {
      case _parser.Success(list) =>
        // If there is more than one atom, then we have a failure.
        if (list.length == 0)
          error("Round-trip parse failure for atom " +
              atom.toString + ".  No atoms returned on parse.")
        if (list.length > 1) {
          error("Round-trip parse failure for atom " +
              atom.toString + ".  Too many atoms returned on parse.")
          for (node <- list) println(" -> " + node.interpret.toParseString)
        }
        // The result must be equal to the original atom.
        val newatom = list(0).interpret
        if (atom != newatom) {
          error("Round-trip parse failure for atom " +
              atom.toString + ".  Atom unequal to original.")
          println("  original: " + atom.toParseString)
          println("  result:   " + newatom.toParseString)
        }
      case _parser.Failure(msg) =>
        error("Round-trip parse failure for atom " + atom.toString +
            ".\nParse failed: " + msg)
    }
  }
  
  /**
   * Define the operators we need.
   */
  private def defineOps {
    // Type of.
    execute("{ operator typeof($x:$T:$U):$U = $T }")
    
    // Bind.
    execute("{ operator bind($v,$a) }")
    _context.operatorLibrary.register("bind",
        (_, list:AtomSeq, _) => list match {
          case Args(from:Variable, to:BasicAtom) =>
          	// Bind the variable in this context.
            _binds += (from.name -> to)
            emitln("Bound " + from.toParseString)
            _no_show
          case _ => _no_show
        })

    // Equal.
    execute("{ operator equal($x,$y):BOOLEAN is commutative }")
    _context.operatorLibrary.register("equal",
        (_, list:AtomSeq, _) => list match {
          case Args(x:BasicAtom, y:BasicAtom) =>
            // Check for equality.
            if (x == y) Literal.TRUE else Literal.FALSE
          case _ => _no_show
        })
        
    // Unbind.
    execute("{ operator unbind($v) }")
    _context.operatorLibrary.register("unbind",
        (_, list:AtomSeq, _) => list match {
          case Args(from:Variable) =>
          	// Unbind the variable in this context.
            _binds -= from.name
            emitln("Unbound " + from.toParseString)
            _no_show
          case _ => _no_show
        })
        
    // Showbinds.
    execute("{ operator showbinds() }")
    _context.operatorLibrary.register("showbinds",
        (_, list:AtomSeq, _) => list match {
          case Args() => {
            println(_binds.map {
              pair => "  %10s -> %s".format(toESymbol(pair._1), pair._2.toParseString)
            }.mkString("{ bind\n", ",\n", "\n}"))
            _no_show
          }
          case _ => _no_show
        })
        
    // Timing.
    execute("{ operator timing() }")
    _context.operatorLibrary.register("timing",
        (_, list:AtomSeq, _) => list match {
          case Args() => {
            _timing = !_timing
            emitln("Timing is " + (if (_timing) "ON" else "OFF") + ".")
            _no_show
          }
          case _ => _no_show
        })
        
    // Context.
    execute("{ operator context() }")
    _context.operatorLibrary.register("context",
        (_, list:AtomSeq, _) => list match {
          case Args() =>
            println(_context.toParseString)
            _no_show
          case _ => _no_show
        })
        
    // Stacktrace.
    execute("{ operator stacktrace() }")
    _context.operatorLibrary.register("stacktrace",
        (_, list:AtomSeq, _) => list match {
          case Args() =>
            _stacktrace = !_stacktrace
            emitln("Printing stack traces is " +
                (if (_stacktrace) "ON" else "OFF") + ".") 
            _no_show
          case _ => _no_show
        })
        
    // Read.
    execute("{ operator read($filename: STRING) }")
    _context.operatorLibrary.register("read",
        (_, list:AtomSeq, _) => list match {
          case Args(StringLiteral(_, filename)) =>
            // TODO Read the content of the file.
            emitln("Not implemented.")
            _no_show
          case _ => _no_show
        })
        
    // Write.
    execute("{ operator write($filename: STRING) }")
    _context.operatorLibrary.register("write",
        (_, list:AtomSeq, _) => list match {
          case Args(StringLiteral(_, filename)) =>
            // TODO Write the content to the file.
            emitln("Not implemented.")
            _no_show
          case _ => _no_show
        })
        
    // Help.
    execute("{ operator help() }")
    _context.operatorLibrary.register("help",
        (_, list:AtomSeq, _) => list match {
          case Args() =>
          	// Give some help.
		        println("""
		        			|Elision Help
	        				|
		            | bind(v,a) .................. Bind variable v to atom a.
		            | context() .................. Display contents of the current context.
		            | disable(r) ................. Disable ruleset r.
		            | enable(r) .................. Enable ruleset r.
		        			| help() ..................... Show this help text.
		            | history() .................. Show the history so far.
		            | quiet() .................... Toggle suppressing most output.
		            | rewrite() .................. Toggle automatic rewriting.
		            | setlimit(l) ................ Set the rewrite limit to l successes.
		            | showbinds() ................ Display the current set of bindings.
		            | showprior() ................ Toggle showing the unrewritten term.
		            | showrules(a) ............... Show the list of rules that apply to atom a.
		            | showscala() ................ Toggle showing the Scala term.
		            | stacktrace() ............... Toggle printing stack traces on error.
		            | tracematch() ............... Toggle match tracing.
		            | traceparse() ............... Toggle parser tracing.
		            | unbind(v) .................. Unbind variable v.
		            |
		            |Use ! followed by a number to re-execute a line from the history.
		            |
		            |To quit type :quit.
		            |""".stripMargin)
		        _no_show
          case _ => _no_show
        })
        
    // Traceparse.
    execute("{ operator traceparse() }")
    _context.operatorLibrary.register("traceparse",
        (_, list:AtomSeq, _) => list match {
          case Args() =>
		        // Toggle tracing.
		        _trace = !_trace
		        _parser = new AtomParser(_context, _trace)
		        emitln("Parser tracing is " + (if (_trace) "ON." else "OFF."))
		        _no_show
          case _ => _no_show
        })
        
    // Tracematch.
    execute("{ operator tracematch() }")
    _context.operatorLibrary.register("tracematch",
        (_, list:AtomSeq, _) => list match {
          case Args() =>
		        // Toggle tracing.
		        BasicAtom.traceMatching = !BasicAtom.traceMatching
		        emitln("Match tracing is " +
		            (if (BasicAtom.traceMatching) "ON." else "OFF."))
		        _no_show
          case _ => _no_show
        })
        
    // Showscala.
    execute("{ operator showscala() }")
    _context.operatorLibrary.register("showscala",
        (_, list:AtomSeq, _) => list match {
          case Args() =>
		        // Toggle showing the Scala term.
		        _showScala = !_showScala
		        emitln("Showing Scala is " + (if (_showScala) "ON." else "OFF."))
		        _no_show
          case _ => _no_show
        })
        
    // Showprior.
    execute("{ operator showprior() }")
    _context.operatorLibrary.register("showprior",
        (_, list:AtomSeq, _) => list match {
          case Args() =>
		        // Toggle showing the prior term.
		        _showPrior = !_showPrior
		        emitln("Showing prior term is " + (if (_showPrior) "ON." else "OFF."))
		        _no_show
          case _ => _no_show
        })
        
    // History.
    execute("{ operator history() }")
    _context.operatorLibrary.register("history",
        (_, list:AtomSeq, _) => list match {
          case Args() =>
		        // Show the history.
		        for (index <- 1 until _hist.getCurrentIndex()) {
		          println(" " + index + ": " + _hist.getHistory(index))
		        }
		        println("Persistent history is found in: " + _filename)
		        _no_show
          case _ => _no_show
        })
        
    // Quiet.
    execute("{ operator quiet() }")
    _context.operatorLibrary.register("quiet",
        (_, list:AtomSeq, _) => list match {
          case Args() =>
            // Enable quiet.
            _quiet = !_quiet
            emitln("Quiet is " + (if (_quiet) "enabled." else "disabled."))
            _no_show
          case _ => _no_show
        })
        
    // Enable a ruleset.
    execute("{ operator enable($x: SYMBOL) }")
    _context.operatorLibrary.register("enable",
        (_, list:AtomSeq, _) => list match {
          case Args(SymbolLiteral(_, sym)) =>
            // Enable the specified ruleset.
            _context.enableRuleset(sym.name)
            _no_show
          case _ => _no_show
        })
        
    // Disable a ruleset.
    execute("{ operator disable($x: SYMBOL) }")
    _context.operatorLibrary.register("disable",
        (_, list:AtomSeq, _) => list match {
          case Args(SymbolLiteral(_, sym)) =>
            // Disable the specified ruleset.
            _context.disableRuleset(sym.name)
            _no_show
          case _ => _no_show
        })
        
    // Set the limit on automatic rewrites.
    execute("{ operator setlimit($limit: INTEGER) }")
    _context.operatorLibrary.register("setlimit",
        (_, list:AtomSeq, _) => list match {
          case Args(IntegerLiteral(_, count)) =>
            // Enable the specified ruleset.
            _context.setLimit(count)
            _no_show
          case _ => _no_show
        })
        
    // Enable or disable the rewriter.
    execute("{ operator rewrite() }")
    _context.operatorLibrary.register("rewrite",
        (_, list:AtomSeq, _) => list match {
          case Args() =>
            // Toggle rewriting.
            _rewrite = !_rewrite
            emitln("Automatic rewriting is " +
                (if (_rewrite) "ON." else "OFF."))
            _no_show
          case _ => _no_show
        })
        
    // See what rules are in scope.
    execute("{ operator showrules($atom) }")
    _context.operatorLibrary.register("showrules",
        (_, list:AtomSeq, _) => list match {
          case Args(atom) =>
            // Get the rules, and print each one.
            for (rule <- _context.getRules(atom)) {
              println(rule.toParseString)
            }
            _no_show
          case _ => _no_show
        })
        
    // Define mod as a Symbolic operator.
    execute("{ operator mod($b: INTEGER, $d: INTEGER): INTEGER }")
    _context.operatorLibrary.register("mod",
        (op: Operator, args: AtomSeq, _) => args.atoms match {
          case Args(IntegerLiteral(_, x), IntegerLiteral(_, y)) => x mod y
          case _ => Apply(op, args, true)
        })
        
    // Define neg as a Symbolic operator.
    execute("{ operator neg($x: INTEGER): INTEGER }")
    _context.operatorLibrary.register("neg",
        (op: Operator, args: AtomSeq, _) => args match {
          case Args(IntegerLiteral(_, x)) => -x
          case _ => Apply(op, args, true)
        })
        
    // Define add as a Symbolic operator.
    execute("""{ operator add($x: INTEGER, $y: INTEGER): INTEGER
        is associative, commutative, identity(0) }""")
		_context.operatorLibrary.register("add",
      (op: Operator, args: AtomSeq, _) => {
			  // Accumulate the integer literals found.
			  var lits:BigInt = 0
			  // Accumulate other atoms found.
			  var other = IndexedSeq[BasicAtom]()
			  // Traverse the list and divide the atoms.
			  args.foreach {
			    x => x match {
			      case IntegerLiteral(_, value) => lits += value
			      case _ => other :+= x
			    }
			  }
			  // Now add the accumulated literals to the list.
			  other :+= Literal(INTEGER, lits)
			  // Construct and return a new operator application.
			  Apply(op, AtomSeq(NoProps, other), true)
      })
      
    // Is bindable.
    execute("{ operator is_bindable($x): BOOLEAN }")
    _context.operatorLibrary.register("is_bindable",
        (op: Operator, args: AtomSeq, _) => args match {
          case Args(term) => if (term.isBindable) Literal.TRUE else Literal.FALSE
          case _ => Apply(op, args, true)
        })
        
    // The operator are defined.
    _opsDefined = true
  }
  
  /**
   * Decide what to do about an atom we just parsed.  This is where most of
   * the commands get processed, operators get added to the library, etc.
   * @param atom	An atom just parsed.
   * @return	True on success, false on failure.
   */
  private def handle(atom: BasicAtom): Boolean = {
    // If we come here with the special "no show" literal, we skip all of this.
    if (atom == _no_show) return true
    
    // Certain atoms require additional processing.
    atom match {
      case od:OperatorDefinition =>
      	// Put operator definitions in the operator library.
        _library.add(od)
        return true
        
      case _ =>
        // Maybe show the atom before we rewrite.
        if (_showPrior) show(atom)
		    // Apply the global bindings to get the possibly-rewritten atom.
		    var (newatom,_) = atom.rewrite(_binds)
		    // Rewrite it using the active rulesets of the context.
		    if (_rewrite) newatom = _context.rewrite(newatom)._1
		    if (_bindatoms) {
			    // Get the next bind variable name.
			    _binds += ("repl"+_bindNumber -> newatom)
			    // Now write out the new atom.
			    show(newatom, Some("repl" + _bindNumber))
			    _bindNumber += 1
		    } else {
		      show(newatom, None)
		    }
		    // Rules go in the rule library if they have declared rulesets.
		    newatom match {
		      case rule:RewriteRule =>
		      		// Rules go in the rule library.
		      		if (!rule.rulesets.isEmpty) {
		      			_context.add(rule)
		      			emitln("Rule stored in library.")
		      		}
		      case _ =>
        }
        return true
    }
  }
}
