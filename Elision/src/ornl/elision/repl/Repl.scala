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
package ornl.elision.repl

import ornl.elision.core._
import scala.collection.mutable.ListBuffer
import ornl.elision.parse.AtomParser
import scala.tools.jline.console.history.FileHistory
import scala.tools.jline.console.ConsoleReader
import java.io.File
import ornl.elision.ElisionException
import java.io.{FileWriter, FileReader, BufferedReader}
import ornl.elision.core.OperatorLibrary

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
	//////////////////// GUI changes
	
	private var _disableGUIComs = true
	
	//////////////////// end GUI changes

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
  val _hist = new FileHistory(new File(_filename))
  _hist.setIgnoreDuplicates(false)
  _hist.setMaxSize(10000)
  
  /** Direct access to the context's operator library. */
  private val _library = new OperatorLibrary(allowRedefinition = true)
  
  /** The context to use. */
  private val _context = new Context()
  _context.operatorLibrary = _library
  
  /** The parser to use. */
  private var _parser = new AtomParser(_context, true, false)
  
  /** Whether to show atoms prior to rewriting with the current bindings. */
  private var _showPrior = false
  
  /** Whether to show the Scala. */
  private var _showScala = false
  
  /** The binding number. */
  private var _bindNumber = 0
  
  /** Whether to bind the results of evaluating atoms. */
  private var _bindatoms = true
  
  /** Whether or not to trace the parboiled parser. */
  private var _trace = false

  /** Toggle which parser to use. */
  private var _toggleParser = false
  
  /** Have we defined the operators.  This is used by the `run` method. */
  private var _opsDefined = false
  
  /** Should stack traces be issued on exception. */
  private var _stacktrace = false
  
  /** Should execution time be printed after every command. */
  private var _timing = false
  
  /** Should we do round-trip parsing. */
  private var _doRoundTrip = false
  
  /**
   * Whether to suppress most printing.  Errors and warnings are not suppressed,
   * and explicitly requested output is also not suppressed.
   */
  private var _quiet = false
  
  /**
   * Whether or not rewriting is enabled.
   */
  private var _rewrite = true
  
  /** Whether we automatically define operators we encounter. */
  private var _autodefine = false
  
  private var _included = Set[String]()
  
  /** Write a message, unless quiet is enabled. */
  private def emitln(msg: String) { if (!_quiet) println(msg) }
  
  /** Emit an error message, with the ERROR prefix. */
  private def error(msg: String) { println("ERROR: " + msg) }
  
  /** Emit a warning message, with the WARNING prefix. */
  private def warn(msg: String) { println("WARNING: " + msg) }
  
  /**
   * The entry point when started from the command line.  Print the current
   * version number, then invoke `run`.
   * @param args	The command line arguments.
   */
  def main(args: Array[String]) {
    run
    emitln("")
    _hist.add("// Ended Normally: " + new java.util.Date)
    _hist.flush()
    val cfile = new FileWriter(_lastcontext)
    if (cfile != null) {
      cfile.write(_context.toParseString)
      cfile.flush()
      cfile.close()
    } else {
      warn("Unable to save context.")
    }
  }
  
  /** Get the current context. */
  def context = _context
  
  /**
   * Save the current context in response to an exception or other
   * unrecoverable error.
   * 
   * @param msg		A human-readable message.
   * @param th		An optional throwable.
   */
  private def _coredump(msg: String, th: Option[Throwable] = None) {
    val cfile = new FileWriter("elision.core")
    if (cfile != null) {
      val binds = <binds>{_context.binds.toParseString}</binds>
      val ops = <operator-library>{_context.operatorLibrary.toParseString}</operator-library>
      val rules = <rule-library>{_context.ruleLibrary.toParseString}</rule-library>
      val err = th match {
        case None => <error/>
        case Some(ex) =>
          <error message={ex.getMessage}>{
            ex.getStackTrace map { item =>
              <item>{item}</item>
            }
          }</error>
      }
      val date = (new java.util.Date).toString
      val hist = <history>{
        val buf = new StringBuffer()
        val it = _hist.entries
        while (it.hasNext) buf.append(it.next).append('\n')
        buf.toString
      }</history>
      val all = <elision-core when={date} msg={msg}>
      		{err}
      		{binds}
      		{ops}
      		{rules}
      		{hist}
      		</elision-core>
  		scala.xml.XML.write(cfile,all,"utf-8",true,null)
      //cfile.write(new scala.xml.PrettyPrinter(80, 2).format(all))
      cfile.flush()
      cfile.close()
      emitln("Wrote core dump to elision.core.")
    } else {
      warn("Unable to save core dump.")
    }
  } 
  
  /**
   * Display the banner.
   */
  def banner() {
    import ornl.elision.parse.Version._
    emitln("""|      _ _     _
							 |  ___| (_)___(_) ___  _ __
							 | / _ \ | / __| |/ _ \| '_ \
							 ||  __/ | \__ \ | (_) | | | |
							 | \___|_|_|___/_|\___/|_| |_|
							 |
							 |Copyright (c) 2012 by UT-Battelle, LLC.
							 |All rights reserved.""".stripMargin)
    _hist.add("// New Session: " + new java.util.Date)
    if (loaded) {
	    	emitln("Version " + major + "." + minor + ", build " + build)
	    	emitln("Web " + web)
	    	_hist.add("// Running: " + major + "." + minor +
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
   * The REPL supports several special commands.  These are procesed by the
   * `execute` method, and are described in its documentation.
   */
  def run() {
    // Display the banner
    banner()

    // Start the clock.
    var starttime = java.lang.System.currentTimeMillis()

    // Define the operators.
    val qt = _quiet
    val ba = _bindatoms
    _quiet = true
    _bindatoms = false
    defineOps
    _quiet = qt
    _bindatoms = ba
    
    // Bootstrap now.
//    emitln("Bootstrapping Strategies...")
//    _quiet = true
//    _bindatoms = false
//    execute(bootstrap.Strategies.defs)
//    _quiet = qt
//    _bindatoms = ba
//    emitln("Bootstrapping Scheme...")
//    _quiet = true
//    _bindatoms = false
//    execute(bootstrap.Scheme.defs)
//    _quiet = qt
//    _bindatoms = ba

    // Show the prompt and read a line.
	
	//////////////////// GUI changes
	
	// activates communications with the GUI if we are using it.
	if(ReplActor.guiMode) {
		_disableGUIComs = false
		ReplActor.start
	}
	
	//////////////////// end GUI changes
	
    val cr = new ConsoleReader
    val term = cr.getTerminal
    cr.flush()
    cr.setHistory(_hist)
    
    // Report startup time.
    var endtime = java.lang.System.currentTimeMillis()
    val elapsed = endtime - starttime
    val mins = elapsed / 60000
    val secs = (elapsed % 60000) / 1000
    val mils = elapsed % 1000
    printf("Startup Time: %d:%02d.%03d\n", mins, secs, mils)
    
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
        try {
			//////////////////// GUI changes
        	segment = 	if (ReplActor.guiMode) {  
					print("" + (if (_quiet) p2 else p1))
					
					ReplActor.waitingForGuiInput = true	// make the Repl wait for GUI Input
					while(ReplActor.waitingForGuiInput) { Thread.sleep(100)} // sleep until the REPL receives input from the GUI
					
					ReplActor.guiInput
				} 
				else {
					cr.readLine(if (_quiet) p2 else p1)
				} 
			/////////////// end GUI changes
        	
			// segment = cr.readLine(if (_quiet) p2 else p1)
        } catch {
          case ex:IllegalArgumentException =>
            error(ex.getMessage())
            return true
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
        while (!fetchline(" > ", " > ") && blanks < 3) 0
	      if (blanks >= 3) {
	        emitln("Entry terminated by three blank lines.")
	        line = ""
	      }
      }
      
      // Watch for the end of stream or the special :quit token.
      if (segment == null || (line.trim.equalsIgnoreCase(":quit"))) {
		//////////////////// GUI changes
		if(ReplActor.guiActor != null)  ReplActor.guiActor ! "quit"
		//////////////////// end GUI changes
		
        return
      }
      
      // Flush the console.  Is this necessary?
      cr.flush()
      
      // Record the start time.
      starttime = java.lang.System.currentTimeMillis()
      
      // Run the line.
      execute(line)
      //println(scala.tools.jline.TerminalFactory.create().getWidth())
      
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
   * This method supports several special commands.  Prefix these with a colon.
   *  - `:quit`
   *    Terminate the current session.
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
    
	//////////////////// GUI changes
	
	// Create the root of our rewrite tree it contains a String of the REPL input.
	val treeRoot = new RWTreeNode(lline)
	
	//////////////////// end GUI changes
	
    // If the line is a history reference, go and look it up now.
    if (lline.startsWith("!")) {
      // This is a history reference, so go and get it.
      val num = lline.substring(1).trim.toInt
      val prior = _hist.get(num)
      if (prior == null) {
        error("No such history entry: " + line)
        return
      }
      lline = prior.toString
      emitln(lline)
    }
    
    // Parse the line.  Handle each atom obtained.
    try {
	    val result = _parser.parseAtoms(lline)
	    result match {
			
			//////////////////// GUI changes
			
	    	case AtomParser.Success(list) => {
	    	  // Interpret each node, and stop if we encounter a failure.
	    	  list.forall(node => {
					RWTree.current = treeRoot.addChild("line node")
					handle(node.interpret)
				} )
			}
			
			//////////////////// end GUI changes
			/*
	    	case _parser.Success(list) =>
	    	  // Interpret each node, and stop if we encounter a failure.
	    	  list.forall(node => handle(node.interpret))*/
	      case AtomParser.Failure(msg) => println(msg)
		  
	    }
    } catch {
      case ElisionException(msg) =>
        error(msg)
      case ex:Exception =>
        error("(" + ex.getClass + ") " + ex.getMessage())
        if (_stacktrace) ex.printStackTrace()
      case oom: java.lang.OutOfMemoryError =>
        System.gc()
        error("Memory exhausted.  Trying to recover...")
        val rt = Runtime.getRuntime()
        val mem = rt.totalMemory()
        val free = rt.freeMemory()
        val perc = free.toDouble / mem.toDouble * 100
        emitln("Free memory: %d/%d (%4.1f%%)".format(free, mem, perc))
      case th:Throwable =>
        error("(" + th.getClass + ") " + th.getMessage())
        if (_stacktrace) th.printStackTrace()
        _coredump("Internal error.", Some(th))
    }
	
	//////////////////// GUI changes
	
	// send the completed rewrite tree to the GUI's actor
	
	if(ReplActor.guiActor != null && !_disableGUIComs && lline != "")
		ReplActor.guiActor ! treeRoot
	
	//////////////////// end GUI changes
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
    if (_doRoundTrip) checkParseString(atom)
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
    
    // Turn everything off!
    val qt = _quiet
    val tp = _trace
    val tm = BasicAtom.traceMatching
    _quiet = true
    _trace = false
    BasicAtom.traceMatching = false
    
    // Parse the string.  The result should be a single atom.
    val result = _parser.parseAtoms(parseString)
    
    // Restore!
    BasicAtom.traceMatching = tm
    _trace = tp
    _quiet = qt
    
    // Check the result.
    result match {
      case AtomParser.Success(list) =>
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
      case AtomParser.Failure(msg) =>
        error("Round-trip parse failure for atom " + atom.toString +
            ".\nParse failed: " + msg)
    }
  }
  
  /**
   * Define the operators we need.
   */
  def defineOps {
    if (_opsDefined) return
    
    // Bootstrap.  To get started, we need a way to define operators and put
    // them in the operator library.  So, first, define that operator.
    val defOper = TypedSymbolicOperator("def", NONE, AtomSeq(NoProps, 'op),
        "Add the named operator to the context.",
        """|This will add the operator op to the current context.  This makes
           |the operator available by name, or through an OPREF.
        """.stripMargin, true)
    _context.operatorLibrary.add(defOper)
    _context.operatorLibrary.register("def",
        _data => _data.args match {
          case Args(op: Operator) =>
            // Add the operator to the library.
            _context.operatorLibrary.add(op)
            emitln("Defined operator " + toESymbol(op.name) + ".")
            ApplyData._no_show
          case Args(op) =>
            error("Atom is not a named operator: " + op.toParseString)
            ApplyData._no_show
          case _ =>
            ApplyData._no_show
        })
        
    // Go get the buildin operators and define them.
    execute(BuiltinOperators.text.text)
    
    // Get an operator from its reference.
    _context.operatorLibrary.register("getop",
        _data => _data.args match {
          case Args(opref: OperatorRef) =>
            // Get the referenced operator.
            opref.operator
          case _ => ApplyData._no_show
        })
        
    // Enable or disable DeBruijn indices.
    _context.operatorLibrary.register("setdebruijn",
        _data => _data.args match {
          case Args(BooleanLiteral(_, flag)) =>
            // Set whether to use De Bruijn indices.
            Lambda.useDeBruijnIndices = flag
            emitln("De Bruijn rewriting is " + (if (flag) "ON." else "OFF."))
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // See if an atom is bindable or not.
    _context.operatorLibrary.register("is_bindable",
        _data => _data.args match {
          case Args(term) =>
            if (term.isBindable) Literal.TRUE else Literal.FALSE
          case _ =>
            _data.as_is
        })
    
    // Fast evaluation.
    _context.operatorLibrary.register("eval",
        _data => _data.args match {
          case Args(x) =>
            // Immediately rewrite this with the context bindings,
            // and return the result.
            x.rewrite(context.binds)._1
          case _ =>
            NONE
        })
    
    // Help on an operator.
    _context.operatorLibrary.register("_help_op",
        _data => _data.args match {
          case Args(or: OperatorRef) =>
            // Give some help.
            emitln(context.operatorLibrary.help(new StringBuffer(), or).toString)
            ApplyData._no_show
          case _ =>
            ApplyData._no_show
        })
        
    // Help on all operators.
    _context.operatorLibrary.register("_help_all",
        _data => _data.args match {
          case Args() =>
            // Give some help.
            val width = scala.tools.jline.TerminalFactory.create().getWidth()
            println("Elision Help\n")
            emitln(context.operatorLibrary.help(
                new StringBuffer(), width).toString)
            println("Use ! followed by a number to re-execute a " +
                "line from the history.\n\nTo quit type :quit.\n")
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
    
    // Bind.
    _context.operatorLibrary.register("bind",
        _data => _data.args match {
          case Args(from:Variable, to:BasicAtom) =>
          	// Bind the variable in this context.
            context.bind(from.name, to)
            emitln("Bound " + from.toParseString)
            ApplyData._no_show
          case _ => ApplyData._no_show
        })

    // Equal.
    _context.operatorLibrary.register("equal",
        _data => _data.args match {
          case Args(x:BasicAtom, y:BasicAtom) =>
            // Check for equality.
            if (x == y) Literal.TRUE else Literal.FALSE
          case _ => ApplyData._no_show
        })
        
    // Unbind.
    _context.operatorLibrary.register("unbind",
        _data => _data.args match {
          case Args(from:Variable) =>
          	// Unbind the variable in this context.
            context.unbind(from.name)
            emitln("Unbound " + from.toParseString)
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Showbinds.
    _context.operatorLibrary.register("showbinds",
        _data => _data.args match {
          case Args() => {
            println(context.binds.filterKeys(!_.startsWith("_")).map {
              pair => "  %10s -> %s".format(toESymbol(pair._1), pair._2.toParseString)
            }.mkString("{ bind\n", ",\n", "\n}"))
            ApplyData._no_show
          }
          case _ => ApplyData._no_show
        })
        
    // Timing.
    _context.operatorLibrary.register("timing",
        _data => _data.args match {
          case Args() => {
            _timing = !_timing
            emitln("Timing is " + (if (_timing) "ON" else "OFF") + ".")
            ApplyData._no_show
          }
          case _ => ApplyData._no_show
        })
        
    // Context.
    _context.operatorLibrary.register("context",
        _data => _data.args match {
          case Args() =>
            println(_context.toParseString)
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Stacktrace.
    _context.operatorLibrary.register("stacktrace",
        _data => _data.args match {
          case Args() =>
            _stacktrace = !_stacktrace
            emitln("Printing stack traces is " +
                (if (_stacktrace) "ON" else "OFF") + ".") 
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Read.
    _context.operatorLibrary.register("read",
        _data => _data.args match {
          case Args(StringLiteral(_, filename)) =>
            val qt = _quiet
            val ba = _bindatoms
            val cfile = new BufferedReader(new FileReader(filename))
            if (cfile != null) {
              var buf = new StringBuilder
              var go = true
              while (go) {
                val line = cfile.readLine
                if (line != null) buf.append(line).append('\n') else go = false
              }
              _quiet = true
              _bindatoms = false
              execute(buf.toString)
              println(buf)
              cfile.close()
            } else {
              error("Unable to open file.")
            }
            _quiet = qt
            _bindatoms = ba
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Include.
    _context.operatorLibrary.register("read_once",
        _data => _data.args match {
          case Args(StringLiteral(_, filename)) =>
            if (!_included.contains(filename)) {
              _included += filename
	            val qt = _quiet
	            val ba = _bindatoms
	            val cfile = new BufferedReader(new FileReader(filename))
	            if (cfile != null) {
	              var buf = new StringBuilder
	              var go = true
	              while (go) {
	                val line = cfile.readLine
	                if (line != null) buf.append(line).append('\n') else go = false
	              }
	              _quiet = true
	              _bindatoms = false
	              execute(buf.toString)
	              println(buf)
	              cfile.close()
	            } else {
	              error("Unable to open file.")
	            }
	            _quiet = qt
	            _bindatoms = ba
            }
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Write.
    _context.operatorLibrary.register("write",
        _data => _data.args match {
          case Args(StringLiteral(_, filename)) =>
				    val cfile = new FileWriter(filename)
				    if (cfile != null) {
				      cfile.write(_context.toParseString)
				      cfile.flush()
				      cfile.close()
				    } else {
				      error("Unable to save context.")
				    }
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Traceparse.
    _context.operatorLibrary.register("traceparse",
        _data => _data.args match {
          case Args() =>
		        // Toggle tracing.
		        _trace = !_trace
		        _parser = new AtomParser(_context, _toggleParser, _trace)
		        emitln("Parser tracing is " + (if (_trace) "ON." else "OFF."))
		        ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Tracematch.
    _context.operatorLibrary.register("tracematch",
        _data => _data.args match {
          case Args() =>
		        // Toggle tracing.
		        BasicAtom.traceMatching = !BasicAtom.traceMatching
		        emitln("Match tracing is " +
		            (if (BasicAtom.traceMatching) "ON." else "OFF."))
		        ApplyData._no_show
          case _ => ApplyData._no_show
        })
 
    // ToggleParser.
    _context.operatorLibrary.register("toggleparser",
        _data => _data.args match {
          case Args() =>
		        // Toggle the parser used.
            _toggleParser = !_toggleParser
            _parser = new AtomParser(_context, _toggleParser, _trace)
            emitln("Using "+ (if(_toggleParser) "packrat" else "parboiled") +" parser.")
		        ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Showscala.
    _context.operatorLibrary.register("showscala",
        _data => _data.args match {
          case Args() =>
		        // Toggle showing the Scala term.
		        _showScala = !_showScala
		        emitln("Showing Scala is " + (if (_showScala) "ON." else "OFF."))
		        ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Showprior.
    _context.operatorLibrary.register("showprior",
        _data => _data.args match {
          case Args() =>
		        // Toggle showing the prior term.
		        _showPrior = !_showPrior
		        emitln("Showing prior term is " + (if (_showPrior) "ON." else "OFF."))
		        ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // History.
    _context.operatorLibrary.register("history",
        _data => _data.args match {
          case Args() =>
		        // Show the history.
            val it = _hist.entries
            while (it.hasNext) println(it.next)
		        println("Persistent history is found in: " + _filename)
		        ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Quiet.
    _context.operatorLibrary.register("quiet",
        _data => _data.args match {
          case Args() =>
            // Enable quiet.
            _quiet = !_quiet
            emitln("Quiet is " + (if (_quiet) "enabled." else "disabled."))
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Declare a ruleset.
    _context.operatorLibrary.register("declare",
        _data => {
        	_data.args foreach { _ match {
	          case sl:SymbolLiteral =>
	            // Declare the specified ruleset.
	            _context.ruleLibrary.declareRuleset(sl.value.name)
	            emitln("Declared ruleset " + sl.toParseString + ".")
	          case _ =>
        	}}
        	ApplyData._no_show
      	})
        
    // Enable a ruleset.
    _context.operatorLibrary.register("enable",
        _data => _data.args match {
          case Args(SymbolLiteral(_, sym)) =>
            // Enable the specified ruleset.
            _context.ruleLibrary.enableRuleset(sym.name)
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Disable a ruleset.
    _context.operatorLibrary.register("disable",
        _data => _data.args match {
          case Args(SymbolLiteral(_, sym)) =>
            // Disable the specified ruleset.
            _context.ruleLibrary.disableRuleset(sym.name)
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Set the limit on automatic rewrites.
    _context.operatorLibrary.register("setlimit",
        _data => _data.args match {
          case Args(IntegerLiteral(_, count)) =>
            // Enable the specified ruleset.
            _context.ruleLibrary.setLimit(count)
            emitln("Rewrite limit is now " + count + ".")
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Set whether to descend into children.
    _context.operatorLibrary.register("setdescend",
        _data => _data.args match {
          case Args(BooleanLiteral(_, flag)) =>
            // Set whether to descend.
            _context.ruleLibrary.setDescend(flag)
            emitln("Top-down rewriting is " + (if (flag) "ON." else "OFF."))
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Set whether to do round-trip parsing.
    _context.operatorLibrary.register("setroundtrip",
        _data => _data.args match {
          case Args(BooleanLiteral(_, flag)) =>
            // Set whether to descend.
            _doRoundTrip = flag
            emitln("Round-trip checking is " + (if (flag) "ON." else "OFF."))
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Set whether to automatically define operators.
    _context.operatorLibrary.register("setautodefine",
        _data => _data.args match {
          case Args(BooleanLiteral(_, flag)) =>
            // Set whether to automatically define operators.
            _autodefine = flag
            emitln("Automatic declaration of operators is " +
                (if (flag) "ON." else "OFF."))
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Enable or disable the rewriter.
    _context.operatorLibrary.register("rewrite",
        _data => _data.args match {
          case Args() =>
            // Toggle rewriting.
            _rewrite = !_rewrite
            emitln("Automatic rewriting is " +
                (if (_rewrite) "ON." else "OFF."))
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // See what rules are in scope.
    _context.operatorLibrary.register("showrules",
        _data => _data.args match {
          case Args(atom) =>
            // Get the rules, and print each one.
            for (rule <- _context.ruleLibrary.getRules(atom)) {
              println(rule.toParseString)
            }
            ApplyData._no_show
          case _ => ApplyData._no_show
        })
        
    // Define mod as a Symbolic operator.
    _context.operatorLibrary.register("mod",
        _data => _data.args match {
          case Args(IntegerLiteral(_, x), IntegerLiteral(_, y)) => x mod y
          case _ => _data.as_is
        })
        
    // Define neg as a Symbolic operator.
    _context.operatorLibrary.register("neg",
        _data => _data.args match {
          case Args(IntegerLiteral(_, x)) => -x
          case _ => _data.as_is
        })
        
    // Define add as a Symbolic operator.
		_context.operatorLibrary.register("add",
      _data => {
			  // Accumulate the integer literals found.
			  var lits:BigInt = 0
			  // Accumulate other atoms found.
			  var other = IndexedSeq[BasicAtom]()
			  // Traverse the list and divide the atoms.
			  _data.args.foreach {
			    x => x match {
			      case IntegerLiteral(_, value) => lits += value
			      case _ => other :+= x
			    }
			  }
			  // Now add the accumulated literals to the list.
			  other :+= Literal(INTEGER, lits)
			  // Construct and return a new operator application.
			  Apply(_data.op, AtomSeq(NoProps, other), true)
      })

    // Force a core dump.
    _context.operatorLibrary.register("fail",
        _data =>
          throw new VerifyError("no memory")
        )
        
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
    if (atom == ApplyData._no_show) return true
    
	//////////////////// GUI changes
	
	// obtain the parent tree node from the stack
	val rwNode = RWTree.current
	// add this atom as a child to the root node
	val atomNode = rwNode.addChild(atom)
	
	//////////////////// end GUI changes
	
    // Certain atoms require additional processing.
    atom match {
      case op: Operator if _autodefine =>
        _context.operatorLibrary.add(op)
        emitln("Defined operator " + toESymbol(op.name) + ".")
        return true

      case _ =>
        // Maybe show the atom before we rewrite.
        if (_showPrior) show(atom)
			
			//////////////////// GUI changes
			
			// Apply the global bindings to get the possibly-rewritten atom.
			
			RWTree.current = atomNode
		    var (newatom,_) = atom.rewrite(context.binds)

			// add newatom as a child node to atomNode.
			val rewriteNode = atomNode.addChild(newatom)
			RWTree.current = rewriteNode
			
			//////////////////// end GUI changes
			/*
		    // Apply the global bindings to get the possibly-rewritten atom.
		    var (newatom,_) = atom.rewrite(_binds)*/
			
		    // Rewrite it using the active rulesets of the context.
		    if (_rewrite) newatom = _context.ruleLibrary.rewrite(newatom)._1
		    if (_bindatoms) {
			    // Get the next bind variable name.
			    context.bind("_repl"+_bindNumber, newatom)
			    // Now write out the new atom.
			    show(newatom, Some("_repl" + _bindNumber))
			    _bindNumber += 1
		    } else {
		      show(newatom, None)
		    }
		    // Rules go in the rule library if they have declared rulesets.
		    newatom match {
		      case rule:RewriteRule =>
		      		// Rules go in the rule library.
		      		if (!rule.rulesets.isEmpty) {
		      			_context.ruleLibrary.add(rule)
		      			emitln("Rule stored in library.")
		      		}
		      case _ =>
        }
        return true
    }
  }
}
