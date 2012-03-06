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
  
  /** Figure out the location to store the history. */
  private val _filename = {
    val prop = new scala.sys.SystemProperties
    val home = prop("user.home")
    val fname = (if (prop("path.separator") == ":") ".elision"
      else "elision.ini")
    home + prop("file.separator") + fname
  }
  
  /** Get a history to use for the line editor. */
  val _hist = new History(new File(_filename))
  
  /** The context to use. */
  private val _context = new Context()
  
  /** The parser to use. */
  private var _parser = new AtomParser(_context, false)
  
  /** The current set of bindings. */
  private var _binds = new Bindings
  
  /** Direct access to the context's operator library. */
  private val _library = _context.operatorLibrary
  
  /** Whether to show atoms prior to rewriting with the current bindings. */
  private var _showPrior = false
  
  /** Whether to show the Scala. */
  private var _showScala = false
  
  /** The binding number. */
  private var _bindNumber = 0
  
  /** Whether or not to trace the parser. */
  private var _trace = false
  
  /** Have we defined the operators.  This is used by the `run` method. */
  private var _opsDefined = false
  
  /** Should stack traces be issued on exception. */
  private var _stacktrace = false
  
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
  
  /**
   * The entry point when started from the command line.  Print the current
   * version number, then invoke `run`.
   * @param args	The command line arguments.
   */
  def main(args: Array[String]) {
    run
    emitln("")
    emitln("Context: ")
    emitln(_context.toParseString)
  }
  
  /**
   * Display the banner.
   */
  private def banner() {
    emitln("""|      _ _     _
							 |  ___| (_)___(_) ___  _ __
							 | / _ \ | / __| |/ _ \| '_ \
							 ||  __/ | \__ \ | (_) | | | |
							 | \___|_|_|___/_|\___/|_| |_|
							 |
							 |Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
							 |All rights reserved.""".stripMargin)
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
    emitln("Bootstrapping Strategies...")
    _quiet = true
    execute(bootstrap.Strategies.defs)
    _quiet = qt
    emitln("Bootstrapping Scheme...")
    _quiet = true
    execute(bootstrap.Scheme.defs)
    _quiet = qt

    // Show the prompt and read a line.
    val cr = new jline.ConsoleReader
    cr.flushConsole()
    cr.setHistory(_hist)
    while(true) {
      val line = cr.readLine(if (_quiet) "q> " else "e> ")
      if (line == null || (line.trim.equalsIgnoreCase(":quit"))) return
      execute(line)
    }
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
    
    // See if this is a history reference and, if so, execute it.
    if (lline.startsWith("!")) {
      // This is a history reference.  The rest of the line is probably
      // a number, so try to parse it as such.
      val num = lline.substring(1).toInt
      try {
      	lline = _hist.getHistory(num)
      } catch {
        case ex:IndexOutOfBoundsException =>
          error("No such history item.")
          return
      }
    }
    
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
    // Bind.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("bind", ANYTYPE, 'a, 'v), Prop()))
    _context.operatorLibrary.register("bind",
        (_, list:AtomList, _) => list match {
          case Args(from:Variable, to:BasicAtom) =>
          	// Bind the variable in this context.
            _binds += (from.name -> to)
            emitln("Bound " + from.toParseString)
            to
          case _ => Literal.FALSE
        })

    // Bind.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("equal", ANYTYPE, 'x, 'y), Prop(Commutative())))
    _context.operatorLibrary.register("equal",
        (_, list:AtomList, _) => list match {
          case Args(x:BasicAtom, y:BasicAtom) =>
            // Check for equality.
            if (x == y) Literal.TRUE else Literal.FALSE
          case _ => Literal.FALSE
        })
        
    // Unbind.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("unbind", ANYTYPE, 'v), Prop()))
    _context.operatorLibrary.register("unbind",
        (_, list:AtomList, _) => list match {
          case Args(from:Variable) =>
          	// Unbind the variable in this context.
            _binds -= from.name
            emitln("Unbound " + from.toParseString)
            TypeUniverse
          case _ => Literal.FALSE
        })
        
    // Showbinds.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("showbinds", ANYTYPE), Prop()))
    _context.operatorLibrary.register("showbinds",
        (_, list:AtomList, _) => list match {
          case Args() => _binds
          case _ => Literal.FALSE
        })
        
    // Context.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("context", ANYTYPE), Prop()))
    _context.operatorLibrary.register("context",
        (_, list:AtomList, _) => list match {
          case Args() =>
            println(_context.toParseString)
            Literal.NOTHING
          case _ => Literal.FALSE
        })
        
    // Stacktrace.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("stacktrace", ANYTYPE), Prop()))
    _context.operatorLibrary.register("stacktrace",
        (_, list:AtomList, _) => list match {
          case Args() =>
            _stacktrace = !_stacktrace
            emitln("Printing stack traces is " +
                (if (_stacktrace) "ON" else "OFF") + ".") 
            Literal.NOTHING
          case _ => Literal.FALSE
        })
        
    // Read.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("read", ANYTYPE, 'filename), Prop()))
    _context.operatorLibrary.register("read",
        (_, list:AtomList, _) => list match {
          case Args(filename:Literal) =>
            // TODO Read the content of the file.
            emitln("Not implemented.")
            Literal.FALSE
          case _ => Literal.FALSE
        })
        
    // Write.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("write", ANYTYPE, 'filename), Prop()))
    _context.operatorLibrary.register("write",
        (_, list:AtomList, _) => list match {
          case Args(filename:Literal) =>
            // TODO Write the content to the file.
            emitln("Not implemented.")
            Literal.FALSE
          case _ => Literal.FALSE
        })
        
    // Help.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("help", ANYTYPE), Prop()))
    _context.operatorLibrary.register("help",
        (_, list:AtomList, _) => list match {
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
		        Literal.NOTHING
          case _ => Literal.FALSE
        })
        
    // Traceparse.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("traceparse", ANYTYPE), Prop()))
    _context.operatorLibrary.register("traceparse",
        (_, list:AtomList, _) => list match {
          case Args() =>
		        // Toggle tracing.
		        _trace = !_trace
		        _parser = new AtomParser(_context, _trace)
		        emitln("Tracing is " + (if (_trace) "ON." else "OFF."))
		        Literal.NOTHING
          case _ => Literal.FALSE
        })
        
    // Tracematch.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("tracematch", ANYTYPE), Prop()))
    _context.operatorLibrary.register("tracematch",
        (_, list:AtomList, _) => list match {
          case Args() =>
		        // Toggle tracing.
		        BasicAtom.traceMatching = !BasicAtom.traceMatching
		        emitln("Match tracing is " +
		            (if (BasicAtom.traceMatching) "ON." else "OFF."))
		        Literal.NOTHING
          case _ => Literal.FALSE
        })
        
    // Showscala.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("showscala", ANYTYPE), Prop()))
    _context.operatorLibrary.register("showscala",
        (_, list:AtomList, _) => list match {
          case Args() =>
		        // Toggle showing the Scala term.
		        _showScala = !_showScala
		        emitln("Showing Scala is " + (if (_showScala) "ON." else "OFF."))
		        Literal.NOTHING
          case _ => Literal.FALSE
        })
        
    // Showprior.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("showprior", ANYTYPE), Prop()))
    _context.operatorLibrary.register("showprior",
        (_, list:AtomList, _) => list match {
          case Args() =>
		        // Toggle showing the prior term.
		        _showPrior = !_showPrior
		        emitln("Showing prior term is " + (if (_showPrior) "ON." else "OFF."))
		        Literal.NOTHING
          case _ => Literal.FALSE
        })
        
    // History.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("history", ANYTYPE), Prop()))
    _context.operatorLibrary.register("history",
        (_, list:AtomList, _) => list match {
          case Args() =>
		        // Show the history.
		        for (index <- 1 until _hist.getCurrentIndex()) {
		          println(" " + index + ": " + _hist.getHistory(index))
		        }
		        println("Persistent history is found in: " + _filename)
		        Literal.NOTHING
          case _ => Literal.FALSE
        })
        
    // Quiet.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("quiet", ANYTYPE), Prop()))
    _context.operatorLibrary.register("quiet",
        (_, list:AtomList, _) => list match {
          case Args() =>
            // Enable quiet.
            _quiet = !_quiet
            emitln("Quiet is " + (if (_quiet) "enabled." else "disabled."))
            Literal.NOTHING
          case _ => Literal.FALSE
        })
        
    // Enable a ruleset.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("enable", ANYTYPE, 'x), Prop()))
    _context.operatorLibrary.register("enable",
        (_, list:AtomList, _) => list match {
          case Args(Literal(_,SymVal(sym))) =>
            // Enable the specified ruleset.
            _context.enableRuleset(sym.name)
            Literal.TRUE
          case _ => Literal.FALSE
        })
        
    // Disable a ruleset.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("disable", ANYTYPE, 'x), Prop()))
    _context.operatorLibrary.register("disable",
        (_, list:AtomList, _) => list match {
          case Args(Literal(_,SymVal(sym))) =>
            // Disable the specified ruleset.
            _context.disableRuleset(sym.name)
            Literal.TRUE
          case _ => Literal.FALSE
        })
        
    // Set the limit on automatic rewrites.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("setlimit", ANYTYPE, 'x), Prop()))
    _context.operatorLibrary.register("setlimit",
        (_, list:AtomList, _) => list match {
          case Args(Literal(_,IntVal(count))) =>
            // Enable the specified ruleset.
            _context.setLimit(count)
            Literal.TRUE
          case _ => Literal.FALSE
        })
        
    // Enable or disable the rewriter.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("rewrite", ANYTYPE), Prop()))
    _context.operatorLibrary.register("rewrite",
        (_, list:AtomList, _) => list match {
          case Args() =>
            // Toggle rewriting.
            _rewrite = !_rewrite
            emitln("Automatic rewriting is " +
                (if (_rewrite) "ON." else "OFF."))
            Literal.TRUE
          case _ => Literal.FALSE
        })
        
    // See what rules are in scope.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("showrules", ANYTYPE, 'x), Prop()))
    _context.operatorLibrary.register("showrules",
        (_, list:AtomList, _) => list match {
          case Args(atom) =>
            // Get the rules, and print each one.
            for (rule <- _context.getRules(atom)) {
              println(rule.toParseString)
            }
            Literal.TRUE
          case _ => Literal.FALSE
        })
    
    // Define add as a native operator.
		val opdef = NativeOperatorDefinition(
		  OperatorPrototype("add",
		      List(Variable(INTEGER, "x"), Variable(INTEGER, "y")), INTEGER),
		  OperatorProperties(associative=true, commutative=true,
		    identity=Some(Literal(INTEGER, 0))))
		// Create a closure to perform the addition.
		def doadd(op: Operator, args: AtomList, binds: Option[Bindings]) = {
		  // Accumulate the integer literals found.
		  var lits:BigInt = 0
		  // Accumulate other atoms found.
		  var other = ListBuffer[BasicAtom]()
		  // Traverse the list and divide the atoms.
		  args.atoms.foreach {
		    x => x match {
		      case Literal(_, IntVal(value)) => lits += value
		      case _ => other += x
		    }
		  }
		  // Now add the accumulated literals to the list.
		  other += Literal(INTEGER, lits)
		  // Construct and return a new operator application.
		  Apply(op, AtomList(other), true)
		}
		// Register the operator and its handler.
		_context.operatorLibrary.add(opdef)
		_context.operatorLibrary.register("add", doadd)
        
    // The operator are defined.
    _opsDefined = true
  }
  
  /**
   * Decide what to do about an atom we just parsed.  This is where most of
   * the commands get processed, operators get added to the library, etc.
   * @param atom	An atom just parsed.
   * @return	True on success, false on failure.
   */
  private def handle(atom: BasicAtom) = {
    // Certain atoms require additional processing.
    atom match {
      case od:OperatorDefinition =>
      	// Put operator definitions in the operator library.
        _library.add(od)
        true
      case _ =>
        // Maybe show the atom before we rewrite.
        if (_showPrior) show(atom)
		    // Apply the global bindings to get the possibly-rewritten atom.
		    var (newatom,_) = atom.rewrite(_binds)
		    // Rewrite it using the active rulesets of the context.
		    if (_rewrite) newatom = _context.rewrite(newatom)._1
		    // Get the next bind variable name.
		    _binds += ("repl"+_bindNumber -> newatom)
		    // Now write out the new atom.
		    show(newatom, Some("repl" + _bindNumber))
		    _bindNumber += 1
		    // Rules go in the rule library.
		    newatom match {
		      case rule:RewriteRule =>
		      	// Rules go in the rule library.
		        _context.add(rule)
		      case _ =>
        }
        true
    }
  }
}
