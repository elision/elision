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
   * The entry point when started from the command line.  Print the current
   * version number, then invoke `run`.
   * @param args	The command line arguments.
   */
  def main(args: Array[String]) {
    run
    println("")
    println("Context: ")
    println(_context.toParseString)
  }
  
  /**
   * Display the banner.
   */
  private def banner() {
    println("""|      _ _     _
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
    
    // Define the operators.
    if (!_opsDefined) defineOps

    // Show the prompt and read a line.
    val cr = new jline.ConsoleReader
    cr.setHistory(_hist)
    while(true) {
      val line = cr.readLine("e> ")
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
   */
  def execute(line: String) {
    var lline = line.trim
    // Skip blank lines.
    if (lline == "") return
    // See if this is a history reference.
    if (lline.startsWith("!")) {
      // This is a history reference.  The rest of the line is probably
      // a number, so try to parse it as such.
      val num = lline.substring(1).toInt
      try {
      	lline = _hist.getHistory(num)
      } catch {
        case ex:IndexOutOfBoundsException =>
          println("ERROR: No such history item.")
          return
      }
    }
    // First parse the line and see what we get.
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
        println("ERROR (" + ex.getClass + "): " + ex.getMessage())
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
    println(pre + atom.toParseString)
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
          println("ERROR: Round-trip parse failure for atom " +
              atom.toString + ".  No atoms returned on parse.")
        if (list.length > 1) {
          println("ERROR: Round-trip parse failure for atom " +
              atom.toString + ".  Too many atoms returned on parse.")
          for (node <- list) println(" -> " + node.interpret.toParseString)
        }
        // The result must be equal to the original atom.
        val newatom = list(0).interpret
        if (atom != newatom) {
          println("ERROR: Round-trip parse failure for atom " +
              atom.toString + ".  Atom unequal to original.")
          println("  original: " + atom.toParseString)
          println("  result:   " + newatom.toParseString)
        }
      case _parser.Failure(msg) =>
        println("ERROR: Round-trip parse failure for atom " + atom.toString +
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
        (_, list:AtomList) => list match {
          case Args(from:Variable, to:BasicAtom) =>
          	// Bind the variable in this context.
            _binds += (from.name -> to)
            println("Bound " + from.toParseString)
            to
          case _ => Literal.FALSE
        })

    // Bind.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("equal", ANYTYPE, 'x, 'y), Prop(Commutative())))
    _context.operatorLibrary.register("equal",
        (_, list:AtomList) => list match {
          case Args(x:Variable, y:BasicAtom) =>
            // Check for equality.
            if (x == y) Literal.TRUE else Literal.FALSE
          case _ => Literal.FALSE
        })
        
    // Unbind.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("unbind", ANYTYPE, 'v), Prop()))
    _context.operatorLibrary.register("unbind",
        (_, list:AtomList) => list match {
          case Args(from:Variable) =>
          	// Unbind the variable in this context.
            _binds -= from.name
            println("Unbound " + from.toParseString)
            TypeUniverse
          case _ => Literal.FALSE
        })
        
    // Showbinds.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("showbinds", ANYTYPE), Prop()))
    _context.operatorLibrary.register("showbinds",
        (_, list:AtomList) => list match {
          case Args() => _binds
          case _ => Literal.FALSE
        })
        
    // Context.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("context", ANYTYPE), Prop()))
    _context.operatorLibrary.register("context",
        (_, list:AtomList) => list match {
          case Args() =>
            println(_context.toParseString)
            Literal.NOTHING
          case _ => Literal.FALSE
        })
        
    // Stacktrace.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("stacktrace", ANYTYPE), Prop()))
    _context.operatorLibrary.register("stacktrace",
        (_, list:AtomList) => list match {
          case Args() =>
            _stacktrace = !_stacktrace
            println("Printing stack traces is " +
                (if (_stacktrace) "ON" else "OFF") + ".") 
            Literal.NOTHING
          case _ => Literal.FALSE
        })
        
    // Read.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("read", ANYTYPE, 'filename), Prop()))
    _context.operatorLibrary.register("read",
        (_, list:AtomList) => list match {
          case Args(filename:Literal) =>
            // TODO Read the content of the file.
            println("Not implemented.")
            Literal.FALSE
          case _ => Literal.FALSE
        })
        
    // Write.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("write", ANYTYPE, 'filename), Prop()))
    _context.operatorLibrary.register("write",
        (_, list:AtomList) => list match {
          case Args(filename:Literal) =>
            // TODO Write the content to the file.
            println("Not implemented.")
            Literal.FALSE
          case _ => Literal.FALSE
        })
        
    // Help.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("help", ANYTYPE), Prop()))
    _context.operatorLibrary.register("help",
        (_, list:AtomList) => list match {
          case Args() =>
          	// Give some help.
		        println("""
		        		|Elision Help
		        		|
		            | bind(v,a) .................. Bind variable v to atom a.
		            | context() .................. Display contents of the current context.
		        		| help() ..................... Show this help text.
		            | history() .................. Show the history so far.
		            | showbinds() ................ Display the current set of bindings.
		            | showprior() ................ Toggle showing the unrewritten term.
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
        (_, list:AtomList) => list match {
          case Args() =>
		        // Toggle tracing.
		        _trace = !_trace
		        _parser = new AtomParser(_context, _trace)
		        println("Tracing is " + (if (_trace) "ON." else "OFF."))
		        Literal.NOTHING
          case _ => Literal.FALSE
        })
        
    // Tracematch.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("tracematch", ANYTYPE), Prop()))
    _context.operatorLibrary.register("tracematch",
        (_, list:AtomList) => list match {
          case Args() =>
		        // Toggle tracing.
		        BasicAtom.traceMatching = !BasicAtom.traceMatching
		        println("Match tracing is " +
		            (if (BasicAtom.traceMatching) "ON." else "OFF."))
		        Literal.NOTHING
          case _ => Literal.FALSE
        })
        
    // Showscala.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("showscala", ANYTYPE), Prop()))
    _context.operatorLibrary.register("showscala",
        (_, list:AtomList) => list match {
          case Args() =>
		        // Toggle showing the Scala term.
		        _showScala = !_showScala
		        println("Showing Scala is " + (if (_showScala) "ON." else "OFF."))
		        Literal.NOTHING
          case _ => Literal.FALSE
        })
        
    // Showprior.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("showprior", ANYTYPE), Prop()))
    _context.operatorLibrary.register("showprior",
        (_, list:AtomList) => list match {
          case Args() =>
		        // Toggle showing the prior term.
		        _showPrior = !_showPrior
		        println("Showing prior term is " + (if (_showPrior) "ON." else "OFF."))
		        Literal.NOTHING
          case _ => Literal.FALSE
        })
        
    // History.
    _context.operatorLibrary.add(NativeOperatorDefinition(
        Proto("history", ANYTYPE), Prop()))
    _context.operatorLibrary.register("history",
        (_, list:AtomList) => list match {
          case Args() =>
		        // Show the history.
		        for (index <- 1 until _hist.getCurrentIndex()) {
		          println(" " + index + ": " + _hist.getHistory(index))
		        }
		        println("Persistent history is found in: " + _filename)
		        Literal.NOTHING
          case _ => Literal.FALSE
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
  private def handle(atom: BasicAtom) = {
    // Certain atoms require additional processing.
    atom match {
      case od:OperatorDefinition =>
      	// Put operator definitions in the operator library.
        _library.add(od)
        true
      case sd:StrategyDefinition =>
        // Put strategy definitions in the context.
        _context.add(sd)
        true
      case _ =>
        // Maybe show the atom before we rewrite.
        if (_showPrior) show(atom)
		    // Apply the global bindings to get the possibly-rewritten atom.
		    val (newatom,_) = atom.rewrite(_binds)
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
