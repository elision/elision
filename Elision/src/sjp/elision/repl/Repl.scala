/*======================================================================
 *       _ _     _
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
  
  /** The history.  This is set in `execute`. */
  private var _history = ListBuffer[String]()

  /**
   * The entry point when started from the command line.  Print the current
   * version number, then invoke `run`.
   * @param args	The command line arguments.
   */
  def main(args: Array[String]) {
    println("""|      _ _     _
							 |  ___| (_)___(_) ___  _ __
							 | / _ \ | / __| |/ _ \| '_ \
							 ||  __/ | \__ \ | (_) | | | |
							 | \___|_|_|___/_|\___/|_| |_|
							 |
							 |Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
							 |All rights reserved.""".stripMargin)
    run
    println("")
    println("Objects: ")
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
      val num = lline.substring(1).toInt - 1
      try {
      	lline = _history(num)
      } catch {
        case ex:ArrayIndexOutOfBoundsException =>
          println("ERROR: No such history item.")
          return
      }
    }
    // Save this in the history.
    _history += line
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
      case Literal(_,SymVal('help)) =>
        // Give some help.
        println("""
        		|Elision Help
        		|
            | bind(v,a) .................. Bind variable v to atom a.
        		| help ....................... Show this help text.
            | history .................... Show the history so far.
            | prior ...................... Toggle showing the unrewritten term.
            | scala ...................... Toggle showing the Scala term.
            | trace ...................... Toggle parser tracing.
            | unbind(v) .................. Unbind variable v.
            |
            |Use ! followed by a number to re-execute a line from the history.
            |
            |To quit type :quit.
            |""".stripMargin)
        true
      case Literal(_,SymVal('trace)) =>
        // Toggle tracing.
        _trace = !_trace
        _parser = new AtomParser(_context, _trace)
        println("Tracing is " + (if (_trace) "ON." else "OFF."))
        true
      case Literal(_,SymVal('scala)) =>
        // Toggle showing the Scala term.
        _showScala = !_showScala
        println("Showing Scala is " + (if (_showScala) "ON." else "OFF."))
        true
      case Literal(_,SymVal('prior)) =>
        // Toggle showing the prior term.
        _showPrior = !_showPrior
        println("Showing prior term is " + (if (_showPrior) "ON." else "OFF."))
        true
      case Literal(_,SymVal('history)) =>
        // Show the history.
        var num = 1
        for (line <- _history) { println(" " + num + ": " + line) ; num += 1 }
        true
      case Apply(Literal(_,SymVal('unbind)),AtomList(seq,_)) =>
        // Try to unbind.
        seq match {
          case Seq(from:Variable) =>
            _binds -= from.name
            println("Unbound " + from.toParseString)
            true
          case _ =>
            println("ERROR: Incorrect form for an unbind.  Need a single " +
            		"argument that must be a variable.")
            false
        }
      case Apply(Literal(_,SymVal('bind)),AtomList(seq,_)) =>
        // Try to bind.
        seq match {
          case Seq(from:Variable,to) =>
            // Bind the variable in this context.
            _binds += (from.name -> to)
            println("Bound " + from.toParseString)
            true
          case _ =>
            // Incorrect form for a bind.
            println("ERROR: Incorrect form for a bind.  Need two arguments, " +
            		"the first of which must be a variable.")
            false
        }
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
