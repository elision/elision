/* Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 */
package sjp.elision.repl

import sjp.elision.core._
import scala.collection.mutable.ListBuffer

/**
 * Provide a REPL to experiment with the new term rewriter.
 * 
 * The REPL can be started from the command line, or programmatically by
 * invoking the `run` method.  It prints a prompt, reads a line from the
 * standard input, and executes the line.
 * 
 * Other uses are possible.  To just execute a line, use the `execute` method.
 * This avoids reading from standard input, etc.  Note that `execute` maintains
 * the history, so history references are possible.
 */
object Repl {
  
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
    println("Elision v0.0 2012/02/03")
    run
    println("")
    println("Objects: ")
  }
  
  /**
   * Repeatedly print a prompt, obtain a line from the standard input,
   * and call [[sjp.elision.repl.Repl.execute]] to execute the line.
   */
  def run() {
    // Show the prompt and read a line.
    print("E> ")
    for(line <- io.Source.stdin.getLines) { execute(line) ; print("E> ") }
  }

  /**
   * Execute a line.  The line can be a command known to the interpreter, or
   * a history reference.  Exceptions are captured and displayed to the
   * standard output.
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
	    val result = _parser.parseAtom(lline)
	    result match {
	    	case _parser.Success(ast) => handle(ast.interpret)
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
  }
  
  /**
   * Decide what to do about an atom we just parsed.  This is where most of
   * the commands get processed, operators get added to the library, etc.
   * @param atom	An atom just parsed.
   */
  private def handle(atom: BasicAtom) = {
    // Certain atoms require additional processing.
    atom match {
      case od:OperatorDefinition =>
      	// Put operator definitions in the operator library.
        _library.add(od)
      case rule:RewriteRule =>
      	// Rules go in the rule library.
        println("WARNING: The rule library is not implemented yet.")
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
            |To quit type CTRL+D.
            |""".stripMargin)
      case Literal(_,SymVal('trace)) =>
        // Toggle tracing.
        _trace = !_trace
        _parser = new AtomParser(_context, _trace)
        println("Tracing is " + (if (_trace) "ON." else "OFF."))
      case Literal(_,SymVal('scala)) =>
        // Toggle showing the Scala term.
        _showScala = !_showScala
        println("Showing Scala is " + (if (_showScala) "ON." else "OFF."))
      case Literal(_,SymVal('prior)) =>
        // Toggle showing the prior term.
        _showPrior = !_showPrior
        println("Showing prior term is " + (if (_showPrior) "ON." else "OFF."))
      case Literal(_,SymVal('history)) =>
        // Show the history.
        var num = 1
        for (line <- _history) { println(" " + num + ": " + line) ; num += 1 }
      case Apply(Literal(_,SymVal('unbind)),AtomList(seq), _) =>
        // Try to unbind.
        seq match {
          case Seq(from:Variable) =>
            _binds -= from.name
            println("Unbound " + from.toParseString)
          case _ =>
            println("ERROR: Incorrect form for an unbind.  Need a single " +
            		"argument that must be a variable.")
        }
      case Apply(Literal(_,SymVal('bind)),AtomList(seq), _) =>
        // Try to bind.
        seq match {
          case Seq(from:Variable,to) =>
            // Bind the variable in this context.
            _binds += (from.name -> to)
            println("Bound " + from.toParseString)
          case _ =>
            // Incorrect form for a bind.
            println("ERROR: Incorrect form for a bind.  Need two arguments, " +
            		"the first of which must be a variable.")
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
    }
  }
}