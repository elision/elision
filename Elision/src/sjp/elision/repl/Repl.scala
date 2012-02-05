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
 */
object Repl {
  
  val context = new Context()
  var parser = new AtomParser(context, false)
  var binds = new Bindings
  val library = context.operatorLibrary
  var showPrior = false
  var showScala = false
  var bindNumber = 0
  var trace = false
  var history = ListBuffer[String]()

  def main(args: Array[String]) {
    println("Elision v0.0 2012/02/03")
    run
    println("")
    println("Objects: ")
  }
  
  def run() {
    // Show the prompt and read a line.
    print("E> ")
    for(line <- io.Source.stdin.getLines) { execute(line) ; print("E> ") }
  }
  
  def show(atom: BasicAtom, what:Option[String] = None) {
    val pre = what match {
      case None => ""
      case Some(name) => "$" + name + " = "
    }
    if (showScala) println("Scala: " + pre + atom)
    println(pre + atom.toParseString)
  }

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
      	lline = history(num)
      } catch {
        case ex:ArrayIndexOutOfBoundsException =>
          println("ERROR: No such history item.")
          return
      }
    }
    // Save this in the history.
    history += line
    // First parse the line and see what we get.
    try {
	    val result = parser.parseAtom(lline)
	    result match {
	    	case parser.Success(ast) => handle(ast.interpret)
	      case parser.Failure(msg) => println(msg)
	    }
    } catch {
      case ex:Exception =>
        println("ERROR (" + ex.getClass + "): " + ex.getMessage())
    }
  }
  
  def handle(atom: BasicAtom) = {
    // Certain atoms require additional processing.
    atom match {
      case od:OperatorDefinition =>
      	// Put operator definitions in the operator library.
        library.add(od)
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
        trace = !trace
        parser = new AtomParser(context, trace)
        println("Tracing is " + (if (trace) "ON." else "OFF."))
      case Literal(_,SymVal('scala)) =>
        // Toggle showing the Scala term.
        showScala = !showScala
        println("Showing Scala is " + (if (showScala) "ON." else "OFF."))
      case Literal(_,SymVal('prior)) =>
        // Toggle showing the prior term.
        showPrior = !showPrior
        println("Showing prior term is " + (if (showPrior) "ON." else "OFF."))
      case Literal(_,SymVal('history)) =>
        // Show the history.
        var num = 1
        for (line <- history) { println(" " + num + ": " + line) ; num += 1 }
      case Apply(Literal(_,SymVal('unbind)),AtomList(seq), _) =>
        // Try to unbind.
        seq match {
          case Seq(from:Variable) =>
            binds -= from.name
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
            binds += (from.name -> to)
            println("Bound " + from.toParseString)
          case _ =>
            // Incorrect form for a bind.
            println("ERROR: Incorrect form for a bind.  Need two arguments, " +
            		"the first of which must be a variable.")
        }
      case _ =>
        // Maybe show the atom before we rewrite.
        if (showPrior) show(atom)
		    // Apply the global bindings to get the possibly-rewritten atom.
		    val (newatom,_) = atom.rewrite(binds)
		    // Get the next bind variable name.
		    binds += ("repl"+bindNumber -> newatom)
		    // Now write out the new atom.
		    show(newatom, Some("repl" + bindNumber))
		    bindNumber += 1
    }
  }
}