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
package ornl.elision.parse

import ornl.elision.core.{BasicAtom, Context}
import ornl.elision.parse.AtomParser.{Presult, Failure, Success, AstNode}

/**
 * A processor is responsible for reading and handling atoms.
 * 
 * The processor instance maintains a [[ornl.elision.core.Context]] instance
 * that is available via the `context` field.  An existing context can be
 * provided when the processor is created.  Otherwise a new context is created
 * and used.
 * 
 * The processor instance also maintains its own [[ornl.elision.core.AtomParser]]
 * instance to parse atoms.  You can enable and disable tracing of the parser
 * via the `trace` field.
 * 
 * @param context		The context to use; if none is provided, use an empty one.
 */
class Processor(val console: Console, val context: Context = new Context) {
  /** Whether to trace the parser. */
  private var _trace = false

  /** The queue of handlers, in order. */
  private var _queue = List[Processor.Handler]()
  
  /** The parser to use. */
  private var _parser = new AtomParser(context, _trace)
  
  /**
   * Display the banner, version, and build information on the current
   * console using the `emitln` method.
   */
  private def banner() {
    import Version._
    val buf = new StringBuffer()
    console.emitln(
        """|      _ _     _
					 |  ___| (_)___(_) ___  _ __
					 | / _ \ | / __| |/ _ \| '_ \
					 ||  __/ | \__ \ | (_) | | | |
					 | \___|_|_|___/_|\___/|_| |_|
					 |
					 |Copyright (c) 2012 by UT-Battelle, LLC.
					 |All rights reserved.""".stripMargin)
    //_hist.add("// New Session: " + new java.util.Date)
    if (loaded) {
    	console.emitln("Version " + major + "." + minor + ", build " + build)
    	console.emitln("Web " + web)
    	//_hist.add("// Running: " + major + "." + minor +
    	//    ", build " + build)
    } else {
      console.emitln("Failed to load version information.")
    }
  }
  
  /**
   * Read the content of the provided file.
   * 
   * @param filename		The file to read.  It may be absolute, or it may be
   * 										relative to the current directory.
   * @throws	java.io.IOException
   * 					The file cannot be found or cannot be read.
   */
  def read(filename: String) {
    read(scala.io.Source.fromFile(filename))
  }
  
  /**
   * Read the content of the provided file.
   * 
   * @param file		The file to read.
   * @throws	java.io.IOException
   * 					The file cannot be found or cannot be read.
   */
  def read(file: java.io.File) {
    read(scala.io.Source.fromFile(file))
  }
  
  /**
   * Read the content of the provided reader.
   * 
   * @param reader		The reader providing input.
   * @throws	java.io.IOException
   * 					An error occurred trying to read.
   */
  def read(source: scala.io.Source) {
  	_execute(_parser.parseAtoms(source)) 
  }
  
  /**
   * Execute the provided text.  The text must form a complete sequence of
   * atoms.  It may contain zero or more atoms, but each atom must be
   * complete, or a parse error may result.
   * 
   * @param text		The text to parse.
   */
  def execute(text: String) {
    _execute(_parser.parseAtoms(text))
  }
  
  private def _execute(result: Presult) {
  	result match {
			case Failure(err) => console.error(err)
			case Success(nodes) =>
			  // We assume that there is at least one handler; otherwise not much
			  // will happen.  Process each node.
			  for (node <- nodes) {
			    _handleNode(node) match {
			      case None =>
			      case Some(newnode) =>
			        // Interpret the node.
			        val atom = newnode.interpret
			        _handleAtom(atom) match {
			          case None =>
			          case Some(newatom) =>
			            // Hand off the node.
			            _result(newatom)
			        }
			    }
			  } // Process all the nodes.
  	}
  }
  
  private def _handleNode(node: AstNode): Option[AstNode] = {
    // Pass the node to the handlers.  If any returns None, we are done.
    var theNode = node
    for (handler <- _queue) {
      handler.handleNode(theNode) match {
        case None => return None
        case Some(alt) => theNode = alt
      }
    } // Perform all handlers.
    return Some(theNode)
  }
  
  private def _handleAtom(atom: BasicAtom): Option[BasicAtom] = {
    // Pass the atom to the handlers.  If any returns None, we are done.
    var theAtom = atom
    for (handler <- _queue) {
      handler.handleAtom(theAtom) match {
        case None => return None
        case Some(alt) => theAtom = alt
      }
    } // Perform all handlers.
    return Some(theAtom)
  }
  
  private def _result(atom: BasicAtom) {
    for (handler <- _queue) {
      handler.result(atom)
    }
  }
  
  /**
   * Specify whether to trace the parser.
   * 
   * @param enable	If true, trace the parser.  If false, do not.
   */
  def trace_=(enable: Boolean) {
    // If the trace state has changed, re-create the parser.
    if (enable != _trace) {
      // The trace state has changed.  Re-create the parser.
      _trace = enable
      _parser = new AtomParser(context, _trace)
    }
  }
  
  /**
   * Register a handler.  This handler will be placed at the front of the
   * queue, so it will be processed before any other currently registered
   * handlers.
   * 
   * @param handler		The handler to add.
   */
	def registerAtFront(handler: Processor.Handler) {
  	_queue = handler +: _queue
  }
	
	/**
	 * Register a handler.  This handler is placed at the end of the queue, so
	 * it is invoked after any other currently registered handlers.
	 * 
	 * @param handler		The handler to add.
	 */
	def register(handler: Processor.Handler) {
	  _queue = _queue :+ handler
	}
}

/**
 * Supporting methods and classes for the `Processor` class.
 */
object Processor {
  /**
   * This is the trait for any class that wants to receive node and atom data
   * as it is obtained by a processor.
   * 
   * Override methods here to obtain the data you want and possibly perform
   * some processing on the node or atom. The methods also allow you to discard
   * the atom from further processing.
   * 
   * Handlers must be registered with the [[ornl.elision.parse.Processor]]
   * instance prior to reading any data.
   * 
   * Note also that if you want to handle errors, you need to override the
   * error handling methods.  These actually do something, so have a look!
   */
  trait Handler {
    /**
     * Handle a parsed abstract syntax tree node.  The default return value
     * for this method is `Some(node)`.  If you need to do processing of the
     * node, override this method.  Otherwise you can leave it as-is.
     * 
     * @param node		The node to process.
     * @return	An optional replacement node, or `None` if the node should be
     * 					*discarded*.
     */
    def handleNode(node: AtomParser.AstNode): Option[AstNode] = Some(node)
    
    /**
     * Handle an atom.  The default return value for this method is
     * `Some(atom)`.  If you need to do processing of the atom, override
     * this method.  Otherwise, leave it as-is.
     * 
     * @param atom		The atom to process.
     * @return	An optional replacement atom, or `None` if the atom should be
     * 					*discarded*.
     */
    def handleAtom(atom: BasicAtom): Option[BasicAtom] = Some(atom)
    
    /**
     * Once all handlers have been called, and if the atom remains, it is
     * passed to this method.  The default implementation does nothing.
     * Override this to do something with the final, fully-processed atom.
     * 
     * @param atom		The atom.
     */
    def result(atom: BasicAtom) {}
  }
}