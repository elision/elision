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

import ornl.elision.core._
import ornl.elision.repl.ReplActor
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
class Processor(val context: Context = new Context)
extends Executor
with TraceableParse
with Timeable
with HasHistory {
  // We are the implicit executor.
  ornl.elision.core.knownExecutor = this
  
  // Set up the stacktrace property.
  declareProperty("stacktrace",
      "Print a stack trace on all (non-Elision) exceptions.", false)
  
  // Configure file search properties.
  declareProperty("usepath",
      "Whether to use the search path to locate files.", true)
  declareProperty("useclasspath",
      "Whether to use the class path to locate files.", true)
  declareProperty("path",
      "The search path to use to locate files.", FileResolver.defaultPath)
  
  /** Whether to trace the parser. */
  private var _trace = false

  /** Select the parser to use */
  private var _toggle = false
  
  /** The queue of handlers, in order. */
  private var _queue = List[Processor.Handler]()
  
  /** The parser to use. */
  private var _parser = new AtomParser(context, _toggle, _trace)
  
  val console = PrintConsole
  
  /**
   * Display the banner, version, and build information on the current
   * console using the `emitln` method of the console.
   */
  protected def banner() {
    import ornl.elision.Version._
    console.emitln(
        """|      _ _     _
					 |  ___| (_)___(_) ___  _ __
					 | / _ \ | / __| |/ _ \| '_ \
					 ||  __/ | \__ \ | (_) | | | |
					 | \___|_|_|___/_|\___/|_| |_|
					 |
					 |Copyright (c) 2012 by UT-Battelle, LLC.
					 |All rights reserved.""".stripMargin)
    addHistoryLine("// New Session: " + new java.util.Date)
    if (loaded) {
    	console.emitln("Version " + major + "." + minor + ", build " + build)
    	console.emitln("Web " + web)
    	addHistoryLine("// Running: " + major + "." + minor +
    	    ", build " + build)
    } else {
      console.emitln("Failed to load version information.")
    }
  }
  
  def reportElapsed {
    console.sendln("elapsed: " + getLastTimeString + "\n")
  }
  
  /**
   * Read the content of the provided file.  This method uses a
   * [[ornl.elision.parse.FileResolver]] instance to find the requested
   * file.
   * 
   * @param filename		The file to read.  It may be absolute, or it may be
   * 										relative to the current directory.
   * @param quiet       If true, do not emit any error messages.
   * @throws	java.io.IOException
   * 					The file cannot be found or cannot be read.
   */
  def read(filename: String, quiet: Boolean) {
    // Make a resolver from the properties.  Is this costly to do every time
    // we want to read a file?  Probably not.
    val usePath = getProperty[Boolean]("usepath")
    val useClassPath = getProperty[Boolean]("useclasspath")
    val path = getProperty[String]("path")
    val resolver = FileResolver(usePath, useClassPath, Some(path))
    resolver.find(filename) match {
      case None =>
        if (!quiet) console.error("File not found: " + filename)
      case Some(reader) =>
        read(scala.io.Source.fromInputStream(reader))
    }
  }
  
  /**
   * Read the content of the provided file.
   * 
   * @param file		The file to read.
   * @throws	java.io.IOException
   * 					The file cannot be found or cannot be read.
   */
  def read(file: java.io.File) {
    read(scala.io.Source.fromFile(file), file.getAbsolutePath)
  }
  
  /**
   * Read the content of the provided reader.
   * 
   * @param source		The reader providing input.
   * @param filename  The file name, if relevant.
   * @throws	java.io.IOException
   * 					An error occurred trying to read.
   */
  def read(source: scala.io.Source, filename: String = "(console)") {
  	_execute(_parser.parseAtoms(source)) 
  }
  
  /**
   * Execute the provided text.  The text must form a complete sequence of
   * atoms.  It may contain zero or more atoms, but each atom must be
   * complete, or a parse error may result.
   * 
   * This method tries to trap exceptions and handle them intelligently.
   * Elision exceptions are printed as error messages.  General exceptions
   * are printed along with their class information and message, and a
   * stack trace if that is enabled.  Out of memory is handled by
   * invoking the garbage collector to try to recover.  Finally, a runtime
   * exception (a `Throwable`) causes a core dump and is reported as an
   * internal error.
   * 
   * In all cases the system attempts to continue, unless a second exception
   * occurs while handling the first.
   * 
   * @param text		The text to parse.
   */
  def execute(text: String) {
    // If the line is a history reference, go and look it up now.
    var lline = text.trim
    if (lline.startsWith("!")) {
      // This is a history reference, so go and get it.
      val num = lline.substring(1).trim.toInt
      val prior = getHistoryEntry(num)
      if (prior == None) {
        console.error("No such history entry: " + lline)
        return
      }
      lline = prior.get
      console.emitln(lline)
    }
	
	//////////////////// GUI changes
	
	// Create the root of our rewrite tree it contains a String of the REPL input.
	val treeRoot = RWTree.createNewRoot(lline) //new RWTreeNode(lline)
	
	//////////////////// end GUI changes
	
    _execute(_parser.parseAtoms(lline))
	
	//////////////////// GUI changes
	
	// send the completed rewrite tree to the GUI's actor
	if(ReplActor.guiActor != null && !ReplActor.disableGUIComs && lline != "")
		ReplActor.guiActor ! treeRoot
	
	//////////////////// end GUI changes
  }
  
  def parse(text: String) = {
    _parser.parseAtoms(text) match {
      case Failure(err) => ParseFailure(err)
      case Success(nodes) => ParseSuccess(nodes map (_.interpret))
    }
  }
  
  private def _execute(result: Presult) {
    import ornl.elision.ElisionException
    startTimer
	
	//////////////////// GUI changes
	val rwNode = RWTree.current
	//////////////////// end GUI changes
	
    try {
    	result match {
  			case Failure(err) => console.error(err)
  			case Success(nodes) =>
  			  // We assume that there is at least one handler; otherwise not much
  			  // will happen.  Process each node.
  			  for (node <- nodes) {
				//////////////////// GUI changes
                var nodeLabel : String = "line node: " // TODO: This should be the parse string of the original term represented by this node.
                val lineNode = RWTree.addTo(rwNode, nodeLabel) //rwNode.addChild("line node")
				RWTree.current = lineNode
				//////////////////// end GUI changes
				
  			    _handleNode(node) match {
  			      case None =>
  			      case Some(newnode) =>
  			        // Interpret the node.
                    RWTree.current = lineNode
  			        val atom = newnode.interpret
                    RWTree.current = lineNode
  			        _handleAtom(atom) match {
  			          case None =>
  			          case Some(newatom) =>
  			            // Hand off the node.
  			            _result(newatom)
  			        }
  			    }
  			  } // Process all the nodes.
    	}
    } catch {
      case ElisionException(msg) =>
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
    stopTimer
    showElapsed
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
	
	//////////////////// GUI changes
	
	// obtain the parent tree node from the stack
	val rwNode = RWTree.current
	// add this atom as a child to the root node
	val atomNode = RWTree.addTo(rwNode, atom) //rwNode.addChild(atom)
	
	//////////////////// end GUI changes
	
    for (handler <- _queue) {
      RWTree.current = atomNode // GUI change
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
   * Determine whether tracing is enabled.
   */
  def trace = _trace
  
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
	 * @param handlers		The handlers to add, in order.
	 */
	def register(handlers: Processor.Handler*) {
	  for (handler <- handlers) handler.init(this)
	  _queue = _queue ++ handlers
	}
	  
  /**
   * Save the current context in response to an exception or other
   * unrecoverable error.
   * 
   * @param msg		A human-readable message.
   * @param th		An optional throwable.
   */
  protected def coredump(msg: String, th: Option[Throwable] = None) {
    val cfile = new java.io.FileWriter("elision.core")
    if (cfile != null) {
      val binds = <binds>{context.binds.toParseString}</binds>
      val ops = <operator-library>{context.operatorLibrary.toParseString}</operator-library>
      val rules = <rule-library>{context.ruleLibrary.toParseString}</rule-library>
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
        val it = getHistoryIterator
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
      console.emitln("Wrote core dump to elision.core.")
    } else {
      console.warn("Unable to save core dump.")
    }
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
     * This method is invoked when the handler is registered.  The primary
     * use is to set up the values of properties that are important to the
     * handler.
     * 
     * The executor is passed along, so properties can be set.  The return
     * value is used to determine if setup was successful or not.
     * 
     * @param exec    The executor.
     * @return  True if success, and false if failure.  When false is
     *          returned, the handler is discarded and registration does
     *          not continue.
     */
    def init(exec: Executor): Boolean = true
    
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