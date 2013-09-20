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

import ornl.elision.actors.ReplActor
import ornl.elision.context.Context
import ornl.elision.context.Executor
import ornl.elision.core.BasicAtom
import ornl.elision.util.Console
import ornl.elision.util.PrintConsole
import ornl.elision.util.FileResolver
import ornl.elision.util.Timeable
import ornl.elision.util.PropertyManager
import ornl.elision.util.HasHistory
import ornl.elision.util.ElisionException
import ornl.elision.util.Version
import ornl.elision.util.Loc
import ornl.elision.core.Dialect

/**
 * Manage the default parser kind to use.
 */
object ProcessorControl {
  /** The default parser to use. */
  var parserKind = 'new
  /** Whether to bootstrap. */
  var bootstrap = true
}

/**
 * Indicate that it is possible to enable and disable tracing of parsing at
 * runtime.  An executor may (or may not) implement this.
 * 
 * Typically enabling and disabling tracing require rebuilding the parser, so
 * this trait is abstract.
 */
trait TraceableParse {
  /**
   * Specify whether to trace the parser.
   * 
   * @param enable  If true, trace the parser.  If false, do not.
   */
  def trace_=(enable: Boolean): Unit
  
  /**
   * Determine whether tracing is enabled.
   */
  def trace: Boolean
}

/**
 * A processor is responsible for reading and handling atoms.
 * 
 * The processor instance maintains a [[ornl.elision.context.Context]] instance
 * that is available via the `context` field.  An existing context can be
 * provided when the processor is created.  Otherwise a new context is created
 * and used.
 * 
 * The processor instance also maintains its own [[ornl.elision.parse.ElisionParser]]
 * instance to parse atoms.  You can enable and disable tracing of the parser
 * via the `trace` field.
 * 
 * @param settings  The settings from the command line parser.
 * @param context		The context to use; if none is provided, use an empty one.
 */
class Processor(val settings: Map[String, String],
    var context: Context = new Context)
extends Executor
with TraceableParse
with Timeable
with HasHistory {
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

  // Atom rewrite timeout property used in BasicAtom. We need to make sure
  // that all executors know about this property, which is why we are declaring
  // the property here.
  declareProperty("rewrite_timeout",
      "The maximum time to try rewriting an atom. In seconds.",
      BigInt(0))

  // This property is used in ACMatcher.scala to decide whether to quickly (and 
  // sometimes erroneously) terminate matching.
  declareProperty("rewrite_aggressive_fail",
      "Whether to aggresively fail fast while rewriting. " +
      "If true, some rewrites may not be applied",
      false)
      
  /** Whether to trace the parser. */
  private var _trace = false

  /** The queue of handlers, in order. */
  private var _queue = List[Processor.Handler]()

  /** Specify the console.  We don't know the number of lines. */
  var console : Console = PrintConsole
  
  /** The list of context checkpoints */
  val checkpoints = new collection.mutable.ArrayBuffer[(java.util.Date, Context)]
  
  /**
   * Display the banner, version, and build information on the current
   * console using the `emitln` method of the console.  This will also
   * record session information in the history file, if enabled.
   * 
   * @param history If true (default), log the start of the session.
   */
  def banner(history: Boolean = true) {
    import ornl.elision.util.Version._
    console.emitln(
        """|      _ _     _
					 |  ___| (_)___(_) ___  _ __
					 | / _ \ | / __| |/ _ \| '_ \
					 ||  __/ | \__ \ | (_) | | | |
					 | \___|_|_|___/_|\___/|_| |_|
					 |
					 |Copyright (c) 2012 by UT-Battelle, LLC.
					 |All rights reserved.""".stripMargin)
    if (loaded) {
      if (history)
        addHistoryLine("// New Session: " + new java.util.Date +
            " Running: " + major + "." + minor + ", build " + build)
    	console.emitln("Version " + major + "." + minor + ", build " + build)
    	console.emitln("Web " + web)
    } else {
      if (history)
        addHistoryLine("// New Session: " + new java.util.Date)
      console.emitln("Failed to load version information.")
    }
  }
  
  def reportElapsed {
    console.sendln("elapsed: " + getLastTimeString + "\n")
  }
  
  /**
   * Make a parser.  This uses the current setting for tracing.
   * 
   * @param name    The name of the source to parse.
   * @return  The new parser.
   */
  private def _makeParser(name: String) = new ElisionParser(name, _trace)
  
  /**
   * Read the content of the provided file.  This method uses a
   * [[ornl.elision.util.FileResolver]] instance to find the requested
   * file.
   * 
   * @param filename		The file to read.  It may be absolute, or it may be
   * 										relative to the current directory.
   * @param quiet       If true, do not emit any error messages.
   * @return  True if the file was found and parse was successful; false if it was not.
   */
  def read(filename: String, quiet: Boolean): Boolean = {
    // Make a resolver from the properties.  Is this costly to do every time
    // we want to read a file?  Probably not.
    val usePath = getProperty[Boolean]("usepath")
    val useClassPath = getProperty[Boolean]("useclasspath")
    val path = getProperty[String]("path")
    val resolver = FileResolver(usePath, useClassPath, Some(path))
    var result = false
    resolver.find(filename) match {
      case None =>
        if (!quiet) console.error("File not found: " + filename)
        result = false
        
      case Some((reader, dir)) =>
        Processor.fileReadStack.push(filename)
        
        // Try to prepend our file's directory to our search path if it isn't
        // already in it.
        if(!path.contains(dir)) {
            val _prop = new scala.sys.SystemProperties
            setProperty[String]("path", dir + _prop("path.separator") + path) 
        }
        
        // Proceed with reading the file's stream.
        result = read(scala.io.Source.fromInputStream(reader), filename)
        
        // Restore our original path.
        setProperty[String]("path", path)
        Processor.fileReadStack.pop
    }
    result
  }
  
  /**
   * Read the content of the provided file.
   * 
   * @param file		The file to read.
   * @throws	java.io.IOException
   * 					The file cannot be found or cannot be read.
   * @return  True if the file was found and parse was successful; false if it was not.
   */
  def read(file: java.io.File) : Boolean = {
    read(scala.io.Source.fromFile(file), file.getAbsolutePath)
  }
  
  /**
   * Read the content of the provided reader.
   * 
   * @param source		The reader providing input.
   * @param filename  The file name, if relevant.
   * @throws	java.io.IOException
   * 					An error occurred trying to read.
   * @return  True if parse was successful; false if it was not.
   */
  def read(source: scala.io.Source, filename: String = "(console)") : Boolean = {
    _execute(_makeParser(filename).parseAtoms(source), true) match {
      case r : Success => true
      case r : Failure => false
    }
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
   * @param name    Name of the data source, to be provided to the parser.
   * @param text		The text to parse.
   */
  def execute(name: String, text: String) {
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
    _execute(_makeParser(name).parseAtoms(lline))
  }
  
  /**
   * Perform actions based on what got parsed.
   * 
   * @param result      Result of most recent parse.
   * @param stoponerror If true, immediately stop when an error is found.
   *                    This is accomplished by throwing an exception to
   *                    be caught at a higher level.
   */
  private def _execute(result: Presult, stoponerror: Boolean = false) : Presult = {
    import ornl.elision.util.ElisionException
    startTimer
    try {
    	result match {
  			case Failure(err) =>
  			  console.error(err)
  			  
  			case Success(nodes) =>
  			  // We assume that there is at least one handler; otherwise not much
  			  // will happen.  Process each node.
  			  console.reset
  			  for (node <- nodes) {
  			    _handleNode(node) match {
  			      case None =>
  			      case Some(newnode) =>
  			        // Interpret the node.
                val atom = newnode.interpret(context)
  			        _handleAtom(atom) match {
  			          case None =>
  			          case Some(newatom) =>
  			            // Hand off the node.
  			            _result(newatom)
  			        }
  			    }
  			    // Watch for errors.  If we are stopping on errors, stop.
  			    if (stoponerror && console.errors > 0) {
  			      throw new ElisionException(Loc.internal, "Stopping due to errors.")
  			    }
  			  } // Process all the nodes.
    	}
    } catch {
      case ee: ElisionException =>
        // An error is encountered, but we only skip the rest of execution at this level.
        console.error(ee.loc, ee.msg)
        
      case ex: Exception =>
        console.error("(" + ex.getClass + ") " + ex.getMessage())
        val trace = ex.getStackTrace()
        if (getProperty[Boolean]("stacktrace")) ex.printStackTrace()
        else console.error("in: " + trace(0))
        
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
        val trace = th.getStackTrace()
        if (getProperty[Boolean]("stacktrace")) th.printStackTrace()
        else console.error("in: " + trace(0))
        coredump("Internal error.", Some(th))
    }
    stopTimer
    showElapsed
    result
  }
  
  private def _handleNode(node: AST.BA): Option[AST.BA] = {
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

    var handlersCount = 1
    
    // We'll only send the GUI atom data here. This may change depending how 
    // we ultimately want the GUI to
    // receive data about the atoms it needs to visualize.
    ReplActor ! ("toGUI", "startBatch")
    ReplActor ! ("toGUI", (theAtom, "Parsed Atom: "))
    
    for (handler <- _queue) {
      handlersCount += 1
      
      handler.handleAtom(theAtom) match {
        case None => 
          return None
        case Some(alt) =>
          theAtom = alt
          ReplActor ! ("toGUI", (theAtom, "Handler " + (handlersCount - 1) + " result: "))
      }
    } // Perform all handlers.
    
    ReplActor ! ("toGUI", "endBatch")
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
    // If the trace state has changed.
    if (enable != _trace) {
      // The trace state has changed.  We keep this logic in case there are
      // things we need to do when the state changes.
      _trace = enable
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
  def coredump(msg: String, th: Option[Throwable] = None) {
    try {
      val cfile = new java.io.FileWriter("elision.core")
      if (cfile != null) {
        import Version._
        val prop = System.getProperties
        val info =
          <platform
            date={(new java.util.Date).toString}
            java.vendor={prop.get("java.vendor").toString}
            java.version={prop.get("java.version").toString}
            os.name={prop.get("os.name").toString}
            os.version={prop.get("os.version").toString}
            os.arch={prop.get("os.arch").toString}
            version={major+"."+minor}
            build={build}
            scala.version={scala.util.Properties.versionString}
          />
        val cont = <context>{context.toParseString}</context>
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
          {info}
      		{err}
      		{cont}
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
    } catch {
      case th: Throwable =>
        // Core dumping failed.  Emit a full stack trace and stop the world.
        println(th)
        th.printStackTrace()
        sys.exit(1)
    }
  }  

  /**  
   * Reloads a core dump created with fail().
   * 
   * @param corePath    The path to the core dump file.
   */
  def loadCoredump(corePath : String) = {
    import java.io._
    import xml._
    try {
      // Extract the XML from the core dump file.
      val coreFile = new File(corePath)
      val coreXML = XML.loadFile(coreFile)
      
      // Display information about the machine where the core dump was
      // created.
      val info = coreXML \ "platform"
      console.emitln("Creation platform:")
      console.emitln("  Created on ...... "+info \ "@date")
      console.emitln("  Java Vendor ..... "+info \ "@java.vendor")
      console.emitln("  Java Version .... "+info \ "@java.version")
      console.emitln("  OS Name ......... "+info \ "@os.name")
      console.emitln("  OS Version ...... "+info \ "@os.version")
      console.emitln("  OS Architecture . "+info \ "@os.arch")
      console.emitln("  Elision Version . "+info \ "@version")
      console.emitln("  Elision Build ... "+info \ "@build")
      console.emitln("  Scala Version ... "+info \ "@scala.version")
      console.emitln("")

      // Display the core dump's error information
      val err = coreXML \ "error"
      val errMsg = err \ "@message"
      console.emitln("Core dump error message:")
      console.emitln("  %s" format errMsg)
      console.emitln("")
      
      // Re-create the context.
      console.emitln("Reloading context...")
      val cont = (coreXML \ "context").text
      new ElisionParser(corePath).parseAtoms(cont) match {
        case Failure(err) =>
          console.error(err)
          console.emitln("Context cannot be reloaded.")
          
        case success: Success =>
          console.emitln("Successfully reloaded context.")
          console.emitln("Rebuilding context...")
          context = new Context()
          val prior = console.quiet
          console.quiet = 1
          _execute(success)
          console.quiet = prior
          console.emitln("Context rebuilt.")
      }

      // Reload the history (caution: this will change the contents of your
      // elision history file.)
      console.emitln("Reloading history...")
      val hist = (coreXML \ "history").text
      val histTokens = hist.split("\n")
      for(token <- histTokens) {
        val histLine = token.drop(token.indexOf(':') + 2)
        addHistoryLine(histLine)
      } // Load all history lines.
      console.emitln("Successfully reloaded history.")
    } catch {
      case fnfe : FileNotFoundException =>
        console.warn("Unable to open core dump at " + corePath)
        
      case exception: ElisionException =>
        console.error(exception.loc, exception.msg)
        
      case exception: Throwable =>
        exception.printStackTrace()
    }
  }
  
  /** Saves a context checkpoint. */
  def saveCheckPt : Int = {
    val date = new java.util.Date
    val contextCpy = context.clone
    val checkPt = (date, contextCpy)
    checkpoints += checkPt
    checkpoints.size - 1
  }

  /** Loads a context checkpoint. */
  def loadCheckPt(index : Int) : Boolean = {
    try{
      val checkpt = checkpoints(index)
      context = checkpt._2
      true
    } catch {
      case _: Throwable => false
    }
  }

  /** Displays the list of saved checkpoints. */
  def displayCheckPts : Unit = {
    console.emitln("Saved checkpoints: ")
    for (i <- 0 until checkpoints.size) {
      val (date, checkpt) = checkpoints(i)
      console.emitln(i + " saved at " + date)
    }
    if (checkpoints.isEmpty) console.emitln("None")
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
    def handleNode(node: AST.BA): Option[AST.BA] = Some(node)

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

  /** 
   * A stack used to keep track of the current file we are loading 
   * operators from. We use a stack here to support recursive file reading. 
   * This allows us to revert back to our previous file once we finish 
   * with an inc() instruction.
   */
  val fileReadStack = new collection.mutable.ArrayStack[String]
  fileReadStack.push("Console")
}
