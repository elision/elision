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
import ornl.elision.util.PrintConsole
import ornl.elision.util.FileResolver
import ornl.elision.util.Timeable

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
class Processor(var context: Context = new Context)
extends Executor
with TraceableParse
with ToggleableParser
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
  
  /** Whether to trace the parser. */
  private var _trace = false

  /** Select the parser to use */
  private var _toggle = false
  
  /** The queue of handlers, in order. */
  private var _queue = List[Processor.Handler]()
  
  /** The parser to use. */
  private var _parser = new AtomParser(context, _trace, _toggle)
  
  /** Specify the console.  We don't know the number of lines. */
  val console = PrintConsole
  
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
   * Read the content of the provided file.  This method uses a
   * [[ornl.elision.parse.FileResolver]] instance to find the requested
   * file.
   * 
   * @param filename		The file to read.  It may be absolute, or it may be
   * 										relative to the current directory.
   * @param quiet       If true, do not emit any error messages.
   * @return  True if the file was found; false if it was not.
   */
  def read(filename: String, quiet: Boolean): Boolean = {
    // Make a resolver from the properties.  Is this costly to do every time
    // we want to read a file?  Probably not.
    val usePath = getProperty[Boolean]("usepath")
    val useClassPath = getProperty[Boolean]("useclasspath")
    val path = getProperty[String]("path")
    val resolver = FileResolver(usePath, useClassPath, Some(path))
    resolver.find(filename) match {
      case None =>
        if (!quiet) console.error("File not found: " + filename)
        false
      case Some(reader) =>
        Processor.fileReadStack.push(filename)
        read(scala.io.Source.fromInputStream(reader), filename)
        Processor.fileReadStack.pop
        true
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
    ReplActor ! ("Eva", "pushTable", "Processor read")
    ReplActor ! ("Eva", "addToSubroot", ("read", "Reading: " + filename))
    ReplActor ! ("Eva", "setSubroot", "read")
    _execute(_parser.parseAtoms(source)) 
    ReplActor ! ("Eva", "popTable", "Processor read")
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
//	ReplActor ! ("Eva", "newTree", lline) // val treeRoot = RWTree.createNewRoot(lline) 
	//////////////////// end GUI changes
	
    _execute(_parser.parseAtoms(lline))
	
	//////////////////// GUI changes
	// send the completed rewrite tree to the GUI's actor
//	if(ReplActor.guiActor != null && !ReplActor.disableGUIComs && lline != "")
//		ReplActor ! ("Eva", "finishTree", None) //ReplActor.guiActor ! treeRoot
	//////////////////// end GUI changes
  }
  
  def parse(text: String) = {
    _parser.parseAtoms(text) match {
      case Failure(err) => ParseFailure(err)
      case Success(nodes) => ParseSuccess(nodes map (_.interpret))
    }
  }
  
  private def _execute(result: Presult) {
    import ornl.elision.util.ElisionException
    startTimer
	
    ReplActor ! ("Eva","pushTable","Processor _execute")
    
    try {
    	result match {
  			case Failure(err) => console.error(err)
  			case Success(nodes) =>
  			  // We assume that there is at least one handler; otherwise not much
  			  // will happen.  Process each node.
  			  for (node <- nodes) {
				//////////////////// GUI changes
                ReplActor ! ("Eva", "setSubroot", "subroot")
                var nodeLabel : String = "line node: " // TODO: This should be the parse string of the original term represented by this node.
                ReplActor ! ("Eva", "addToSubroot", ("lineNode", nodeLabel)) //val lineNode = RWTree.addTo(rwNode, nodeLabel)
				ReplActor ! ("Eva", "setSubroot", "lineNode") //RWTree.current = lineNode
				
				
  			    _handleNode(node) match {
  			      case None =>
  			      case Some(newnode) =>
  			        // Interpret the node.
                    ReplActor ! ("Eva", "addTo", ("lineNode", "interpret", "Interpretation Tree: "))
                    ReplActor ! ("Eva", "setSubroot", "interpret") // RWTree.current = lineNode // GUI changes
  			        val atom = newnode.interpret
                    
                    ReplActor ! ("Eva", "addTo", ("lineNode", "handle", "Handler Tree: "))
                    ReplActor ! ("Eva", "setSubroot", "handle")
  			        _handleAtom(atom) match {
  			          case None =>
  			          case Some(newatom) =>
                        ReplActor ! ("Eva", "addTo", ("lineNode", "result", "Result Tree: "))
                        ReplActor ! ("Eva", "setSubroot", "result") //RWTree.current = lineNode
  			            // Hand off the node.
  			            _result(newatom)
  			        }
  			    }
  			  } // Process all the nodes.
              //////////////////// end GUI changes
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
    
    ReplActor ! ("Eva","popTable","Processor _execute")
    
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
	ReplActor ! ("Eva", "pushTable", "_handleAtom")
	// add this atom as a child to the root node
	ReplActor ! ("Eva", "addToSubroot", ("atomNode", atom)) //val atomNode = RWTree.addTo(rwNode, atom)
	
	//////////////////// end GUI changes
	
    for (handler <- _queue) {
      ReplActor ! ("Eva", "setSubroot", "atomNode") //RWTree.current = atomNode // GUI change
      handler.handleAtom(theAtom) match {
        case None => 
            ReplActor ! ("Eva", "popTable", "_handleAtom") // GUI change
            return None
        case Some(alt) => theAtom = alt
      }
    } // Perform all handlers.
    
    ReplActor ! ("Eva", "popTable", "_handleAtom") // GUI change
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
      _parser = new AtomParser(context, _trace, _toggle)
    }
  }
 
  /**
   * Specify whether to trace the parser.
   * 
   * @param enable  If true, trace the parser.  If false, do not.
   */
  def toggle_=(enable: Boolean) {
    // If the toggle state has changed, re-create the parser.
    if (enable != _toggle) {
      // The toggle state has changed.  Re-create the parser.
      _toggle = enable
      _parser = new AtomParser(context, _trace, _toggle)
    }
  }
  
  /**
   * Determine whether tracing is enabled.
   */
  def trace = _trace

  /**
   * Determine which parser to use
   */
  def toggle = _toggle
  
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
    try {
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
            val cfile = new java.io.PrintWriter("coreReloadResults.txt")
            
            def corePrint(str : String) {
                cfile.println(str)
                console.emitln(str)
            }
            
            // Extract the XML from the core dump file.
            val coreFile = new File(corePath)
            val coreXML = XML.loadFile(coreFile)
            
            // Display the core dump's error information
            val err = coreXML \ "error"
            val errMsg = err \ "@message"
            corePrint("Core dump error message: " + errMsg)
            
            val stackTrace = (err \\ "item").map(_.text).mkString("\n") 
            corePrint(stackTrace)
            
            corePrint("Reloading context...")
            
            val ops = (coreXML \ "operator-library").text
            val binds = (coreXML \ "binds").text
            val rules = (coreXML \ "rule-library").text
            
            // In the reloading process, it's likely that the operators, bindings, and rules are out of order.
            // So in order to try to resolve all the rules, we will perform an initial iteration to read in this data, 
            // and then we will perform more iterations until either all the elements are successfully
            // read in or we are unable to successfully read any more remaining elements. 
            
            val unresolved = new collection.mutable.Queue[AstNode]
            val origBinds = context.binds
            val origOpLib = context.operatorLibrary
            val origRuleLib = context.ruleLibrary
            
            // Parse all the elements into AstNodes
            
            corePrint("Parsing operators...")
            val unresolvedOps = new collection.mutable.Queue[AstNode]
            val reOpLib = new OperatorLibrary(context.operatorLibrary.allowRedefinition)
            
            _parser.parseAtoms(ops) match {
                case Failure(err) =>
                    corePrint(err)
                case Success(nodes) =>
                    context.operatorLibrary = reOpLib
                    
                    for(node <- nodes) {
                        unresolvedOps.enqueue(node)
                    }
            }
            
            corePrint("Parsing bindings...")
            val unresolvedBinds = new collection.mutable.Queue[AstNode]
            
            _parser.parseAtoms(binds) match {
                case Failure(err) =>
                    corePrint(err)
                case Success(nodes) =>
                    unresolvedBinds.enqueue(nodes(0))
            }
            
            corePrint("Parsing rules...")
            val unresolvedRules = new collection.mutable.Queue[AstNode]
            val reRuleLib = new RuleLibrary(context.ruleLibrary.allowUndeclared)
            
            _parser.parseAtoms(rules) match {
                case Failure(err) =>
                    corePrint(err)
                case Success(nodes) =>
                    context.ruleLibrary = reRuleLib
                    
                    for(node <- nodes) {
                        unresolvedRules.enqueue(node)
                    }
            }
            
            // iterate until all elements are resolved or no more elements could be resolved.
            
            var lastNetQSize = unresolvedOps.size + unresolvedBinds.size + unresolvedRules.size + 1
            var iterations = 1
            
            while(!(unresolvedOps.isEmpty && unresolvedBinds.isEmpty && unresolvedRules.isEmpty) && 
                unresolvedOps.size + unresolvedBinds.size + unresolvedRules.size < lastNetQSize) {
                
                lastNetQSize = unresolvedOps.size + unresolvedBinds.size + unresolvedRules.size
                corePrint("\nReloading elements: Iteration " + iterations + "...")
                
                // attempt to reload operators.
                if(!unresolvedOps.isEmpty) {
                    corePrint(" Reloading operators...")
                    val opSize = unresolvedOps.size
                    for(i <- 0 until opSize) {
                        val node = unresolvedOps.dequeue
                        try {
                            node.interpret match {
                                case op : Operator =>
                                    reOpLib.add(op)
                                    corePrint(" Added operator " + op.name)
                                case _ =>
                                    corePrint(" Encountered a non-Operator")
                            }
                        }
                        catch {
                            case _ =>
                                unresolvedOps.enqueue(node)
                        }
                    }
                }
                
                // attempt to reload bindings.
                if(!unresolvedBinds.isEmpty) {
                    corePrint(" Reloading bindings...")
                    
                    val node = unresolvedBinds.dequeue
                    try {
                        node.interpret match {
                            case reBinds : BindingsAtom =>
                                context.binds = reBinds.mybinds
                                corePrint(" Added the bindings")
                            case _ =>
                                corePrint(" Encountered a non-BindingsAtom")
                        }
                    }
                    catch {
                        case _ =>
                            unresolvedBinds.enqueue(node)
                    }
                    
                }
                
                // attempt to reload rules.
                if(!unresolvedRules.isEmpty) {
                    corePrint(" Reloading rules...")
                    val ruleSize = unresolvedRules.size
                    for(i <- 0 until ruleSize) {
                        val node = unresolvedRules.dequeue
                        try {
                            node.interpret match {
                                case rule : RewriteRule =>
                                    reRuleLib.add(rule)
                                    corePrint(" Added a rule ")
                                case _ =>
                                    corePrint(" Encountered a non-RewriteRule")
                            }
                        }
                        catch {
                            case _ =>
                                unresolvedRules.enqueue(node)
                        }
                    }
                }
                
                iterations += 1
            }
            
            // show reloading results
            
            if(unresolvedOps.isEmpty && unresolvedBinds.isEmpty && unresolvedRules.isEmpty) {
                corePrint("\nSuccessfully reloaded the context!")
            }
            else {
                context.operatorLibrary = origOpLib
                context.binds = origBinds
                context.ruleLibrary = origRuleLib
                corePrint("\nFailed to reload the context. Restored the original context.")
            }
            
            if(!unresolvedOps.isEmpty) {
                corePrint("\nFailed to reload " + unresolvedOps.size + " operators: ")
                while(!unresolvedOps.isEmpty) {
                    val node = unresolvedOps.dequeue
                    try {
                        node.interpret
                    }
                    catch {
                        case err : Throwable =>
                            corePrint(err.getMessage)
                    }
                }
            }
            
            if(!unresolvedBinds.isEmpty) {
                corePrint("\nFailed to reload the bindings")
                while(!unresolvedBinds.isEmpty) {
                    val node = unresolvedBinds.dequeue
                    try {
                        node.interpret
                    }
                    catch {
                        case err : Throwable =>
                            corePrint(err.getMessage)
                    }
                }
            }
            
            if(!unresolvedRules.isEmpty) {
                corePrint("\nFailed to reload " + unresolvedRules.size + " rules: ")
                while(!unresolvedRules.isEmpty) {
                    val node = unresolvedRules.dequeue
                    try {
                        node.interpret
                    }
                    catch {
                        case err : Throwable =>
                            corePrint(err.getMessage)
                    }
                }
            }
            
            cfile.close
            console.emitln("\nCore dump reload results printed to coreReloadResults.txt.")
            
            // reload the history (caution: this will change the contents of your elision history file.)
            console.emitln("Reloading history...")
            val hist = (coreXML \ "history").text
            val histTokens = hist.split("\n")
            try {
                for(token <- histTokens) {
                    val histLine = token.drop(token.indexOf(':') + 2)
                    addHistoryLine(histLine)
                }
                console.emitln("Successfully reloaded history.")
            }
            catch {
                case _ => 
                    console.emitln("Failed to reload the history")
            }
            
        }
        catch {
            case fnfe : FileNotFoundException =>
                console.warn("Unable to open core dump at " + corePath)
        }
   }
   
   
   /** Saves a context checkpoint. */
   def saveCheckPt : Int = {
        val date = new java.util.Date
        
        val contextCpy = context.cloneContext
        
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
        }
        catch {
            case _ => false
        }
   }
   
   /** Displays the list of saved checkpoints. */
   def displayCheckPts : Unit = {
        console.emitln("Saved checkpoints: ")
        for(i <- 0 until checkpoints.size) {
            val (date, checkpt) = checkpoints(i)
            console.emitln(i + " saved at " + date)
        }
        if(checkpoints.isEmpty) console.emitln("None")
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
  
  /** 
   * A stack used to keep track of the current file we are loading operators from. 
   * We use a stack here since we might access another file using operators such as inc(). 
   * So this allows us to revert back to our previous file once we finish with an inc() instruction.
   */
  val fileReadStack = new collection.mutable.ArrayStack[String]
  fileReadStack.push("Console")

}