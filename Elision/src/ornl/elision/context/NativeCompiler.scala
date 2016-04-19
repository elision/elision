/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2013 by Stacy Prowell (sprowell@gmail.com).
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
package ornl.elision.context

import java.io.File
import java.io.FileWriter
import scala.Array.canBuildFrom
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import ornl.elision.util.Debugger
import ornl.elision.util.Loc
import ornl.elision.core.BasicAtom
import ornl.elision.core.ApplyData
import ornl.elision.core.knownExecutor
import ornl.elision.core.toEString
import ornl.elision.core.toESymbol
import ornl.elision.core.NativeHandlerException
import java.net.URLClassLoader
import ornl.elision.util.Version

/**
 * Trait for all handlers.
 */
trait HandlerClass {
  def handler(_data: ApplyData): BasicAtom
}

object NativeCompiler {
  /**
   * Store local overrides.  The compiler checks here before it checks the
   * cache, so this provides an "in memory" cache.
   */
  private var _override = Map[String, HandlerClass]()
  
  /**
   * Stash a handler.  The compiler checks here before it checks the cache,
   * so this can provide an "override" with an in-memory cache.
   * 
   * @param source    The source file.
   * @param operator  The operator name.
   * @param handler   The handler.
   * @param obj       The object to stash.
   * @return	The key for the object stored.
   */
  def stash(source: String, operator: String, handler: String,
      obj: HandlerClass) = {
    // Compute the key and store the object.
    val key = getKey(source, operator, handler)
    _override += (key -> obj)
    key
  }
  
  /**
   * Write Scala code to stash a handler.  This adds the code to invoke
   * `stash` with the object.
   * 
   * @param source    The source file.
   * @param operator  The operator name.
   * @param handler   The handler.
   * @param app       An appendable to get the output.
   * @return  The appendable.
   */
  def writeStash(source: String, operator: String, handler: String,
      app: Appendable) = {
    // Compute the key.
    val key = getKey(source, operator, handler)
    // Write the prelude.
    app.append(makeObject(source, operator, key, makeMethod(handler)))
    app.append("ornl.elision.context.NativeCompiler.stash(%s, %s, %s, %s)\n" format (
        toEString(source), toEString(operator), toEString(handler),
        key))
  }
  
  /**
   * Convert a native handler into a cache key.
   * @param source    The source file.
   * @param operator  The operator name.
   * @param handler   The handler.
   * @return  The cache key.
   */
  private def getKey(source: String, operator: String, handler: String) = {
    //Use the version information so we rebuild if needed across Elision versions.
    val scala_version = util.Properties.versionString
    val elision_version = (Version.major, Version.minor, Version.build)
    val versionpart = (scala_version, elision_version).hashCode.toHexString
    // Remove whitespace from the handler. We are getting an etremely high rate
    // of recompilationf of native handlers without this.
    val stripped_handler = handler.replaceAll("\\s", "")
    val handlerpart = (source.trim, operator.trim,
                       stripped_handler).hashCode.toHexString
    val key = versionpart + handlerpart
    "NH" + key
  }
  
  /**
   * Add the method boilerplate.
   * @param handler The handler text.
   * @return  The method, ready for compilation.
   */
  private def makeMethod(handler: String) =
    """|  def handler(_data: ApplyData): BasicAtom = {
       |    import _data._
       |    // Force the correct context, in case there is another in scope.
       |    val context = _data.context
       |    import ApplyData._
       |    import console._
       |    // Start of handler code.
       |%s
       |    // End of handler code.
       |  }
       |""".stripMargin format (handler)
   
  /**
   * Add the object boilerplate around the handler method.
   * @param source    The source file.
   * @param operator  The operator name.
   * @param key       The cache key.
   * @param method    The method text.
   * @return  The object, ready for compilation.
   */
  private def makeObject(source: String, operator: String, key: String, method: String) =
    """|/**
       | * Native handler source.  This source file was automatically created.
       | * Operator name: %s
       | * Operator source: %s
       | * Created on: %s
       | */
       |import ornl.elision.context.{HandlerClass, Context}
       |import ornl.elision.core._
       |import ornl.elision.util.Loc
       |object %s extends HandlerClass {
       |%s
       |}
       |""".stripMargin format (operator, source,
           (new java.util.Date).toString, key, method)

}

/**
 * Provide for the compilation and caching of native operators.
 * 
 * The location of the native cache is obtained from the executor instance,
 * which must specify the configuration option `elision.cache`.
 */
class NativeCompiler {
  import NativeCompiler.{getKey, makeObject, makeMethod}
  
  /** Location of the native cache. */
  private val _cache = new File(knownExecutor.getSetting("elision.cache"))
  if (!_cache.exists) {
    _cache.mkdir
  }
  
  // Try to get a URL class loader.  If we cannot, give up!
  private val _cl = {
    var here = java.lang.Thread.currentThread.getContextClassLoader
    while (! here.isInstanceOf[URLClassLoader]) {
      here = here.getParent()
      if (here == null) {
        throw new NativeHandlerException(Loc.internal, "Unable to locate " +
        		"correct class loader and obtain class path.  Cannot continue.")
      }
    } // Find a URL class loader.
    here.asInstanceOf[URLClassLoader]
  }

  // Get the path separator and then use it to build the classpath as a string.
  // We need it to construct the settings.
  private val _prop = new scala.sys.SystemProperties
  private val _ps = _prop("path.separator")
  private val _urls = _cl.getURLs.toList
  private val _classpath = (_urls.map(_.getPath)).mkString(_ps)
  
  // Build the settings, reporter, and compiler to use later on.
  private val _settings = new Settings(knownExecutor.console.emitln _)
  _settings.deprecation.value = true
  _settings.unchecked.value = true
  _settings.outdir.value = _cache.getAbsolutePath
  _settings.classpath.value = _classpath
  private val _reporter = new ConsoleReporter(_settings)
  private val _compiler = new Global(_settings, _reporter)
  
  /**
   * Compile (or load a pre-compiled cached) handler.
   * 
   * @param loc       Location of the operator definition.
   * @param operator  Name of the operator to get the handler.
   * @param handler   The source code of the handler.
   * @return  The handler.
   */
  def compile(loc: Loc, operator: String, handler: String) = {
    val key = getKey(loc.source, operator, handler).intern()
    key.synchronized {
      getCachedHandler(key) match {
        case None =>
          // The handler was not found in the cache.  Make it now.
          Debugger("opcache", "Creating cached handler for "+toESymbol(operator)+
              " from "+loc.toShortString+".")
          makeCachedHandler(loc.source, key, operator, handler) match {
            case None =>
              // Somehow the native handler compilation failed.  Throw an
              // exception.
              throw new NativeHandlerException(loc,
                  "Unable to compile native handler for operator %s%s."
                  format (toESymbol(operator), loc.toShortString))
              
            case Some(handler) =>
              handler
          }
          
        case Some(handler) =>
          // The handler was found in the cache.
          handler
      }
    }
  }
  
  /**
   * If a cached native handler exists, load it now.
   * @param key The cache key.
   * @return  The optional class implementing the native handler.
   */
  private def getCachedHandler(key: String): Option[ApplyData => BasicAtom] = {
    // See if the handler is stashed.  If so, there is nothing to do.
    NativeCompiler._override.get(key) match {
      case None =>
      case Some(obj) =>
        return Some(obj.handler _)
    }
    
    // The handler is not in the memory cache.  Go to the disk cache and try
    // to load it.
    val classloader = new java.net.URLClassLoader(
        Array(_cache.toURI.toURL), this.getClass.getClassLoader)
    try {
      val clazz = classloader.loadClass(key+"$")
      // Okay, now we have the object containing the handler.  Return the
      // handler.
      Debugger("opcache", "Found class: " + clazz)
      Debugger("opcache") {
        for (meth <- clazz.getMethods()) {
          Debugger("opcache", "  with method: " + meth)
        } // Print all methods.
      }
      val handlerclass = clazz.getField("MODULE$").get(null).asInstanceOf[HandlerClass]
      Some(handlerclass.handler _)
    } catch {
      case cnfe: ClassNotFoundException =>
        None
    }
  }
  
  /**
   * Create and compile a cached handler and then load it.
   * @param source    The source file.
   * @param operator  The operator name.
   * @param handler   The handler.
   * @return  The compiled native handler.
   */
  private def makeCachedHandler(source: String, key: String, operator: String,
      handler: String) = {
    // Create the source file.
    val file = new File(_cache, key + ".scala")
    // Write the object.
    Debugger("opcache", "Writing cached file: " + file.getAbsolutePath)
    val writer = new FileWriter(file)
    writer.append(makeObject(source, operator, key, makeMethod(handler)))
    writer.flush
    writer.close
    if (!file.exists) {
      Debugger("opcache", "Unable to create file.")
      None
    } else {
      // Compile the source file.
      (new _compiler.Run).compile(List(file.getAbsolutePath))
      if (_reporter.hasErrors || _reporter.hasWarnings) {
        _reporter.printSummary
      }
      // Now load the source file.
      getCachedHandler(key)
    }
  }
}
