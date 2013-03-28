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
package ornl.elision.core

import java.io.File
import java.io.FileWriter
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import ornl.elision.util.Debugger

/**
 * Provide for the compilation and caching of native operators.
 */
class NativeCompiler {  
  /** Location of the native cache. */
  private val _cache = new File(System.getenv("HOME"), ".elision_cache")
  if (!_cache.exists) {
    _cache.mkdir
  }

  // Get the path separator and then use it to build the classpath as a string.
  // We need it to construct the settings.
  private val _prop = new scala.sys.SystemProperties
  private val _ps = _prop("path.separator")
  private lazy val _urls =
    java.lang.Thread.currentThread.getContextClassLoader match {
    case cl: java.net.URLClassLoader => cl.getURLs.toList
    case other => sys.error("classloader is not a URLClassLoader. " +
        "It is a " + other.getClass.getName)
  }
  private lazy val _classpath = (_urls.map(_.getPath)).mkString(_ps)
  
  // Build the settings, reporter, and compiler to use later on.
  val _settings = new Settings(Debugger.debugln _)
  _settings.deprecation.value = true
  _settings.unchecked.value = true
  _settings.outdir.value = _cache.getAbsolutePath
  _settings.classpath.value = _classpath
  val _reporter = new ConsoleReporter(_settings)
  val _compiler = new Global(_settings, _reporter)

  // /** Make an interpreter. */
  // private val _main = new scala.tools.nsc.interpreter.IMain(_settings) {}

  // Make the core package available.
  // _main.beQuietDuring(_main.interpret("import ornl.elision.core._"))

  /**
   * Class (using duck typing) for all handlers.
   */
  type HandlerClass = {
    def handler(_data: ApplyData): BasicAtom
  }
  
  /**
   * Add the method boilerplate.
   * @param handler The handler text.
   * @return  The method, ready for compilation.
   */
  def makeMethod(handler: String) =
    """|  def handler(_data: ApplyData): BasicAtom = {
       |    import _data._
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
  def makeObject(source: String, operator: String, key: String, method: String) =
    """|/**
       | * Native handler source.  This source file was automatically created.
       | * Operator name: %s
       | * Operator source: %s
       | * Created on: %s
       | */
       |import ornl.elision.core._
       |object %s {
       |%s
       |}
       |""".stripMargin format (operator, source,
           (new java.util.Date).toString, key, method)
  
  /**
   * Convert a native handler into a cache key.
   * @param source    The source file.
   * @param operator  The operator name.
   * @param handler   The handler.
   * @return  The cache key.
   */
  def getKey(source: String, operator: String, handler: String) = {
    val key = (source.hashCode() * 31 + handler.hashCode()).toHexString
    "NH" + operator.getBytes.map {
      "%02x".format(_)
    }.mkString("") + key
  }
  
  /**
   * If a cached native handler exists, load it now.
   * @param key The cache key.
   * @return  The optional class implementing the native handler.
   */
  def getCachedHandler(key: String): Option[ApplyData => BasicAtom] = {
    val file1 = new File(_cache, key + ".class")
    val file2 = new File(_cache, key + "$.class")
    if (file1.exists && file2.exists) {
      Debugger("opcache", "Reading cached file: " + file1.getAbsolutePath)
      Debugger("opcache", "Reading cached file: " + file2.getAbsolutePath)
      val clazz = new java.net.URLClassLoader(
          Array(file1.toURI.toURL, file2.toURI.toURL),
          this.getClass.getClassLoader).loadClass(key+"$")
      // Okay, now we have the object containing the handler.  Return the
      // handler.
      Debugger("opcache", "Found class: " + clazz)
      Debugger("opcache") {
        for (meth <- clazz.getMethods()) {
          println("  with method: " + meth)
        } // Print all methods.
      }
      val handlerclass = clazz.asInstanceOf[HandlerClass]
      Some(handlerclass.handler _)
    } else {
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
  def makeCachedHandler(source: String, operator: String, handler: String) = {
    // Create the source file.
    val key = getKey(source, operator, handler)
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
