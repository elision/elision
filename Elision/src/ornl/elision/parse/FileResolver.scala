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
import java.io.{File, FileInputStream, InputStream}

/**
 * Common configuration for the file resolver.  This also provides for
 * construction of instances, including reuse of prior instances.
 */
object FileResolver {
  private val _known = scala.collection.mutable.Map[Any,FileResolver]()
  
  /** Access to system properties. */
  private val _prop = new scala.sys.SystemProperties
  
  /** The user's home folder. */
  val home = _prop("user.home")

  /** The path separator character. */
  private val _pathSeparator = _prop("path.separator")
    
  /**
   * The appropriate default path, initialized from the environment variable
   * `ELISION_PATH`.
   */
  val defaultPath = System.getenv("ELISION_PATH") match {
    case null => "."
    case str: String => str
  }
  
  /**
   * Get a file resolver.
   * 
   * @param usePath       If true, use the search path.  This can be provided via
   *                      the `setPath` methods, or from the `ELISION_PATH`
   *                      environment variable, if set.  In case neither is set,
   *                      the path `.` is used.  Default is true.
   * @param useClassPath  If true, use the class path to search for files.  This
   *                      is always used after attempting to use the search path.
   *                      Default is true.
   * @param path          Optional search path.  Default is `None`, and the path
   *                      is initialized from `ELISION_PATH` as described above.
   */
  def apply(usePath: Boolean, useClassPath: Boolean, path: Option[String]) = {
    _known.get((usePath, useClassPath, path)) match {
      case None =>
        val fr = new FileResolver(usePath, useClassPath, path)
        _known.put((usePath, useClassPath, path), fr)
        fr
      case Some(fr) => fr
    }
  }
}

/**
 * Given a file name locate the file in the search path or, if permitted, in
 * the classpath.
 * 
 * @param usePath       If true, use the search path.  This can be provided via
 *                      the `setPath` methods, or from the `ELISION_PATH`
 *                      environment variable, if set.  In case neither is set,
 *                      the path `.` is used.  Default is true.
 * @param useClassPath  If true, use the class path to search for files.  This
 *                      is always used after attempting to use the search path.
 *                      Default is true.
 * @param path          Optional search path.  Default is `None`, and the path
 *                      is initialized from `ELISION_PATH` as described above.
 */
class FileResolver(usePath: Boolean = true, useClassPath: Boolean = true,
    path: Option[String] = None) {
  import FileResolver._
  
  /** The correctly initialized search path. */
  private var _path = path match {
    case Some(str) => str.split(_pathSeparator)
    case None => defaultPath.split(_pathSeparator)
  }
  
  /**
   * Set the search path.  Any previous setting is discarded.
   * 
   * @param path    A sequence of folder names separated by the appropriate
   *                path separator character for the platform.
   * @return  This resolver.
   */
  def setPath(path: String): FileResolver = setPath(path.split(_pathSeparator))
  
  /**
   * Set the search path.  Any previous setting is discarded.
   * 
   * @param path    An array of folders names.
   * @return  This resolver.
   */
  def setPath(path: Array[String]): FileResolver = {
    _path = path
    this
  }
  
  /**
   * Locate the specified file in the allowed search paths.
   * 
   * If the provided name is an absolute path, no search is performed.
   * Otherwise the search path is used first (if enabled) and then the
   * class path is used (if enabled).  If the file is found, a reader is
   * created and returned.  Otherwise `None` is returned.
   * 
   * @param name    The file name, possibly including leading path.
   * @return  None if not found, or a reader to read the file and the path of the file's parent directory.
   */
  def find(name: String): Option[(InputStream, String)] = {
    try {
      // If the filename is absolute, we just try to open it and do not search.
      var file = new File(name)
      if (file.isAbsolute) {
        // The provided path is absolute.  Just open it.
        return Some(new FileInputStream(file),file.getParentFile.getCanonicalPath)
      }
      
      // See if we should look in the search path.  We always look there first
      // before we look in the classpath.  This is to allow overriding the
      // distribution files.
      if (usePath) {
        // We need to search the path.
        for (folder <- _path) {
          // Construct the file.
          file = new File(folder, name)
          // See if it exists.
          if (file.exists) {
            // The file has been found.  Just open it.
            return Some(new FileInputStream(file), file.getParentFile.getCanonicalPath)
          }
        } // Search the path.
      }
      
      // See if we should look in the class path.
      if (useClassPath) {
        // Try to get the file as a stream.  If it is not leading with a slash,
        // it needs to do so now.
        val fn = (if (name.startsWith("/")) name else "/"+name)
        val stream = getClass.getResourceAsStream(fn)
        if (stream != null) {
          return Some(stream, "")
        }
      }
    } catch {
      case ex:java.io.FileNotFoundException =>
    }
      
    // We are done.  We did not find the file.
    return None
  }
}
