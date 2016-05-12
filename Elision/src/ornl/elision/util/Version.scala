/*======================================================================
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 * The Elision Term Rewriter
 * 
 * Copyright (c) 2012 by UT-Battelle, LLC.
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
 * Collection of administrative costs for redistribution of the source code or
 * binary form is allowed. However, collection of a royalty or other fee in excess
 * of good faith amount for cost recovery for such redistribution is prohibited.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER, THE DOE, OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
======================================================================*/
package ornl.elision.util

import scala.language.existentials

/**
 * Provide information about the current version of Elision.
 * 
 * == Configuration ==
 * This information is obtained from the `configuration.xml` file
 * expected to be in the root of the current class path (thus in
 * the jar file).  It has the following format.
 * 
 * {{{
 * <configuration
 *   name = "name of program"
 *   maintainer = "name and email of maintainer"
 *   web = "web address of program">
 *   <version
 *     major = "major version number"
 *     minor = "minor version number"
 *     build = "build date / time identifier"/>
 *   <main name="command name" class="fully.qualified.class.name"
 *     description="human-readable description"/>
 *   <dialect name="dialect name" class="fully.qualified.class.name"/>
 *   <option name="option name" class="fully.qualified.class.name"
 *     description="human-readable description" value="value"/>
 * </configuration>
 * }}}
 * 
 * == Use ==
 * This object loads its data at construction time.  This is typically when
 * you first reference a field in this object.
 * 
 * The `loaded` field reveals if the file was successfully loaded.  If not, the
 * other fields contain default (and useless) information.  Missing fields
 * result in a value of the empty string for that field.  Extra fields not
 * specified above are ignored, but may be used in the future.  That is, the
 * absence of a tag is not license to use that tag; keep custom tags in a
 * separate, declared XML namespace, please.
 * 
 * Both the configuration and its DTD (`elision_configuration.dtd`) are
 * expected to be found in the root of the class path.
 * 
 * It is permissible for significant functionality to depend on the ability to
 * read this file.
 * 
 * == DTD ==
 * The following is the DTD for the configuration file.
 * 
 * {{{
 * <!ELEMENT configuration (version,(main|dialect|option)*,ANY*)>
 * <!ATTLIST configuration name CDATA #REQUIRED>
 * <!ATTLIST configuration maintainer CDATA #REQUIRED>
 * <!ATTLIST configuration web CDATA #REQUIRED>
 * <!ELEMENT version (ANY*)>
 * <!ATTLIST version major CDATA #REQUIRED>
 * <!ATTLIST version minor CDATA #REQUIRED>
 * <!ATTLIST version build CDATA #REQUIRED>
 * <!ATTLIST version trivial CDATA "0">
 * <!ATTLIST version status (alpha|beta|rc|final) "alpha">
 * <!ELEMENT main (ANY*)>
 * <!ATTLIST main name CDATA #REQUIRED>
 * <!ATTLIST main class CDATA #REQUIRED>
 * <!ATTLIST main description CDATA #REQUIRED>
 * <!ATTLIST main gui (true|false) #REQUIRED>
 * <!ELEMENT dialect (ANY*)>
 * <!ATTLIST dialect name CDATA #REQUIRED>
 * <!ATTLIST dialect class CDATA #REQUIRED>
 * <!ELEMENT option (ANY*)>
 * <!ATTLIST option name CDATA #REQUIRED>
 * <!ATTLIST option class CDATA #REQUIRED>
 * <!ATTLIST option description CDATA #REQUIRED>
 * <!ATTLIST option value CDATA #REQUIRED>
 * }}}
 */
object Version {
  /** Name of the program. */
  var name = "Elision"
    
  /** Name and email of the maintainer. */
  var maintainer = "<maintainer>"
    
  /** Web address for the program. */
  var web = "<web>"
    
  /** Major version number (integer string). */
  var major = "UNK"
    
  /** Minor version number (integer string). */
  var minor = "UNK"
    
  /** Twelve digit build identifier.  Date and time. */
  var build = "UNK"
    
  /** If true then data was loaded.  If false, it was not. */
  var loaded = false
  
  /** The default command to execute. */
  private var _default = "repl"
    
  /** A type for classes that have a main method. */
  type HasMain = Class[_ <: {def main(args: String*): Unit}]
  
  /** A type for an entry in the command catalog. */
  type CEntry = (HasMain, String, Boolean)
    
  /** The commands.  Map each command to its class and a description. */
  private var _commands: Map[String,CEntry] = Map()
  
  /** The dialects.  Map each dialect name to a class. */
  private var _dialects: Map[Symbol, AnyRef] = Map()
  
  /** A type for an entry in the options catalog. */
  type OEntry = (Class[_], String, String)
  
  /** The options.  Map each to its data. */
  private var _options: Map[String, OEntry] = Map()

  /**
   * An error occurred processing a main.
   * 
   * @param msg A human-readable message.
   */
  class MainException(msg: String) extends ElisionException(Loc.internal, msg)
  
  /**
   * Get a main class based on the command, or an unambiguous prefix.  The
   * exception is the empty string.  If we are passed the empty string, then
   * return the default command.
   * 
   * @param command The command prefix.  It is not case-sensitive.
   * @return  The requested class and command description, as a pair.
   */
  def get(command: String): CEntry = {
    if (command.length == 0) return _commands(_default)
    val sname = command.toLowerCase
    var found: Option[String] = None
    for (name <- _commands.keys) {
      // See if this is what we want.
      if (name.startsWith(sname)) {
        found match {
          case None =>
            // Found a candidate.
            found = Some(name)
          case Some(other) =>
            // Ambiguous.
            throw new MainException("The command " + command +
                " is ambiguous.  It could be " + other + " or " + name + ".")
        }
      }
    } // Search for the command.
    found match {
      case None =>
        throw new MainException("The command " + command + " is not known.")
      case Some(name) =>
        // Done!
        _commands(name)
    }
  }
  
  /**
   * Invoke the given command, passing the provided command line arguments.
   * 
   * @param command The requested command prefix.  It is not case sensitive.
   * @param args    The command line arguments.
   */
  def invoke(command: String, args: Array[String]) {
    require(args != null && command != null)
    // Get the actual command.
    val (clazz, _, gui) = get(command)
    Debugger("invoke") {
      Debugger.debugln("invoke", "Command:  " + command)
      Debugger.debugln("invoke", "Class:    " + clazz.toString())
      Debugger.debugln("invoke", "GUI:      " + gui)
      Debugger.debugln("invoke", "Arguments:" + args.mkString(","))
    }
    // Invoke the main method.
    clazz.getMethod("main",classOf[Array[String]]).invoke(null, args)
    // If not a gui, exit.
    if (!gui) System.exit(0)
  }
  
  /**
   * Return the loaded dialects.  This is a map from symbol to a class, but
   * the class has not been verified to be a dialect (yet).
   * 
   * @return  The known dialects.
   */
  def getDialects() = _dialects
  
  /**
   * Create a help string with the given width.
   */
  def help(buf: Appendable, width: Int = 80): Appendable = {
    buf.append("\nElision can be started in multiple ways.  The following " +
    		"commands start Elision\nas indicated.\n\n")
    var maxdwidth = 0
    for (pairs <- _commands) {
      maxdwidth = maxdwidth max pairs._2._2.length
    } // Print the commands.
    val usewidth = width max (maxdwidth+10)
    for (cmd <- _commands.keys.toList.sortWith((x,y) => x<y)) {
      val (_, description, _) = get(cmd)
      buf.append(" "+cmd+" ")
      buf.append("."*(usewidth - maxdwidth - 3 - cmd.length))
      buf.append(" "+description).append("\n")
    } // Append all the commands.
    buf.append("\nDefault is: " + _default + "\n")
    buf
  }
  
  private def _init {
    // This could be simplified if it were not necessary to configure the
    // parser to avoid validation.  This is necessary since Scala's
    // resolver does not find the DTD correctly.

    // Make an XML parser.
    val factory = javax.xml.parsers.SAXParserFactory.newInstance
    factory.setValidating(false)
    val parser = factory.newSAXParser
    
	  // Open the file.  We expect to find config.xml in the classpath.
	  val config_resource = getClass.getResource("/configuration.xml")
	  if (config_resource != null) {
	    // Parse the file.
	    val source = new org.xml.sax.InputSource(config_resource.toString)
	    val config = scala.xml.XML.loadXML(source, parser)
	    //val config = scala.xml.XML.load(config_resource)
	    
	    // Pull out the data and override the local defaults.
	    name = config.\("@name").text
	    maintainer = config.\("@maintainer").text
	    web = config.\("@web").text
	    major = config.\("version").\("@major").text
	    minor = config.\("version").\("@minor").text
	    build = config.\("version").\("@build").text
	    loaded = true
	    
	    // Pull out the mains.
	    var setdefault = true
	    for (main <- config.\("main")) {
	      // Pull out the parts, load the class, and store the parts.
	      val name = main.\("@name").text.toLowerCase
        val fqcn = main.\("@class").text
        val description = main.\("@description").text
        val gui = main.\("@gui").text.equalsIgnoreCase("true")
        try {
          // Load the class from its fully-qualified name.
          val clazz = Class.forName(fqcn).asInstanceOf[HasMain]
          // Since we have successfully loaded a main, make it the default and
          // reset the map.  If we never loaded anything we would preserve the
          // default map and command.
          if (setdefault) {
            _default = name
            _commands = Map[String,CEntry]()
            setdefault = false
          }
          _commands += (name -> (clazz, description, gui))
        } catch {
          case ex: Exception =>
            println("ERROR: Elision is mis-configured.  The class for " +
            		"command " + name + " could not be loaded.\nReason: " +
            		ex.toString)
        }
	    } // Pull out the mains.
	    
	    // Pull out the dialects.
	    for (dialect <- config.\("dialect")) {
	      // Pull out the parts.
	      val name = dialect.\("@name").text
	      val fqcn = dialect.\("@class").text
	      // We can load the class, but not check it without creating a
	      // dependency on the dialect registry, and we don't want that.
	      // So here we just store it and let the registry pull it later.
	      try {
	        val clazz = Class.forName(fqcn).newInstance()
	        _dialects += (Symbol(name) -> clazz.asInstanceOf[AnyRef])
	      } catch {
	        case ex: Exception =>
            println("ERROR: Elision is mis-configured.  The class for " +
                "dialect "+name+" could not be loaded and instantiated.\n" +
            		"Reason: " + ex.toString)
	      }
	    } // Pull out the dialects.
	  }
  }
  _init
}
