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
package ornl.elision.repl

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
 * </configuration>
 * }}}
 * 
 * == Use ==
 * This object loads its data at construction time.  This is typically when
 * you first reference a field.
 * 
 * The `loaded` field reveals if the file was successfully loaded.  If not, the
 * other fields contain default (and useless) information.  Missing fields
 * result in a value of the empty string for that field.  Extra fields not
 * specified above are ignored, but may be used in the future.
 * 
 * Both the configuration and its DTD (`elision_configuration.dtd`) are
 * expected to be found in the root of the class path.
 * 
 * == DTD ==
 * The following is the DTD for the configuration file.
 * 
 * {{{
 * <!ELEMENT configuration (version,ANY*)>
 * <!ATTLIST configuration name CDATA #REQUIRED>
 * <!ATTLIST configuration maintainer CDATA #REQUIRED>
 * <!ATTLIST configuration web CDATA #REQUIRED>
 * <!ELEMENT version (ANY*)>
 * <!ATTLIST version major CDATA #REQUIRED>
 * <!ATTLIST version minor CDATA #REQUIRED>
 * <!ATTLIST version build CDATA #REQUIRED>
 * <!ATTLIST version trivial CDATA "0">
 * <!ATTLIST version status (alpha|beta|rc|final) "alpha">
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

  private def init {
	  // Open the file.  We expect to find config.xml in the classpath.
	  val config_resource = getClass.getResource("/configuration.xml")
	  if (config_resource != null) {
	    // Parse the file.
	    val config = scala.xml.XML.load(config_resource.toString())
	    
	    // Pull out the data and override the local defaults.
	    name = config.\("@name").text
	    maintainer = config.\("@maintainer").text
	    web = config.\("@web").text
	    major = config.\("version").\("@major").text
	    minor = config.\("version").\("@minor").text
	    build = config.\("version").\("@build").text
	    loaded = true
	  }
  }
  init
}

/**
 * Provide facilities for reading and interpreting Elision atoms.
 * 
 * == Purpose ==
 * This provides a generic framework for building interactive and batch
 * processing systems, and maintains a [[ornl.elision.core.Context]]
 * instance.
 * 
 * == Use ==
 * The executive maintains an instance of [[ornl.elision.core.Context]] to
 * store rules, operators, and bindings.
 * 
 * There are a few important methods available.
 *   * `interpret` takes text and attempts to parse the text to obtain atoms.
 *   * `append` expects to get text, and keeps track of whether there are open
 *     parentheses, braces, brackets, or verbatim blocks.  You can continue to
 *     append text; once the system detects that the input is "complete" (in
 *     that all parentheses, braces, brackets, and verbatim blocks are closed)
 *     it passes the complete text to `interpret`.
 *     
 * == Callbacks ==
 * Override the `execute` method.  This method is passed each atom as it is
 * found by `interpret`.  The default implementation does nothing.
 */
class Executive {
  import ornl.elision.core._
  
  /** The context used by this executive. */
	val context = new Context()
  
  /**
   * Override this method to receive every atom as it is obtained by the
   * interpret method.  The default imlementation does nothing.
   * 
   * @param atom	An atom.
   */
  def execute(atom: BasicAtom) {}
  
  def append(text: String) = {
    
  }
}
