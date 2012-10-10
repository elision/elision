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
package ornl.elision
import scala.collection.mutable.StringBuilder

/**
 * This is the entry point when running from the jar file.
 */
object Main extends App {

  // Process the arguments and invoke the correct item.  If we received no
  // command, we pass the empty string to get the default command.
  try {
    if (args.length <= 0) {
      // Perform the default action.
      Version.invoke("", args)
    } else {
      // Remove the command, and invoke with the remainder.
      Version.invoke(args(0), args.slice(1,args.length))
    }
  } catch {
    case ex: Version.MainException =>
      println("ERROR: " + ex.getMessage)
  }
  
  /**
   * Provide command line help.
   */
  def help = {
    val buf = new StringBuilder
    buf.append(
        """|Usage: [global switches] [command] [command switches]
           |Execute the command [command].  Prior to this, the switches given
           |by [global switches] are processed.  The [command switches] are
           |passed to the selected [command].
           |
           |Global Switches:
           |""".stripMargin)
  }
  
  /**
   * Process the command line arguments, extract any switches and process them,
   * and then return the resulting structure.
   * 
   * @param args  The command line arguments.
   * @return  The processed arguments.
   */
  def processArguments(args: Array[String]) = {
    var index = 0
    while (index < args.size) {
      // See if this argument starts with a dash.  If so, it is possibly a
      // switch, and we will process it as such.
      val arg = args(index)
      if (arg.startsWith("-")) {
        // The argument starts with a dash; it might be a switch.  Try to match
        // it against the different permissible kinds of switches.  We start
        // with the most complex first.
        val Assignment = "^--([^=]+)=(.*)$".r
        val Longswitch = "^--(.*)$".r
        val Shortswitch = "^-(.*)$".r
        arg match {
          case Assignment(name, value) =>
            // This is a switch that assigns a value to some named option.
          case Longswitch(name) =>
            // This is just a long switch.
          case Shortswitch(singles) =>
            // This can be a collection of short switches (and maybe even
            // their arguments) on a single dash.
          case _ => // Does not match; skip it.
        }
      }
      
      // Move to next argument.
      index += 1
    } // Process all command line arguments.
  }
}
