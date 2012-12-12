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
import ornl.elision.util.Text
import ornl.elision.cli.Switches
import ornl.elision.cli.SwitchUsage
import ornl.elision.cli.Switch
import ornl.elision.cli.ArgSwitch
import ornl.elision.util.Debugger

/**
 * This is the entry point when running from the jar file.  This also provides
 * the `fail` method that prints out command line invocation errors.
 * 
 * This should be the main class of the jar file.  This can be done by adding
 * the following to the jar file's Manifest.
 * 
 * {{{
 * Main-Class: ornl.elision.Main
 * }}}
 * 
 * If you want to add a command to Elision, you can do that by adding a new
 * main declaration to the `configuration.xml` file loaded by
 * [[ornl.elision.Version]].  You can then use [[ornl.elision.cli.Switches]]
 * to process command line arguments and switches in a consistent manner.
 */
object Main extends App {
  
  /**
   * Print out an error message about the command line, highlighting the
   * offending element.  You should follow this with calling `sys.exit(N)`,
   * where `N` is the (non-zero) exit value you want.
   * 
   * To use this pass the arguments, the position of the bad argument, and
   * the error message.  The arguments are printed, and the offending
   * argument is "underlined" with carets.
   * 
   * @param args      The command line arguments.
   * @param position  The index of the bad argument or switch.
   * @param err       Text describing the error.
   */
  def fail(args: Array[String], position: Int, err: String) {
    (new Text()).addln("ERROR: "+err).wrap(80) foreach (println(_))
    println()
    var index = 0
    var line = ""
    while (index < position) {
      print(args(index))
      print(" ")
      line += " "*(args(index).length + 1)
      index += 1
    } // Move to the offending item.
    println(args(index))
    print(line)
    println("^"*(args(index).length))
  }
  
  /**
   * Print usage information.  This is a switch handler (see
   * [[ornl.elision.cli.Switches]]) and satisfies the contract for such a
   * method.
   * 
   * @return  Always `None`.
   */
  private def _usage(): Option[String] = {
    println("Usage:")
    println("[global switches...] [command] [command switches and arguments...])")
    println()
    println("Global switches:")
    SwitchUsage(_globals)
    println()
    println("Try -h after a command to see help on the command.")
    System.exit(0)
    None
  }
  
  // Define the special global switches that can come before the command.
  private val _globals = Seq(
      Switch(Some("help"), Some('h'), "Provide basic usage information.", _usage _),
      ArgSwitch(Some("debug"), Some('d'), "Enable a debugging tag.", "TAG",
          (tag: String) => {
            Debugger.enableDebugModes(tag, Debugger.Mode.ON)
            None
          })
  )

  // Process the arguments and invoke the correct item.  If we received no
  // command, we pass the empty string to get the default command.
  try {
    // Process the switches.  Stop when the first argument is encountered.
    // To do this, we implement an argument handler that sets a flag and then
    // generates an error that terminates the argument parse.  We preserve
    // the argument.
    var okay = false
    var cmd = ""
    def firstarg(arg: String) = {
      okay = true
      cmd = arg
      (Some(""), true)
    }
    val (remain, err, pos) = Switches(args, _globals, firstarg _)
    
    // Check for an actual error, and display it if we find one.  We then exit.
    if (err != None && !okay) {
      // There was an actual error!
      fail(args, pos, err.get)
      sys.exit(1)
    }
    
    // Decide what to do.  If we have a command, invoke it.  If not, then
    // invoke the default command.  We add one to the position to omit the
    // command, and we note that if no command was present the slice is
    // empty (as it should be).
    Version.invoke(cmd, args.slice(pos+1, args.length))
  } catch {
    case ex: Version.MainException =>
      println("ERROR: " + ex.getMessage)
      sys.exit(2)
  }
}
