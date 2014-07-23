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
import ornl.elision.cli.CLI
import ornl.elision.cli.Switch
import ornl.elision.cli.Setting
import ornl.elision.cli.ArgSwitch
import ornl.elision.util.Text
import ornl.elision.util.Version
import ornl.elision.util.Debugger
import ornl.elision.parse.ProcessorControl

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
 * [[ornl.elision.util.Version]].  You can then use [[ornl.elision.cli.CLI]]
 * to process command line arguments and switches in a consistent manner.
 */
object Main extends App {
  
  /**
   * Print usage information.  This is a switch handler (see
   * [[ornl.elision.cli.Switches]]) and satisfies the contract for such a
   * method.
   * 
   * @return  Always `None`.
   */
  private def _usage(): Option[String] = {
    println("Usage:")
    println("[global switches...] [command] [command switches and arguments...]")
    println()
    CLI(_globals, _settings)
    println()
    println("Use the command \"help\" to get a list of all commands.")
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
  
  // Define the special global settings that can be specified.
  private val _settings = Seq()

  // Process the arguments and invoke the correct item.  If we received no
  // command, we pass the empty string to get the default command.
  try {
    // Process the command line.  Stop when the first argument is encountered.
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
    val state = CLI(args, _globals, _settings, true, firstarg _)
    
    // Check for an actual error, and display it if we find one.  We then exit.
    if (state.errstr != None && !okay) {
      // There was an actual error!
      CLI.fail(args, state.errindex, state.errstr.get)
      sys.exit(1)
    }
    
    // Decide what to do.  If we have a command, invoke it.  If not, then
    // invoke the default command.  If the command was found, then we need to
    // omit it from the argument list.  Otherwise leave the list as-is.
    val start = (if (cmd.length == 0) state.errindex else state.errindex+1)
    val newargs = (state.remain ++ args.slice(start, args.length).toList).toArray
    Version.invoke(cmd, newargs)
  } catch {
    case ex: Version.MainException =>
      println("ERROR: " + ex.getMessage)
      sys.exit(2)
  }
}
