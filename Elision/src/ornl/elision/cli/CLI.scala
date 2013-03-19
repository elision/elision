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
package ornl.elision.cli

import ornl.elision.util.Text
import ornl.elision.util.Debugger
import ornl.elision.util.toQuotedString

/**
 * Provide algorithms to implement a command line interface.
 */
object CLI {
  
  /** Whether this is the platform Windows. */
  val iswin = System.getProperty("os.name").toLowerCase().indexOf("win") >= 0
  
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
   * Encode the state of the command line after processing.
   * 
   * @param remain      Remaining command line arguments.
   * @param settings    Detected (and known) settings.
   * @param errstr      An error string, or `None` if no errors.
   * @param errindex    Index of the element causing the error.
   */
  case class CLIState(remain: List[String], settings: Map[String,String],
      errstr: Option[String], errindex: Int) {
    Debugger.debug(
        """|Creating a CLI State Object:
           |  remain:    %s
           |  settings:  %s
           |  errstr:    %s
           |  errindex:  %d
           |""".stripMargin.format(
               remain.mkString(","),
               settings.mkString(","),
               errstr.toString,
               errindex), "cli")
  }
    
  /**
   * Process the command line arguments, extract any switches and process them,
   * and then return the resulting structure.
   * 
   * Invoke this with the argument array and the defined switches.  Optionally
   * an argument handler can be provided.  The return value contains an optional
   * string that, if not `None`, specifies an error that stopped processing.
   * The index of the argument where processing stopped is also returned.
   * If the optional string is `None`, then no error was detected (and the
   * integer value is undefined).  In all cases the list of arguments that have
   * been fully processed and remain is returned.
   * 
   * Each command line argument is considered, and checked against the known
   * switches.  There are three patterns for a likely switch.  If a switch is
   * found, then the list of defined switches is checked and, if present, the
   * closure for the switch is invoked at that time.  Otherwise an error is
   * generated and processing stops.  Recognized switches are discarded.  The
   * switch handler may return `None` to indicate success, or it may return
   * an error message to stop processing with an error.
   * 
   * If not a switch, then the argument is checked to see if it is a setting.
   * These are indicated with a name=value pair enclosed in square brackets.
   * 
   * If the command line argument does not appear to be a switch or setting,
   * then it is passed to the argument handler.  The handler may return either
   * `None` or an error string, and a Boolean flag.  An error string stops
   * processing.  The flag indicates whether to preserve (`true`) the argument
   * in the list returned, or to discard (`false`) the argument.  Note that an
   * argument that causes an error will not be preserved, regardless of the
   * flag.
   * 
   * The default argument handler always returns `(None, true)`.
   * 
   * Switches come in the following forms:
   *  - Single-character switches, such as `-d` and `-x`.  These may be combined
   *    as `-dx`.  If an argument is required, it may be provided immediately
   *    after the switch, or it may be the next argument: `-dFoo` and  `-d Foo`.
   *    Because of this, a switch taking an argument must be the last switch
   *    given on a single dash.
   *  - Simple long switches, such as `--clear` and `--help`.
   *  - Long switches that take an argument, such as `--debug=Foo`.
   * Additionally, any argument that starts with three dashes is treated as
   * an argument, and not a switch.  The first two dashes are discarded.  Thus
   * the argument `---foo` becomes `-foo`.  This makes it possible to pass
   * arguments that start with a dash.
   * 
   * @param args          The command line arguments.
   * @param switches      Known command line switches.
   * @param settings      Known settings.
   * @param keepunknown   Preserve unknown settings.  If true, then any
   *                      settings that are not recognized are kept and
   *                      returned as arguments.
   * @param handler       A handler to invoke whenever an argument is found.
   *                      The default handler always returns `(None, true)`.
   * @return  The state of the command line after processing.
   */
  def apply(
      args: Array[String],
      switches: Seq[AbstractSwitch],
      settings: Seq[Setting],
      keepunknown: Boolean,
      handler: (String) => (Option[String], Boolean) = (str) => (None, true)):
      CLIState = {
    var index = 0
    var remain = List[String]()
    var sets = Map[String,String]()
    
    // Populate initial values for the settings.
    settings foreach (setting => setting.value match {
      case None =>
      case Some(value) => sets += (setting.name -> value)
    })
    
    // Process the command line arguments.
    while (index < args.size) {
      // See if this argument starts with a dash.  If so, it is possibly a
      // switch, and we will process it as such.
      val arg = args(index)
      Debugger.debug("Looking at argument %d: %s".format(
          index, toQuotedString(arg)), "cli")
      // The argument starts with a dash; it might be a switch.  Try to match
      // it against the different permissible kinds of switches.  We start
      // with the most complex first.
      val Assignment = "^--([^-=]+)=(.*)$".r
      val Longswitch = "^--([^-].*)$".r
      val Shortswitch = "^-([^-].*)$".r
      val Setting = """^\[(.*)=(.*)\]$""".r
      arg match {
        case Assignment(name, value) =>
          Debugger.debug("  Assignment("+toQuotedString(name)+","+
              toQuotedString(value)+")", "cli")
          // This is a switch that assigns a value to some named option.
          switches.find(_.long.getOrElse(null) == name) match {
            case None =>
              // Failed to find the switch.  Generate an error.
              return CLIState(remain, sets, Some("The switch --"+name+
                  " is not known."), index)
            case Some(sw: Switch) =>
              // The switch does not take an argument.
              return CLIState(remain, sets, Some("The switch --"+name+
                  " does not take an argument."), index)
            case Some(sw: ArgSwitch) =>
              // Found the correct switch.  Invoke its closure.
              sw.closure(value) match {
                case None => // Okay.
                case Some(err) => // Error
                  return CLIState(remain, sets, Some(err), index)
              }
          }
          
        case Longswitch(name) =>
          Debugger.debug("  Longswitch("+toQuotedString(name)+")", "cli")
          // This is just a long switch.
          switches find (_.long.getOrElse(null) == name) match {
            case None =>
              // Failed to find the switch.  Generate an error.
              return CLIState(remain, sets, Some("The switch --"+name+
                  " is not known."), index)
            case Some(sw: Switch) =>
              // Found the correct switch.  Invoke its closure.
              sw.closure() match {
                case None => // Okay.
                case Some(err) => // Error
                  return CLIState(remain, sets, Some(err), index)
              }
            case Some(sw: ArgSwitch) =>
              // The switch requires an argument.
              return CLIState(remain, sets, Some("The switch --"+name+
                  " requires an argument."), index)
          }
          
        case Shortswitch(singles) =>
          Debugger.debug("  Shortswitch("+toQuotedString(singles)+")", "cli")
          // This can be a collection of short switches (and maybe even
          // their arguments) on a single dash.
          var chindex = 0
          while (chindex < singles.length()) {
            val ch = singles(chindex)
            switches find (_.short.getOrElse(null) == ch) match {
              case None =>
                // Failed to find the switch.  Generate an error.
                return CLIState(remain, sets, Some("The switch -"+ch+
                    " is not known."), index)
              case Some(sw: Switch) =>
                // Found the correct switch.  Invoke its closure.
                sw.closure() match {
                  case None => // Okay.
                  case Some(err) => // Error
                    return CLIState(remain, sets, Some(err), index)
                }
              case Some(sw: ArgSwitch) =>
                // The switch requires an argument.  This will either be the
                // remainder of the text, or the next command line argument.
                // If there are characters after this one, assume they make
                // up the argument.
                if (chindex < singles.length - 1) {
                  // Found the argument.  Get it and invoke the closure.
                  sw.closure(singles.substring(chindex+1)) match {
                    case None => // Okay.
                    case Some(err) => // Error
                      return CLIState(remain, sets, Some(err), index)
                  }
                } else {
                  // The next command line argument must be the argument to
                  // this switch.  Get it.
                  index += 1
                  if (index < args.size) {
                    sw.closure(args(index)) match {
                      case None => // Okay.
                      case Some(err) => // Error
                        return CLIState(remain, sets, Some(err), index)
                    }
                  } else {
                    return CLIState(remain, sets, Some("The switch -"+ch+
                        " requires an argument."), index-1)
                  }
                }
                chindex = singles.length
            }
            chindex += 1
          } // Loop over all characters.
          
        case Setting(name, value) =>
          Debugger.debug("  Setting("+toQuotedString(name)+","+
              toQuotedString(value)+")", "cli")
          // Found a setting.  Decide what to do.
          settings find (_.name == name) match {
            case None =>
              // The setting was not found.  If we are preserving unknowns,
              // keep it.  If not, generate an error.
              if (keepunknown) {
                remain :+= arg
              } else {
                return CLIState(remain, sets, Some("The setting "+
                    ornl.elision.util.toQuotedString(name)+" is not known."),
                    index)
              }
            case Some(set: Setting) =>
              // The setting was found.  Save the value for later.
              sets += (name -> value)
          }
          
        case _ =>
          Debugger.debug("  Argument("+toQuotedString(arg)+")", "cli")
          // Does not match; must be an argument.  Handle it.
          // There is a special case to consider here.  If there are three
          // dashes at the start, then discard the first two.  This is a trick
          // to enable passing arguments that start with a dash.
          val value = (if (arg.startsWith("--")) arg.substring(2) else arg)
          handler(value) match {
            case (Some(error), _) =>
              return CLIState(remain, sets, Some(error), index)
            case (None, true) =>
              remain :+= value
            case (None, false) =>
              // Nothing to do.
          }
      }
      
      // Move to next argument.
      index += 1
    } // Process all command line arguments.
    
    // No errors.
    CLIState(remain, sets, None, index)
  }
  
  /** Get the console width.  It must be at least 60 characters. */
  private val _width =
    scala.tools.jline.TerminalFactory.create().getWidth() min 80
    
  /** Print help for a collection of switches and settings. */
  def apply(switches: Seq[AbstractSwitch], settings: Seq[Setting]) {
    // Sort the switches.
    val sorted = switches sortBy (_.name)
    
    // Get length of longest switch, limited to 30 characters.
    val nwidth = sorted.foldLeft(0)(_ max _.name.length) min 30
    
    // Figure out how much space is available for descriptions.
    val dwidth = _width - nwidth - 5
    
    // For each switch, print some basic information.
    if (!sorted.isEmpty) println("Switches:")
    val txt = new Text()
    sorted foreach {
      sw =>
        // If the name is longer than the name field, print it on its own line
        // and then dot over to the description field.
        if (sw.name.length > nwidth) {
          println(sw.name)
          print(" ")
          print("."*(nwidth+4))
        } else {
          print(sw.name)
          print(" ")
          print("."*(nwidth-sw.name.length+4))
        }
        print(" ")
        
        // Wrap the description field.
        txt.addln(sw.description)
        var first = true
        txt.wrap(dwidth) foreach {
          line =>
            if (first) {
              first = false
            } else {
              print(" "*(nwidth+6))
            }
            println(line)
        } // Print all lines of the description.
        txt.clear
    } // Print all switches.
    
    // Sort the settings.
    val setsorted = settings sortBy (_.name)
    
    // Get the length of the longest setting, limited to 30 characters.
    val setnwidth = setsorted.foldLeft(0)(_ max _.name.length) min 30
    
    // Figure out how much space is available for descriptions.
    val setdwidth = _width - setnwidth - 5
    
    // For each setting print some basic information.
    if (!setsorted.isEmpty) println("\nSettings:")
    setsorted foreach {
      set =>
        // If the name is longer than the name field, print it on its own line.
        if (set.name.length > setnwidth) {
          println(set.name)
          print(" ")
          print("."*(setnwidth+4))
        } else {
          print(set.name)
          print(" ")
          print("."*(setnwidth-set.name.length+4))
        }
        print(" ")
        
        // Complete the description and then wrap the description field.
        txt.add(set.description)
        if (set.varname.isDefined) {
          txt.add("  Can be set by the environment variable "+set.varname.get+".")
        }
        if (set.javaprop.isDefined) {
          txt.add("  Can be set by the Java property "+set.javaprop.get+".")
        }
        set.value match {
          case None =>
            txt.add("  Not currently set.")
          case Some(value) =>
            txt.add("  Current value: "+ornl.elision.util.toQuotedString(value))
        }
        var first = true
        txt.wrap(setdwidth) foreach {
          line =>
            if (first) {
              first = false
            } else {
              print(" "*(setnwidth+6))
            }
            println(line)
        } // Print all lines of the description.
        txt.clear
    } // Print all settings.
  }
}