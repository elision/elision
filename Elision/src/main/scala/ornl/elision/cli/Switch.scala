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
package ornl.elision.cli
import ornl.elision.util.Text

/**
 * Provide methods to work with the command line arguments.
 */
object Switches {
    
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
   * If the command line argument does not appear to be a switch, then it is
   * passed to the argument handler.  The handler may return either `None` or
   * an error string, and a Boolean flag.  An error string stops processing.
   * The flag indicates whether to preserve (`true`) the argument in the list
   * returned, or to discard (`false`) the argument.  Note that an argument
   * that causes an error will not be preserved, regardless of the flag.
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
   * 
   * @param args      The command line arguments.
   * @param switches  Known command line switches.
   * @param handler   A handler to invoke whenever an argument is found.
   * @return  A tuple with the remaining arguments, an optional string that
   *          describes an error condition, and an integer that is the position
   *          of the argument causing the error (if any).
   */
  def apply(
      args: Array[String],
      switches: Seq[AbstractSwitch],
      handler: (String) => (Option[String], Boolean) = (str) => (None, true)):
      (List[String], Option[String], Int) = {
    var index = 0
    var remain = List[String]()
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
            switches.find(_.long.getOrElse(null) == name) match {
              case None =>
                // Failed to find the switch.  Generate an error.
                return (remain, Some("The switch --"+name+
                    " is not known."), index)
              case Some(sw: Switch) =>
                // The switch does not take an argument.
                return (remain, Some("The switch --"+name+
                    " does not take an argument."), index)
              case Some(sw: ArgSwitch) =>
                // Found the correct switch.  Invoke its closure.
                sw.closure(value) match {
                  case None => // Okay.
                  case Some(err) => // Error
                    return (remain, Some(err), index)
                }
            }
          case Longswitch(name) =>
            // This is just a long switch.
            switches find (_.long.getOrElse(null) == name) match {
              case None =>
                // Failed to find the switch.  Generate an error.
                return (remain, Some("The switch --"+name+
                    " is not known."), index)
              case Some(sw: Switch) =>
                // Found the correct switch.  Invoke its closure.
                sw.closure() match {
                  case None => // Okay.
                  case Some(err) => // Error
                    return (remain, Some(err), index)
                }
              case Some(sw: ArgSwitch) =>
                // The switch requires an argument.
                return (remain, Some("The switch --"+name+
                    " requires an argument."), index)
            }
          case Shortswitch(singles) =>
            // This can be a collection of short switches (and maybe even
            // their arguments) on a single dash.
            var chindex = 0
            while (chindex < singles.length()) {
              val ch = singles(chindex)
              switches find (_.short.getOrElse(null) == ch) match {
                case None =>
                  // Failed to find the switch.  Generate an error.
                  return (remain, Some("The switch -"+ch+
                      " is not known."), index)
                case Some(sw: Switch) =>
                  // Found the correct switch.  Invoke its closure.
                  sw.closure() match {
                    case None => // Okay.
                    case Some(err) => // Error
                      return (remain, Some(err), index)
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
                        return (remain, Some(err), index)
                    }
                  } else {
                    // The next command line argument must be the argument to
                    // this switch.  Get it.
                    index += 1
                    if (index < args.size) {
                      sw.closure(args(index)) match {
                        case None => // Okay.
                        case Some(err) => // Error
                          return (remain, Some(err), index)
                      }
                    } else {
                      return (remain, Some("The switch -"+ch+
                          " requires an argument."), index-1)
                    }
                  }
                  chindex = singles.length
              }
              chindex += 1
            } // Loop over all characters.
          case _ => // Does not match; must be an argument.  Handle it.
            handler(arg) match {
              case (Some(error), _) =>
                return (remain, Some(error), index)
              case (None, true) =>
                remain :+= arg
              case (None, false) =>
                // Nothing to do.
            }
        }
      } else {
        // Does not start with hyphen; assume it is an argument.
        handler(arg) match {
          case (Some(error), _) =>
            return (remain, Some(error), index)
          case (None, true) =>
            remain :+= arg
          case (None, false) =>
            // Nothing to do.
        }
      }
      
      // Move to next argument.
      index += 1
    } // Process all command line arguments.
    
    // No errors.
    (remain, None, index)
  }
}

/**
 * Provide usage help.
 */
object SwitchUsage {
  /** Get the console width.  It must be at least 60 characters. */
  private val _width =
    scala.tools.jline.TerminalFactory.create().getWidth() min 80
  
  /** Print help for a collection of switches. */
  def apply(switches: Seq[AbstractSwitch]) {
    // Sort the switches.
    val sorted = switches sortBy (_.name)
    
    // Get length of longest switch, limited to 30 characters.
    val nwidth = sorted.foldLeft(0)(_ max _.name.length) min 30
    // Figure out how much space is available for descriptions.
    val dwidth = _width - nwidth - 5
    
    // For each switch, print some basic information.
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
  }
}

/**
 * Unifying class for all switches.
 * 
 * @param long        Long version of the switch.
 * @param short       Short version of the switch.
 * @param description Human-readable description of the switch.
 */
sealed abstract class AbstractSwitch(
    val long: Option[String],
    val short: Option[Char],
    val description: String) {
  require(long != None || short != None)
  
  /** Printable version of the switch.  Does not include description. */
  val name: String
}

/**
 * Provide an implementation of a command line switch.
 * 
 * @param long        Long version of the switch.
 * @param short       Short version of the switch.
 * @param description Human-readable description of the switch.
 * @param closure     Closure to invoke when the switch is read.  The closure
 *                    takes no arguments and returns `None` on success or a
 *                    string describing an error condition.
 */
case class Switch(
    override val long: Option[String],
    override val short: Option[Char],
    override val description: String,
    closure: () => Option[String] = () => None)
    extends AbstractSwitch(long, short, description) {
  val name = short match {
    case None => "--"+long
    case Some(ch) => "-"+ch+(
        long match {
          case None => ""
          case Some(str) => ", --"+str
        })
  }
}

/**
 * Represent a switch that takes an argument.
 * 
 * @param long        Long version of the switch.
 * @param short       Short version of the switch.
 * @param description Human-readable description of the switch.
 * @param argument    Name of the argument to the switch.
 * @param closure     Closure to invoke when the switch is read.  The closure
 *                    takes the argument to the switch as its argument, and
 *                    returns `None` on success or a string describing an
 *                    error condition.
 */
case class ArgSwitch(
    override val long: Option[String],
    override val short: Option[Char],
    override val description: String,
    argument: String,
    closure: (String) => Option[String] = (arg) => None)
    extends AbstractSwitch(long, short, description) {
  val name = short match {
    case None => "--"+long+"=["+argument+"]"
    case Some(ch) => "-"+ch+(
        long match {
          case None => " ["+argument+"]"
          case Some(str) => ", --"+str+"=["+argument+"]"
        })
  }
}
