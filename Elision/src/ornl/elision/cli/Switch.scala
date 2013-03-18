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
