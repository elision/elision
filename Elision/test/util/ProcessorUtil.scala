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
package util

import ornl.elision.repl.ReplMain
import ornl.elision.repl.ERepl
import ornl.elision.cli.Setting
import java.io.File
import ornl.elision.cli.CLI
import scala.collection.mutable.HashMap
import ornl.elision.cli.CLI.CLIState
import ornl.elision.parse.Processor._
import ornl.elision.core.BasicAtom
/**
 * @author fxuser
 *
 */
trait ProcessorUtil {
  val cli = ReplMain.prep(Array(""))
  var settings = new HashMap[String, String]
  settings("elision.root") = sys.env("FX_DIR") + java.io.File.separator + "config" +
    java.io.File.separator + "elision"
  settings("elision.history") = "history"
  settings("elision.context") = "context"
  settings("elision.rc") = "rc"
  settings("elision.cache") = "cache"

  val repl = new ERepl(CLIState(List(), settings.toMap))
  ornl.elision.core.knownExecutor = repl
  repl.bootstrap(0)

  /**
   * Convert a result from parsing to a list of basic atoms.
   * This is currently used for unit testing.
   *
   * @param result    The result from Processor.parse.
   * @param success   On a ParseSuccess call this function.
   * @param failure   On any other result call this function
   * @return          A list of the atoms returned or an empty list if the
   *                  result was not a success.
   */
  def toBasicAtom(result: repl.ParseResult)(success: BasicAtom => Unit)(failure: String => Unit) = result match {
    case repl.ParseSuccess(atoms) =>
      val lib = repl.context.ruleLibrary
      val firstnode = atoms.head
      val ra = lib.rewrite(firstnode)._1
      success(ra)
    case _ =>
      failure("Round trip testing failed for atom:\n")
  }
}