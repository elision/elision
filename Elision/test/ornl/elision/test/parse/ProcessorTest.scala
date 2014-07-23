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
package ornl.elision.test.parse

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import ornl.elision.repl.ERepl
import ornl.elision.repl.ReplMain
import ornl.elision.cli.CLI
import scala.collection.mutable.HashMap
import ornl.elision.cli.CLI.CLIState

/**
 * @author jb9
 *
 */
class ProcessorTest extends AssertionsForJUnit {
  /**
   * Test method for {@link ornl.elision.parse.Processor#parse(java.lang.String)}.
   */
  @Test 
  def testParse() {
  val cli = ReplMain.prep(Array(""))
  var settings = new HashMap[String, String]
  settings("elision.root") = sys.env("FX_DIR") + java.io.File.separator + "config" +
    java.io.File.separator + "elision"
  settings("elision.history") = "history"
  settings("elision.context") = "context"
  settings("elision.rc") = "rc"
  settings("elision.cache") = "cache"

  val test = new ERepl(CLIState(List(), settings.toMap))
    ornl.elision.core.knownExecutor = test
    test.bootstrap(0)
    test.parse("(UnitTest)", "inc(\"" + sys.env("FX_DIR") + "/config/files_inc.eli\")")
    val a = test.parse("(UnitTest)", "mult_32(acc_32($M:FMAP(DWORD,BYTE),add_32(-12,$EBP:DWORD)),add_32(2,acc_32($M:FMAP(DWORD,BYTE),add_32(-4,$EBP:DWORD))))")
    println (a.toString)
  }

}
