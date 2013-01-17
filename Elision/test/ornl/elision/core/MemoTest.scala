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
package ornl.elision.core;

import org.scalatest.junit.AssertionsForJUnit
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import scala.collection.mutable.BitSet
import ornl.elision.repl.ERepl

/**
 * @author jb9
 *
 */
class MemoTest extends AssertionsForJUnit {
  /**
   * Test method for {@link ornl.elision.core.Memo#put(ornl.elision.core.BasicAtom, scala.collection.mutable.BitSet, ornl.elision.core.BasicAtom, int)}.
   */
  @Test
  def testPut() {
    val test = Memo
    val bitset = new BitSet
    def funcGen(i: Int)(t: Int): BasicAtom = {
      if (t > 1)
        Lambda(Variable("test " + i + "_" + t, "test_val_val " + i + "_" + t), funcGen(i)(t - 1))
      else
        Variable("test_val" + i + "_" + t, (i + "_" + t).toString)
    }
    val num = 10000
    val v = Array.fill[BasicAtom](num)(null)
    for (i <- 1 to num) {
      v(i - 1) = funcGen(i)(200)
    }
    println("starting to add")
    for (i <- 1 to num) {
      //println(f.toString)
      test.put(Variable("test " + i, "test_val " + i), bitset, v(i - 1), i % 10)
      //test.put(Lambda(Variable("test " + i, "test_val_val " + i), Variable(i, i.toString)), bitset, Lambda(Variable("lambda " + i, "lambda_val " + i), Variable(i, i.toString)), i % 10)
    }
    for (i <- 1 to 1000) {
      test.get(v(i - 1), bitset)
    }
    println(test.showStats)
  }
}
