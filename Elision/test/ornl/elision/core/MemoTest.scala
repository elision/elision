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
import java.net.{ Socket, ServerSocket }
import java.util.concurrent.{ Executors, ExecutorService }
import java.util.Date
import java.util.concurrent.TimeUnit
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
    println("testPut")
    val test = Memo
    test.clear
    val bitset = new BitSet
    def funcGen(t: Int)(i: Int): BasicAtom = {
      if (t > 1)
        Lambda(Variable("test " + i + "_" + t, "test_val_val " + i + "_" + t), funcGen(i)(t - 1))
      else
        Variable("test_val" + i + "_" + t, (i + "_" + t).toString)
    }
    def f = funcGen(100)(_)
    val num = 12000
    val num2 = num + 10
    val starttime = System.currentTimeMillis
    for (i <- 0 to num - 1) {
      f(i)
    }
    println("Time to create " + num + " objects is " + (System.currentTimeMillis - starttime) + "(ms)")
    println("starting to add")
    val rt = Array.fill[Long](num)(0)
    for (i <- 0 to num - 1) {
      rt(i) = System.currentTimeMillis
      test.put(Variable("test " + i, "test_val " + i), bitset, f(i), i % 10)
      rt(i) = System.currentTimeMillis - rt(i)
    }
    val gt = Array.fill[Long](num2)(0)
    for (i <- 0 to num2 - 1) {
      gt(i) = System.currentTimeMillis
      test.get(f(i), bitset)
      gt(i) = System.currentTimeMillis - gt(i)
    }
    println(test.showStats)
    println("Avg time to add: " + rt.reduceLeft(_ + _) / rt.length + "(ms) and total time was " + rt.reduceLeft(_ + _))
    println("Avg time to get: " + gt.reduceLeft(_ + _) / gt.length + "(ms)")
  }

  @Test def testPutMultithreaded {
    println("testPutMultithreaded")
    val test = Memo
    test.clear
    val bitset = new BitSet
    def funcGen(t: Int)(i: Int): BasicAtom = {
      if (t > 1)
        Lambda(Variable("test " + i + "_" + t, "test_val_val " + i + "_" + t), funcGen(i)(t - 1))
      else
        Variable("test_val" + i + "_" + t, (i + "_" + t).toString)
    }
    def f = funcGen(100)(_)
    val num = 12000
    val num2 = num + 10
    val poolSize = 2
    val pool: ExecutorService = Executors.newFixedThreadPool(poolSize)
    val rt = Array.fill[Long](num)(0)

    class Handler(i: Integer) extends Runnable {
      def message = (Thread.currentThread.getName() + "\n").getBytes

      def run() {
        rt(i) = System.currentTimeMillis
        test.put(Variable("test " + i, "test_val " + i), bitset, f(i), i % 10)
        rt(i) = System.currentTimeMillis - rt(i)
      }
    }

    val starttime = System.currentTimeMillis
    for (i <- 0 to num - 1) {
      f(i)
    }
    println("Time to create " + num + " objects is " + (System.currentTimeMillis - starttime) + "(ms)")
    println("starting to add")
    val gt = Array.fill[Long](num2)(0)
    try {
      for (i <- 0 to num - 1) {
        pool.execute(new Handler(i))
      }
    } finally {
      pool.shutdown()
    }
    pool.awaitTermination(10, TimeUnit.MINUTES)
    for (i <- 0 to num2 - 1) {
      gt(i) = System.currentTimeMillis
      test.get(f(i), bitset)
      gt(i) = System.currentTimeMillis - gt(i)
    }
    println(test.showStats)
    println("Avg time to add: " + rt.reduceLeft(_ + _) / rt.length + "(ms) and total time was " + rt.reduceLeft(_ + _))
    println("Avg time to get: " + gt.reduceLeft(_ + _) / gt.length + "(ms)")
  }
  @Test def testPut10Threads {
    println("testPut10Threads")
    val test = Memo
    test.clear
    val bitset = new BitSet
    def funcGen(t: Int)(i: Int): BasicAtom = {
      if (t > 1)
        Lambda(Variable("test " + i + "_" + t, "test_val_val " + i + "_" + t), funcGen(i)(t - 1))
      else
        Variable("test_val" + i + "_" + t, (i + "_" + t).toString)
    }
    def f = funcGen(100)(_)
    val num = 12000
    val num2 = num + 10
    val poolSize = 10
    val pool: ExecutorService = Executors.newFixedThreadPool(poolSize)
    val rt = Array.fill[Long](num)(0)

    class Handler(i: Integer) extends Runnable {
      def message = (Thread.currentThread.getName() + "\n").getBytes

      def run() {
        rt(i) = System.currentTimeMillis
        test.put(Variable("test " + i, "test_val " + i), bitset, f(i), i % 10)
        rt(i) = System.currentTimeMillis - rt(i)
      }
    }

    val starttime = System.currentTimeMillis
    for (i <- 0 to num - 1) {
      f(i)
    }
    println("Time to create " + num + " objects is " + (System.currentTimeMillis - starttime) + "(ms)")
    val gt = Array.fill[Long](num2)(0)
    println("starting to add")
    try {
      for (i <- 0 to num - 1) {
        pool.execute(new Handler(i))
      }
    } finally {
      pool.shutdown()
    }
    pool.awaitTermination(10, TimeUnit.MINUTES)
    for (i <- 0 to num2 - 1) {
      gt(i) = System.currentTimeMillis
      test.get(f(i), bitset)
      gt(i) = System.currentTimeMillis - gt(i)
    }
    println(test.showStats)
    println("Avg time to add: " + rt.reduceLeft(_ + _) / rt.length + "(ms) and total time was " + rt.reduceLeft(_ + _))
    println("Avg time to get: " + gt.reduceLeft(_ + _) / gt.length + "(ms)")
  }
}
