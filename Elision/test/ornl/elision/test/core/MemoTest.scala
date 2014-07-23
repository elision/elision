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
package ornl.elision.test.core;

import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

import scala.collection.mutable.BitSet

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

import ornl.elision.core.BasicAtom
import ornl.elision.core.Lambda
import ornl.elision.core.Memo
import ornl.elision.core.Variable
import ornl.elision.core.strToLiteral

/**
 * @author jb9
 *
 */
class MemoTest extends AssertionsForJUnit {
 val numObjs = 8000 
 val ccaSize = 500
 var numThreads = 1
  
 
 def funcGen(t: Int)(i: Int): BasicAtom = {
   val j = i%8000
    if (t > 1)
      Lambda(Variable("test " + j + "_" + t, "test_val_val " + j + "_" + t), funcGen(t-1)(i))
    else
      Variable("test_val" + j + "_" + t, (j + "_" + t).toString)
  }
  
  /**
   * Test method for {@link ornl.elision.core.Memo#put(ornl.elision.core.BasicAtom, scala.collection.mutable.BitSet, ornl.elision.core.BasicAtom, int)}.
   */
  @Test
  def testPut() {
    println("testPut (Single-threaded)")
    val test = Memo
    test.clear
    val bitset = new BitSet
    
    def f = funcGen(ccaSize)(_)
    val num = numObjs
    val num2 = num + 10
    
    // create a bunch of atoms, but don't actually do anything with them
    val starttime = System.currentTimeMillis
    for (i <- 0 to num - 1) {
      f(i)
    }
    println("Time to create " + num + " objects is " + (System.currentTimeMillis - starttime) + "(ms)")
    
    // putting atoms into the Memo cache.
    println("starting to add")
    val rt = Array.fill[Long](num)(0)
    var totalAddTime = System.currentTimeMillis
    for (i <- 0 to num - 1) {
      val atom = f(i)
      rt(i) = System.currentTimeMillis
      test.put(atom, bitset, Variable("test " + i, "test_val " + i), i % 10)
      rt(i) = System.currentTimeMillis - rt(i)
    }
    totalAddTime = System.currentTimeMillis - totalAddTime
    
    // getting atoms from the Memo cache
    println("starting to get")
    val gt = Array.fill[Long](num2)(0)
    var totalGetTime = System.currentTimeMillis
    for (i <- 0 to num2 - 1) {
      val atom = f(i)
      gt(i) = System.currentTimeMillis
      test.get(atom, bitset)
      gt(i) = System.currentTimeMillis - gt(i)
    }
    totalGetTime = System.currentTimeMillis - totalGetTime
    
    // print results
    println(test.showStats)
    println("Avg time to add: " + rt.reduceLeft(_ + _) / rt.length + "(ms) and total time was " + totalAddTime) 
    println("Avg time to get: " + gt.reduceLeft(_ + _) / gt.length + "(ms) and total time was " + totalGetTime) 
  }
  
  
  def putThreaded {
    if(numThreads == 1) {
      testPut
      return
    }
    
    println("testPutThreaded (" + numThreads + " threads)")
    val test = Memo
    test.clear
    val bitset = new BitSet
    
    def f = funcGen(ccaSize)(_)
    val num = numObjs
    val num2 = num + 10
    val poolSize = numThreads
    var pool: ExecutorService = Executors.newFixedThreadPool(poolSize)
    val rt = Array.fill[Long](num)(0)
    val gt = Array.fill[Long](num2)(0)
    
    // thread for doing Memo.put
    class PutHandler(i: Int) extends Runnable {
      def message = (Thread.currentThread.getName() + "\n").getBytes

      def run() {
        val atom = f(i)
        rt(i) = System.currentTimeMillis
        test.put(atom, bitset, Variable("test " + i, "test_val " + i), i % 10)
        rt(i) = System.currentTimeMillis - rt(i)
      }
    }
    
    // thread for doing Memo.get
    class GetHandler(i: Int) extends Runnable {
      def message = (Thread.currentThread.getName() + "\n").getBytes

      def run() {
        val atom = f(i)
        gt(i) = System.currentTimeMillis
        test.get(atom, bitset)
        gt(i) = System.currentTimeMillis - gt(i)
      }
    }
    
    // create a bunch of atoms, but don't actually do anything with them
    val starttime = System.currentTimeMillis
    for (i <- 0 to num - 1) {
      f(i)
    }
    println("Time to create " + num + " objects is " + (System.currentTimeMillis - starttime) + "(ms)")
    
    // putting atoms into the Memo cache.
    println("starting to add")
    var totalAddTime = System.currentTimeMillis
    try {
      for (i <- 0 to num - 1) {
        pool.execute(new PutHandler(i))
      }
    } finally {
      pool.shutdown()
    }
    pool.awaitTermination(10, TimeUnit.MINUTES)
    totalAddTime = System.currentTimeMillis - totalAddTime
    
    pool = Executors.newFixedThreadPool(poolSize)
    
    // getting atoms from the Memo cache
    println("starting to get")
    var totalGetTime = System.currentTimeMillis
    try {
      for (i <- 0 to num2 - 1) {
        pool.execute(new GetHandler(i))
      }
    } finally {
      pool.shutdown()
    }
    pool.awaitTermination(10, TimeUnit.MINUTES)
    totalGetTime = System.currentTimeMillis - totalGetTime
    
    // print results
    println(test.showStats)
    println("Avg time to add: " + rt.reduceLeft(_ + _) / rt.length + "(ms) and total time was " + totalAddTime)
    println("Avg time to get: " + gt.reduceLeft(_ + _) / gt.length + "(ms) and total time was " + totalGetTime)
  }
  
  @Test
  def test2Threads {
    numThreads = 2
    putThreaded
  }
  
  @Test
  def test10Threads {
    numThreads = 10
    putThreaded
  }
  
  def test20Threads {
    numThreads = 20
    putThreaded
  }
}
