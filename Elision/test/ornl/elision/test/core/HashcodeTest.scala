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

import scala.collection.mutable.BitSet

import org.junit.Test
import org.scalatest.junit.AssertionsForJUnit

import ornl.elision.core.BasicAtom
import ornl.elision.core.Lambda
import ornl.elision.core.Variable
import ornl.elision.core.strToLiteral

/**
 * @author jb9
 *
 */
class HashcodeTest extends AssertionsForJUnit {
 val numObjs = 10
 val ccaSize = 5000
 
 /** Constructs a deeply nested Lamda atom. */
 def funcGen(t: Int)(i: Int): BasicAtom = {
    if (t > 1)
      Lambda(Variable("test " + i + "_" + t, "test_val_val " + i + "_" + t), funcGen(t-1)(i))
    else
      Variable("test_val" + i + "_" + t, (i + "_" + t).toString)
  }
 
 
  /**
   * Test method for {@link ornl.elision.core.Memo#put(ornl.elision.core.BasicAtom, scala.collection.mutable.BitSet, ornl.elision.core.BasicAtom, int)}.
   */
  @Test
  def testHashCodes() {
    println("testHashCodes - atom depth: " + ccaSize)
    val bitset = new BitSet
    
    def f = funcGen(ccaSize)(_)
    println("starting to compute hash codes...")
    
    // compute hashCodes.
    val rt = Array.fill[Long](numObjs)(0)
    for (i <- 0 to numObjs-1) {
      val atom = f(i)
      
      rt(i) = System.currentTimeMillis
      atom.hashCode
      rt(i) = System.currentTimeMillis - rt(i)
    }
    println("Avg time to compute hashCode: " + rt.reduceLeft(_ + _) / rt.length + "(ms)") 
    
    // compute otherHashCodes
    val gt = Array.fill[Long](numObjs)(0)
    for (i <- 0 to numObjs-1) {
      val atom = f(i)
      
      gt(i) = System.currentTimeMillis
      atom.otherHashCode
      gt(i) = System.currentTimeMillis - gt(i)
    }
    println("Avg time to compute otherHashCode: " + gt.reduceLeft(_ + _) / gt.length + "(ms)") 
  }
}
