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
package sjp.elision.test
import sjp.elision.core._

/**
 * Generate instances of a thing according to combinatorial rules.
 */
abstract class Generator[A] extends Iterator[A] {
  protected var _exhausted = false
  protected var _current: A = null.asInstanceOf[A] 
  def next =
    if (_current != null || hasNext) {
      val tmp = _current
      _current = null.asInstanceOf[A]
      tmp
    } else {
      null.asInstanceOf[A]
    }
  def hasNext = _current != null || findNext
  def findNext: Boolean
}

abstract class SimpleGenerator[A] extends Generator[A] {
  val instances: Seq[A]
  lazy val iter = instances.iterator
  def findNext = {
    _current = iter.next
    _current != null
  }
}

class StringGen extends SimpleGenerator[String] {
  val instances = Seq("", "\"", "\n", "Fred", "Ti\tm", "`'J\n \"$m")
}

class IntegerGen extends SimpleGenerator[BigInt] {
  val instances = Seq[BigInt](0, -1, 1, 21, 0xffff, 0xffffffffffffL)
}

class AtomGen extends Generator[BasicAtom] {
  val kinds = Seq[Generator[BasicAtom]](
      new ApplyGen)
  val kinditer = kinds.iterator
  var iter = kinditer.next
  def findNext = {
    if (iter.hasNext) {
      _current = iter.next
    } else if (kinditer.hasNext) {
      iter = kinditer.next
      findNext
    }
    _current != null
  }
}

class ApplyGen extends Generator[BasicAtom] {
  val lhsiter = new AtomGen
  var rhsiter = new AtomGen
  var lhs = lhsiter.next
  def findNext = {
    if (!rhsiter.hasNext) {
      if (lhsiter.hasNext) {
        lhs = lhsiter.next
        rhsiter = new AtomGen
        findNext
      }
    } else {
      _current = Apply(lhs, rhsiter.next)
    } 
    _current != null
  }
}

class AtomListGen extends Generator[BasicAtom] {
  val sizes = Seq(0, 1, 2)
  def findNext = false
}