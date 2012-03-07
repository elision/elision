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
package sjp.elision.util

import scala.collection.IndexedSeq

/**
 * A discard list is an immutable list that supports fast lookup by index and
 * single-element discard.
 * 
 * @param backing	The original list.
 */
class DiscardList[A](backing: IndexedSeq[A]) extends IndexedSeq[A] {
  lazy val length = backing.length
  def apply(index: Int) = backing(index)
	override def slice(from: Int, until: Int): DiscardList[A] =
	  new SliceList(this, from, until)
  
  /**
   * Discard a single element from this list, returning a new list.  This is
   * done "in place" so it should be fast.  As discards mount, lookup time
   * can suffer, approaching linear time.
   * 
   * @param index	The zero-based index to discard.
   * @return	The new list.
   */
	def discard(index: Int): DiscardList[A] = new OmitList(this, index)
}

private class OmitList[A](backing: IndexedSeq[A], omit: Int)
extends DiscardList[A](backing) {
	override lazy val length = backing.length - 1
	override def apply(index: Int) =
	  if (index >= omit) backing(index+1) else backing(index)
}

private class SliceList[A](backing: IndexedSeq[A], from: Int, until: Int)
extends DiscardList[A](backing) {
  override lazy val length = until - from
  override def apply(index: Int) = backing(from + index)
}
