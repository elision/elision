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
package ornl.elision.util

import scala.collection.mutable.HashSet

/**
 * A fast (but risky) linked list implementation. Note that for speed
 * objects of this class are VERY mutable. Use with care.
 */
class FastLinkedList[A] extends Mutable {

  /** Pointer to the list head. */
  private var head : FastLinkedListNode[A] = null
  /** Pointer to the list tail. */
  private var tail : FastLinkedListNode[A] = null
  /** The node to be returned by next(). */
  private var currNode : FastLinkedListNode[A] = null
  /**
   * The current marking used to prevent infinite loops when iterating
   * over the list via next().
   */
  private var visitMark : Long = 0

  /**
   * Add an item to the end of the list. This will create 1 new linked
   * list node object. This is an O(1) operation.
   *
   * @param item The item to append.
   */
  def append(item : A) {
    val nextNode = new FastLinkedListNode[A](item)
    if (tail != null) {
      tail.next = nextNode
    }
    tail = nextNode
    if (head == null) head = nextNode
  }

  /**
   * Append a list to the end of the current list. This will create no
   * new nodes (the given list will be tacked directly onto the end of
   * the current list). Use this with caution as it is possible to
   * wind up with lists with cycles.  This is an O(1) operation.
   *
   * @param item The list to append.
   */
  def append(item : FastLinkedList[A]) {
    if (item.head != null) {
      if (tail != null) tail.next = item.head
      tail = item.tail
      if (head == null) head = item.head
    }
  }

  /**
   * Prepare the list for iteration via next(). This must always be
   * done prior to list iteration with next().
   */
  def reset() {
    currNode = head
    visitMark = FastLinkedList.nextVisitMark
    FastLinkedList.nextVisitMark = FastLinkedList.nextVisitMark + 1
  }

  /**
   * Get the next node in the list. Note that cycles in the list will
   * not be iterated over.
   *
   * @return If there is a new next node, return the node. If not,
   * return null.
   */
  def next() : FastLinkedListNode[A] = {

    if (currNode == null) return null

    // No loops.
    if (currNode.visitMark == visitMark) {
      currNode = null;
      return null;
    }
    currNode.visitMark = visitMark

    val r = currNode
    currNode = currNode.next
    return r
  }

  /**
   * The list as a string.
   *
   * @return The list as a string.
   */
  override def toString() : String = {
    reset()
    var node = next()
    var r = "["
    var first = true
    while (node != null) {
      if (!first) r += ", "
      first = false
      r += node
      node = next()
    }
    r += "]"
    return r
  }
}

/**
 * Class data for FastLinkedList objects.
 */
object FastLinkedList {
  protected var nextVisitMark : Long = 1
}

/**
 * A single node in a FastLinkedList.
 *
 * @param d The data stored in the node.
 */
class FastLinkedListNode[A](d : A) {
  var next : FastLinkedListNode[A] = null
  val data : A = d
  var visitMark : Long = 0

  /**
   * The list node as a string.
   *
   * @return The list node as a string.
   */
  override def toString() : String = {
    return data.toString
  }
}
