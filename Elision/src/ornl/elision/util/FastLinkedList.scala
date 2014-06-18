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

import ornl.elision.core.BasicAtom

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
  /** A unique ID for this linked list. */
  private var listID = FastLinkedList.nextListID
  /** Approximate # of elements in the list. */
  private var len : Long = 0
  /** Track the list IDs to follow when there is branching. */
  private var idStack : java.util.LinkedList[Long] = null
  private var alreadyAppended : java.util.HashSet[Long] = new java.util.HashSet[Long]()
  // Can't append a list to itself.
  alreadyAppended.add(listID)

  /**
   * Add an item to the end of the list. This will create 1 new linked
   * list node object. This is an O(1) operation.
   *
   * @param item The item to append.
   */
  def append(item : A) {
    //println("** Appending '" + item+ "' to " + this)
    val nextNode = new FastLinkedListNode[A](item)
    nextNode.next.put(listID, null)
    if (tail != null) {
      tail.next.put(listID, nextNode)
    }
    tail = nextNode
    if (head == null) {
      nextNode.switchID = listID
      head = nextNode
      //println("** Switched head to " + head + " w. switch ID " + listID)
    }
    len += 1
    //println("** Result is " + this)
  }

  /**
   * Append a list to the end of the current list. This will create no
   * new nodes (the given list will be tacked directly onto the end of
   * the current list). Use this with caution as it is possible to
   * wind up with lists with cycles.  This is an O(1) operation.
   *
   * @param item The list to append. Note that a list can only be appended
   * to the current list once (subsequent appendings are ignored).
   */
  def appendAll(item : FastLinkedList[A]) {
    //println("** Appending '" + item + "' to " + this)
    if (item.head != null) {

      // Was other already appended to us? If so, we do not need
      // to change.
      if (alreadyAppended.contains(item.listID)) return
      alreadyAppended.addAll(item.alreadyAppended)

      // Were we already appended to the other list? If so, make
      // ourselves equal to the other list.
      if (item.alreadyAppended.contains(listID)) {
        head = item.head
        tail = item.tail
        listID = item.listID
        alreadyAppended = item.alreadyAppended
        return
      }

      // No appendings of us or the other list have been done. Append
      // the other list to this one.
      if (head == null) {
        head = item.head
        listID = item.listID
        //println("** Switched head to head of other list (" + head + " w. ID " + listID + ")")
      }
      if (tail != null) {
        tail.next.put(listID, item.head)
        //println("** Connected " + tail + " -> " + item.head + " w. ID " + listID)
      }
      tail = item.tail
      len += item.size
      //println("** Result = " + this)
    }
  }

  /**
   * Get the approximate # of elements in the list.
   */
  def size() : Long = {
    len
  }

  /**
   * Prepare the list for iteration via next(). This must always be
   * done prior to list iteration with next().
   */
  def reset() {
    currNode = head
    visitMark = FastLinkedList.nextVisitMark
    FastLinkedList.nextVisitMark = FastLinkedList.nextVisitMark + 1
    idStack = new java.util.LinkedList[Long]()
    idStack.add(listID)
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
      //println("** FastLinkedList: found loop")
      //println("** FastLinkedList: curr list = " + listID)
      //println("** FastLinkedList: next nodes = " + currNode.next)
      currNode = null;
      return null;
    }
    currNode.visitMark = visitMark

    //println("** next(): currNode = '" + currNode + "'")
    //println("** idStack = " + idStack)
    val r = currNode
    var i : java.util.Iterator[Long] = idStack.iterator()
    var transientListID = i.next()
    //println("** Next links = " + currNode.next)
    //println("** Trying list ID " + transientListID)
    while (!currNode.hasNext(transientListID)) {
      if (!i.hasNext()) {
        currNode = null
        return null
      }
      transientListID = i.next()
      //println("** Trying list ID " + transientListID)
    }
    currNode = currNode.getNext(transientListID)
    //println("** next node = '" + currNode + "'")
    if ((currNode != null) && (currNode.switchID > 0)) {
      idStack.add(currNode.switchID)
      //println("** Added list ID " + currNode.switchID + " to stack")
    }
    //println("** next() (" + visitMark + ") returning " + r)
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
  protected var _nextListID : Long = 1

  def nextListID() = {
    _nextListID += 1
    _nextListID
  }
}

/**
 * A single node in a FastLinkedList.
 *
 * @param d The data stored in the node.
 */
class FastLinkedListNode[A](d : A) {

  var next : java.util.HashMap[Long, FastLinkedListNode[A]] = 
    new java.util.HashMap[Long, FastLinkedListNode[A]]
  val data : A = d
  var visitMark : Long = 0
  var switchID : Long = -1

  /**
   * Test to see if the given list ID will yield a valid next node.
   *
   * @param id The ID of the list for which to check the next node.
   *
   * @return true if calling getNext() with the given ID will return a
   * next node, or false if there is no next node associated with the
   * given ID.
   */
  def hasNext(id : Long) : Boolean = {
    ((next.keySet().size() == 1) || next.containsKey(id))
  }

  /**
   * Get the next node in the given list.
   *
   * @param id The ID of the list for which to get the next node.
   *
   * @return The next node in the list.
   */
  def getNext(id : Long) : FastLinkedListNode[A] = {

    // If there is only 1 branch for the next node, return that.
    val keys = next.keySet()
    if (keys.size() == 1) return next.get(keys.iterator().next)

    // If the desired list ID is not a possible branch for the next node,
    // return null.
    if (!next.containsKey(id)) return null

    // Return the next list node for the desired list.
    return next.get(id)
  }

  /**
   * The list node as a string.
   *
   * @return The list node as a string.
   */
  override def toString() : String = {
    data match {
      case x : BasicAtom => return x.toParseString
      case _ => return data.toString
    }
  }
}
