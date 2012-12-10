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

  
/**
 * An omit sequence permits rapid modification of a sequence by either
 * omitting a single element or inserting a sequence.  These operations are
 * performed "in place" to reduce the overhead of copying.  Still, an omit
 * sequence is immutable; these operations return a new sequence.
 * 
 * @author Stacy Prowell (prowellsj@ornl.gov)
 */
abstract class OmitSeq[A] extends IndexedSeq[A] with HasOtherHash {

  /**
   * Omit a single element from this list, returning a new list.  This is
   * done "in place" so it should be fast.  As omits mount, lookup time
   * can suffer, approaching linear time.
   * 
   * @param index The zero-based index to omit.
   * @return  The new list.
   */
  def omit(index: Int): IndexedSeq[A] = new _OmitSeq2(this, index)
  
  /**
   * Insert a sequence into the list starting at the given (zero-based) index.
   * This is done "in place" so it should be fast.  As inserts mount, lookup
   * time can suffer, as with omits.
   * 
   * @param index The zero-based index of the first inserted element.
   * @param items The items to insert.  If this is empty, nothing is done.
   * @return  The new list.
   */
  def insert(index: Int, items: IndexedSeq[A]): IndexedSeq[A] = {
    if (items.size == 0) this else new _OmitSeq3(this, index, items)
  }
}

/**
 * Provide convenient construction of an omit sequence and automatic
 * (implicit) transformation of indexed collections to an omit sequence.
 */
object OmitSeq {

  /**
   * Convert an indexed sequence to an omit sequence.
   * 
   * @param backing The backing sequence.
   * @return  The new omit sequence.
   */
  implicit def fromIndexedSeq[A](backing: IndexedSeq[A]): OmitSeq[A] =
    new _OmitSeq1[A](backing)
    
  /**
   * Make a new omit sequence that is initially empty.
   * 
   * @return An empty sequence.
   */
  def apply[A](): OmitSeq[A] = new _OmitSeq1(Seq[A]().toIndexedSeq)
  
  /**
   * Make a new omit sequence from the given items.
   * 
   * @param items The items of the new sequence.
   * @return  The new sequence.
   */
  def apply[A](items: A*): OmitSeq[A] = new _OmitSeq1[A](items.toIndexedSeq)

  /**
   * Directly create an omit sequence from the given backing sequence, omitting
   * the specified index.
   *
   * @param backing The backing sequence.
   * @param omit    The index to omit.
   * @return  The new omit sequence instance.
   */
  def apply[A](backing: IndexedSeq[A], omit: Int): OmitSeq[A] =
    new _OmitSeq2[A](backing, omit)
}

/**
 * Construct an omit sequence that simply wraps a backing sequence but does
 * not actually omit or insert anything.
 * 
 * @param backing The backing sequence.
 */
private class _OmitSeq1[A](backing: IndexedSeq[A]) extends OmitSeq[A] {
  // Proxy to backing sequence.
  lazy val length = backing.length
  
  /**
   * Hash code for an OmitSeq. It looks like the default hashCode
   * computation for an IndexedSeq uses toString (which is bad), so
   * hashCode has been overwritten.
   */
  override lazy val hashCode = backing.hashCode
  
  /**
   * Alternate hash code for an OmitSeq.
   */
  lazy val otherHashCode = backing.foldLeft(BigInt(0))(other_hashify)+1
    
  // Proxy to backing sequence.
  def apply(index: Int) = {
    backing(index)
  }
}

/**
 * Construct an omit sequence that omits a single element from the backing
 * sequence.
 * 
 * @param backing The backing sequence.
 * @param omit    The (zero-based) index of the item to omit.
 */
private class _OmitSeq2[A](backing: IndexedSeq[A], omit: Int)
extends OmitSeq[A] {
  /** Length is one less than the backing sequence. */
  lazy val length = backing.length - 1

  /**
   * Hash code for an OmitSeq. It looks like the default hashCode
   * computation for an IndexedSeq uses toString (which is bad), so
   * hashCode has been overwritten.
   */
  override lazy val hashCode = backing.hashCode
  
  /**
   * Alternate hash code for an OmitSeq.
   */
  lazy val otherHashCode = backing.foldLeft(BigInt(0))(other_hashify)+1
  
  /** Return the requested element by zero-based index. */
  override def apply(index: Int) =
    if (index >= omit) backing(index+1) else backing(index)
}

/**
 * Construct an omit sequence that inserts another sequence of elements at
 * a given index.
 * 
 * @param backing The backing sequence.
 * @param insert  The (zero-based) index of the items to insert.
 * @param items   The sequence of items to be inserted.
 */
private class _OmitSeq3[A](backing: IndexedSeq[A], insert: Int,
    items: IndexedSeq[A]) extends OmitSeq[A] {
  // Some stored constants for fast reference.
  private val _il = items.length
  private val _tip = _il + insert
  
  /** Length is the backing sequence plus the inserted items. */
  override lazy val length = backing.length + _il
  
  /**
   * Hash code for an OmitSeq. It looks like the default hashCode
   * computation for an IndexedSeq uses toString (which is bad), so
   * hashCode has been overwritten.
   */
  override lazy val hashCode = backing.hashCode
  
  /**
   * Alternate hash code for an OmitSeq.
   */
  lazy val otherHashCode = backing.foldLeft(BigInt(0))(other_hashify)+1
  
  /** Return the requested element by zero-based index. */
  override def apply(index: Int) =
    if (index < insert) backing(index)
    else {
      if (index < _tip) items(index - insert)
      else backing(index - _il)
    }
}
