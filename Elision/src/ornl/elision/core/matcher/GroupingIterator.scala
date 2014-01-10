/*======================================================================
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 * The Elision Term Rewriter
 * 
 * Copyright (c) 2012 by UT-Battelle, LLC.
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 * 1. Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 * 
 * Collection of administrative costs for redistribution of the source code or
 * binary form is allowed. However, collection of a royalty or other fee in excess
 * of good faith amount for cost recovery for such redistribution is prohibited.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER, THE DOE, OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
 * OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
 * OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 * ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
======================================================================*/
package ornl.elision.core.matcher

import ornl.elision.core.Apply
import ornl.elision.core.AtomSeq
import ornl.elision.core.BasicAtom
import ornl.elision.core.OperatorRef
import ornl.elision.util.OmitSeq
import ornl.elision.util.OmitSeq.fromIndexedSeq

/**
 * Iterate over all associative groupings of items.
 * 
 * == Caveats ==
 * The number of patterns must be less than the number of subjects, or an
 * exception is thrown on construction.  Note that if there are more patterns
 * than subjects, no match is possible.  If there are equal numbers of patterns
 * and subjects, use the [[ornl.elision.core.matcher.SequenceMatcher]] instead.
 * 
 * Additionally the number of patterns must be at least two.  If the number of
 * patterns is less than two, there is no need to perform any grouping.
 * 
 * == Operators ==
 * If an operator is provided, it is applied to subgroupings, but not to the
 * top-level group (so that the correct number of atoms is returned for
 * sequence matching).
 * 
 * For example, suppose we have three patterns and the subjects
 * `%(a,b,c,d,e)`.  One permissible grouping is `%(a,b),%(c,d),e`,
 * which contains the two subgroups `%(a,b)` and `%(c,d)`.  If no
 * operator is provided, these three groups are returned as-is.  If the
 * operator `f` is provided, then instead the three atoms
 * `f(a,b),f(c,d),e` are returned.
 * 
 * This is used when matching the argument list of an operator application, and
 * allows `add(x,y,z)` to become `add.%(add(x,y),x)`, for instance.
 * 
 * @param patterns	The patterns.
 * @param subjects	The subjects.
 * @param op				The operator, if known.
 */
class GroupingIterator(patterns: AtomSeq, subjects: AtomSeq,
    op: Option[OperatorRef]) extends Iterator[OmitSeq[BasicAtom]] {
  
  /* How this class works.
   * 
   * This class uses a vector of "markers" to iterate through the possible
   * groupings until they are all exhausted.
   * 
   * Imaging a bookshelf holding S books, one for each subject we are given.
   * We want to insert dividers between the books to break them into P groups,
   * where P is the number of patterns we are given.  We cannot rearrange the
   * books.
   * 
   * So we have to distribute (P-1) dividers (or "markers" here) into (S-1)
   * "slots."  For example, consider 5 subjects and three patterns.  We have
   * the following picture, where the vertical bars represent subjects and the
   * commas represent markers.
   * 
   * |,|,| | |
   * |,| |,| |
   * |,| | |,|
   * | |,|,| |
   * | |,| |,|
   * | | |,|,|
   * 
   * So there are six different ways to place the two markers into the four
   * slots: 4C2 = 4! / 2! / 2! = 24 / 2 / 2 = 6.
   * 
   * The illustration also shows how this class generates groupings.  At each
   * stage, the rightmost marker is advanced until it reaches the "end."  Then
   * the immediately prior marker is advanced, the rightmost marker is moved
   * just right of it, and the process begins again.
   * 
   * The number of markers is held in the `_markcount` constant, and the
   * number of slots in the `_slotcount` constant.  The actual marker
   * positions are held as zero-based indices in the `_marker` array.
   * To rapidly detect when an endpoint has been reached, the array
   * `_endpoint` holds the maximum index for each marker.
   * 
   * For the example above, these would be set as follows.
   * 
   * {{{
   * _markcount = 2
   * _slotcount = 4
   * _markers = Array(0,1)
   * _endpoint = Array(2,3)
   * }}}
   * 
   * During execution, the `_markers` array would have the following values.
   * 
   * {{{
   * Array(0,1)
   * Array(0,2)
   * Array(0,3)
   * Array(1,2)
   * Array(1,3)
   * Array(2,3)
   * }}}
   * 
   * The final value is equal to the `_endpoint` array, and the iterator
   * is exhausted.  All this is managed in one place: the `_advance` method.
   */
  
  /** Whether this iterator is exhausted. */
  private var _exhausted = false
  
  /** Patterns. */
  private val _pats = patterns.atoms
  
  /** Subjects. */
  private val _subs = subjects.atoms
  
  // Enforce the length constraint.
	require(_pats.length < _subs.length)
  require(_pats.length > 1)
  
  /** The operator, if known.  This avoids lots of gets. */
  val operator = op match {
    case None => null
    case _ => op.get
  }
  
  /** Current result, if any. */
  private var _current: IndexedSeq[BasicAtom] = null
	
	// Compute the initial and final marker vectors.
  
  /** This is the number of markers needed. */
	private val _markcount = _pats.length - 1
	/** This is the number of "slots" for markers. */
	private val _slotcount = _subs.length - 1
	/** The marker position array. */
	private val _markers = new Array[Int](_markcount)
	/** The maximum value for each marker position. */
	private val _endpoint = new Array[Int](_markcount)
	
	/** Initialize the marker and endpoint arrays. */
	for (i <- 0 until _markcount) {
	  _markers(i) = i
	  _endpoint(i) = _subs.length - _pats.length + i
	} // Initialize the arrays.
	
  /**
   * Advance the markers array once.  This updates the markers to the
   * next position, or sets the `_exhausted` flag if there are no more
   * positions.
   * 
   * Note that this method does not work when there is exactly one pattern.
   * Thus we have to deal with that case at the top level.
   */
	private def _advance {
	  // Get the index of the rightmost marker.  It becomes the "current"
	  // marker.
	  var here = _markcount-1
	  while(true) {
	    // Move the current marker to the right.
	    _markers(here) += 1
	    // If we reach the limit for the marker, then we need to shift to the
	    // marker to its left and begin to advance it.  Otherwise we correct
	    // any markers to the right of the current marker and return the next
	    // grouping.
	    if (_markers(here) > _endpoint(here)) {
	      here -= 1
	      // If we run out of markers to advance, we are done and the iterator
	      // is exhausted.
	      if (here < 0) {
	        _exhausted = true
	        return
	      }
	    } else {
	      // All markers to the right of the last marker we moved should be
	      // lined up just to its right, ready to start their movement.
	      for (i <- here+1 until _markcount) _markers(i) = _markers(i-1) + 1
	      return
	    }
	  } // Advance markers to the next grouping.
	}
	
	/**
	 * Use up the current value, and set it to null.  Whatever it was is
	 * returned.
	 * 
	 * @return	The prior current value.
	 */
	private def useCurrent = {
	  val tmp = _current
	  _current = null
	  tmp
	}
	
	/**
	 * Determine if this iterator has another grouping.
	 * 
	 * @return True if there is another grouping, and false if not.
	 */
	def hasNext = {
	  if (_exhausted) false
	  else if (_current != null) true
	  else {
	    // The easy cases are gone.  We have to generate the next grouping.
	    // The marker array already contains the recipe for generating the
	    // next grouping, so we use it now, and advance it at the end.
	    
	    // We need the endpoints of the slice.  The first slice starts at zero.
	    var startSlice = 0
	    
	    // This holds the list we are building.  If there are M markers, we need
	    // M+1 positions (for the M+1 groups).
	    val nextList = new scala.collection.mutable.ArraySeq[BasicAtom](_markcount+1)
	    for (marker <- 0 to _markcount) {
	      // Get the end of the slice.  The last time through the slice ends at
	      // the last subject.
	      val endSlice = if (marker == _markcount) _slotcount else _markers(marker)
	      
	      // Now get the slice.
	      val slice = _subs.slice(startSlice, endSlice+1)
	      
	      // Turn the slice into an atom list, except in the case of a single
	      // atom.
	      val item: BasicAtom =
	        (if (slice.length != 1) {
		        // Turn the slice into a list.
		      	val list = AtomSeq(subjects.props, slice)
		      
			      // If we have an operator, apply it now.
		      	if (operator != null) Apply(operator, list) else list
	        } else {
	        	slice(0)
	        })
	      
	      // Save the item.
	      nextList(marker) = item
	      
	      // Set the start of the next slice.
	      startSlice = endSlice+1
	    } // Collect all the slices.
	    
	    // Save the new current value.
	    _current = nextList
	    
	    // Advance the markers.
	    _advance
	    
	    // We have a value.
	    true
	  }
	}
	
	/**
	 * Get the next grouping.
	 * 
	 * @return	The next grouping.
	 */
	def next =
	  if (_current != null) useCurrent
	  else if (hasNext) useCurrent
	  else null
}
