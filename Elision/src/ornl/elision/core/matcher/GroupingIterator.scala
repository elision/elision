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
import ornl.elision.core._

/**
 * Iterate over all associative groupings of items.
 * 
 * The number of patterns must be less than the number of subjects, or an
 * exception is thrown on construction.
 * 
 * If an operator is provided, it is applied to subgroupings, but not to the
 * top-level group (so that the correct number of atoms is returned for
 * sequence matching).
 * 
 * This iterator requires that the number of patterns be strictly less than
 * the number of subjects, and further that the number of patterns be at least
 * two.
 * 
 * @param patterns	The patterns.
 * @param subjects	The subjects.
 * @param op				The operator, if known.
 */
class GroupingIterator(patterns: AtomSeq, subjects: AtomSeq,
    op: Option[OperatorRef]) extends Iterator[OmitSeq[BasicAtom]] {
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
	private val _limit = _pats.length - 1
	/** This is the number of "slots" for markers. */
	private val _sublimit = _subs.length - 1
	/** The marker position array. */
	private val _markers = new Array[Int](_limit)
	/** The maximum value for each marker position. */
	private val _endpoint = new Array[Int](_limit)
	
	/** Initialize the marker and endpoint arrays. */
	for (i <- 0 until _limit) {
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
	private def advance {
	  var here = _limit-1
	  while(true) {
	    _markers(here) += 1
	    if (_markers(here) > _endpoint(here)) {
	      here -= 1
	      if (here < 0) {
	        _exhausted = true
	        return
	      }
	    } else {
	      for (i <- here+1 until _limit) _markers(i) = _markers(i-1) + 1
	      return
	    }
	  }
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
	    // M+1 slots.
	    val nextList = new scala.collection.mutable.ArraySeq[BasicAtom](_limit+1)
	    for (marker <- 0 to _limit) {
	      // Get the end of the slice.  The last time through the slice ends at
	      // the last subject.
	      val endSlice = if (marker == _limit) _sublimit else _markers(marker)
	      
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
	    }
	    
	    // Save the new current value.
	    _current = nextList
	    
	    // Advance the markers.
	    advance
	    
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
