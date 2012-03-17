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
package sjp.elision.core.matcher
import sjp.elision.core._

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
 * @param patterns	The patterns.
 * @param subjects	The subjects.
 * @param op				The operator, if known.
 */
class GroupingIterator(patterns: AtomList, subjects: AtomList,
    op: Option[Operator]) extends Iterator[OmitSeq[BasicAtom]] {
  /** Whether this iterator is exhausted. */
  private var _exhausted = false
  
  /** Patterns. */
  private val _pats = patterns.atoms
  
  /** Subjects. */
  private val _subs = subjects.atoms
  
  // Enforce the length constraint.
	require(_pats.length < _subs.length)
  
  /** The operator, if known.  This avoids lots of gets. */
  val operator = op match {
    case None => null
    case _ => op.get
  }
  
  /** Current result, if any. */
  private var _current: IndexedSeq[BasicAtom] = null
	
	// Compute the initial and final marker vectors.
	private val _limit = _pats.length - 1
	private val _sublimit = _subs.length - 1
	private val _markers = new Array[Int](_limit)
	private val _endpoint = new Array[Int](_limit)
	for (i <- 0 until _limit) {
	  _markers(i) = i
	  _endpoint(i) = _subs.length - _pats.length + i
	} // Initialize the arrays.
	
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
		      	val list = AtomList(slice, subjects.props)
		      
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

//object GroupingIterator extends App {
//          
//  // Define add as a native operator.
//  val _context = new Context
//	_context.operatorLibrary.add(NativeOperatorDefinition(
//	    Proto("add", INTEGER, ("x", INTEGER), ("y", INTEGER)),
//	    Props(Associative, Commutative, Identity(0))))
//	_context.operatorLibrary.register("add",
//    (op: Operator, args: AtomList, _) => {
//		  // Accumulate the integer literals found.
//		  var lits:BigInt = 0
//		  // Accumulate other atoms found.
//		  var other = IndexedSeq[BasicAtom]()
//		  // Traverse the list and divide the atoms.
//		  args.atoms.foreach {
//		    x => x match {
//		      case IntegerLiteral(_, value) => lits += value
//		      case _ => other :+= x
//		    }
//		  }
//		  // Now add the accumulated literals to the list.
//		  other :+= Literal(INTEGER, lits)
//		  // Construct and return a new operator application.
//		  Apply(op, AtomList(other), true)
//    })
//  val op = _context.operatorLibrary("add")
//
//  val pats = AtomList(Vector("a","b","c").map(Variable(INTEGER, _)), None)
//  val subs = AtomList((1 to 7).map(IntegerLiteral(INTEGER, _)), None)
//  val gi = new GroupingIterator(pats, subs, Some(op))
////  while (!gi._exhausted) {
////    println(gi._markers.mkString(", "))
////    gi.advance
////  }
//  while (gi.hasNext) {
//    val ba = gi.next
//    print(ba.mkParseString("add(",", ", ") = "))
//    println(Apply(op, AtomList(ba, None)).toParseString)
//  }
//}
