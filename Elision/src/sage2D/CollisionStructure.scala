/*======================================================================
 * 
 * SAGE 2D : Scala-Accelerated 2D Game Engine
 * 
 * Copyright (c) 2012 by Stephen Lindberg (sllindberg21@students.tntech.edu)
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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
======================================================================*/

package sage2D

import collection.mutable.ListBuffer

/**
 * An trait used for designing data structures for making collision detections
 * more efficient.
 */

trait CollisionStructure {
	/**
	 * Inserts a Sprite into the CollisionStructure.
	 * @param sprite	The Sprite being inserted into the CollisionStructure
	 * @return 			true if sprite was successfully inserted. Otherwise false.
	 */
	def insert(sprite : Sprite) : Boolean
	
	/**
	 * Returns a list of Sprites that might be colliding with a Sprite, as 
	 * as dictated by the CollisionStructure's logic.
	 * @param sprite	The Sprite for which we are obtaining a list of other
	 *					Sprites that it may be colliding with.
	 */
	def query(sprite : Sprite) : ListBuffer[Sprite]
}


import java.awt.geom.Point2D
	import java.awt.geom.Rectangle2D

/**
 * A Quadtree recursively divides an area into four quadrants and then inserts Sprites into 
 * these recursive quadrants in the lowest possible recursive quadrant that the Sprite's bounding 
 * box can be completely encapsuled in. A Sprite is added to the spriteList of each recursive quadrant
 * it visits, including the root, when it is inserted. A similar process is used to query a Sprite's 
 * location in the QuadTree.
 */
 
class QuadTree(val minX : Double, val minY : Double, val maxX : Double, val maxY : Double, val depth : Int = 0) extends CollisionStructure {
	
	
	/** Auxillary constructor using bounding points. */
	def this(upperLeft : Point2D, lowerRight : Point2D, depth : Int) = 
		this(upperLeft.getX, upperLeft.getY, lowerRight.getX, lowerRight.getY, depth)
	
	/** 
	 * The Quadtree's quadrant nodes. If a quadrant is currently empty, 
	 * it will be null. In increasing order, the indices correspond to the 
	 * following quadrants: upper-left, upper-right, lower-left, lower-right
	 */
	var quads : Array[QuadTree] = new Array[QuadTree](4)
	
	/** A list of the quadrants' upper-left points. */
	var min : Array[Point2D.Double] = new Array[Point2D.Double](4)
	
	/** A list of the quadrants' lower-right points. */
	var max : Array[Point2D.Double] = new Array[Point2D.Double](4)
	
	/** The list of Sprites currently contained in this quadrant */
	var spriteList : ListBuffer[Sprite] = new ListBuffer[Sprite]
	
	// Compute the values for the quadrants' min and max arrays.
	private val midX = minX + (maxX - minX)/2
	private val midY = minY + (maxY - minY)/2
	
	min(0) = new Point2D.Double(minX,minY)
	min(1) = new Point2D.Double(midX,minY)
	min(2) = new Point2D.Double(minX,midY)
	min(3) = new Point2D.Double(midX,midY)
	
	max(0) = new Point2D.Double(midX,midY)
	max(1) = new Point2D.Double(maxX,midY)
	max(2) = new Point2D.Double(midX,maxY)
	max(3) = new Point2D.Double(maxX,maxY)
	
	/** checks if a Sprite's bounding box intersects with this quadrant. */
	protected def intersects(bbox : Rectangle2D) : Boolean = 
		!(bbox.getX > maxX || bbox.getY > maxY || bbox.getX + bbox.getWidth < minX || bbox.getY + bbox.getHeight < minY)
	
	/** checks if a Sprite's bounding box is completely encapsuled by this quadrant. */
	protected def contains(bbox : Rectangle2D) : Boolean = 
		(bbox.getX >= minX && bbox.getY >= minY && bbox.getX + bbox.getWidth <= maxX && bbox.getY + bbox.getHeight <= maxY)
	
	/** checks if a Sprite's bounding box intersects with a quadrant. */
	protected def quadIntersects(bbox : Rectangle2D, i : Int) : Boolean = 
		!(bbox.getX > max(i).x || bbox.getY > max(i).y || bbox.getX + bbox.getWidth < min(i).x || bbox.getY + bbox.getHeight < min(i).y)
	
	/** checks if a Sprite's bounding box is completely encapsuled by a quadrant. */
	protected def quadContains(bbox : Rectangle2D, i : Int) : Boolean = 
		(bbox.getX >= min(i).x && bbox.getY >= min(i).y && bbox.getX + bbox.getWidth <= max(i).x && bbox.getY + bbox.getHeight <= max(i).y)
	
	
	
	
	
	// insert(Sprite) : Boolean inherited from CollisionStructure trait
	
	def insert(s : Sprite) : Boolean = {
		// We won't insert a Sprite that has been marked as destroyed.
		if(s.isDestroyed)	return false
		
		// obtain the Sprite's bounding box
		val bbox : Rectangle2D = s.getCollisionBox
		
		// Don't insert the Sprite if it's completely outside the the QuadTree's area
		// and the dontInsertOutsideSprites flag is true.
		if(QuadTree.dontInsertOutsideSprites && !this.intersects(bbox)) return false
		
		// At this point, the Sprite will be recursively inserted somewhere in the QuadTree.
		insertRec(s,bbox)
		true
	}
	
	/**
	 * Recursively inserts the Sprite in the tree until it reaches the deepest quadrant
	 * that can completely encapsulate it. The Sprite is inserted into that quadrant's 
	 * list.
	 * @param s		The Sprite being inserted into the QuadTree.
	 * @param bbox	s's bounding box.
	 */
	
	protected def insertRec(s : Sprite, bbox : Rectangle2D) : Unit = {
		// end the recursion once we reach the maximum depth value. Just insert the Sprite in the 
		// current quadrant.
		if(depth == QuadTree.maxDepth) {
			spriteList += s
			return
		}
		
		// check each quadrant to see if our Sprite overlaps it. If so, insert it in that quadrant too.
		
		for(i <- 0 to 3) {
			
			if(quadContains(bbox,i)) {
				// create a new subtree for the quadrant if necessary.
				if(quads(i) == null) quads(i) = new QuadTree(min(i),max(i),depth+1)
				
				// recursive insert
				quads(i).insertRec(s,bbox)
				return
			}
			
		}
		
		// The Sprite is added into the current quadrant's list if it can't be inserted into a deeper quadrant.
		spriteList += s
	}
	
	def query(s : Sprite) : ListBuffer[Sprite] = {
		// end the query if the Sprite is marked as destroyed.
		if(s.isDestroyed)	return null
		
		// start our empty list
		val resultList = new ListBuffer[Sprite]
		
		// obtain the Sprite's bounding box
		val bbox = s.getCollisionBox
		
		// recursively accumulate the list of possibly colliding Sprites and return it.
		recQuery(bbox, resultList)
		resultList
	}
	
	protected def recQuery(bbox : Rectangle2D, resultList : ListBuffer[Sprite]) : Unit = {
		// append all of this quadrant's Sprites to our current result list.
		resultList.appendAll(spriteList)
		
		// Recursively append the lists of quadrants this Sprite's bounding box 
		// intersects with.
		for(i <- 0 to 3) {
			if(quads(i) != null && quads(i).intersects(bbox)) 
				quads(i).recQuery(bbox,resultList)
		}
	}
}

object QuadTree {
	var maxDepth = 10
	var dontInsertOutsideSprites = true
}




