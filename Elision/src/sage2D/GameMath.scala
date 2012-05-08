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

import util.Random



/** 
 * A group of commonly used mathmatical functions used for games and other sprite-driven applications
 */

object GameMath {
	import scala.math
	
	/** a global Random object for user convenience */
	var random : Random = new Random
	
	
	/** converts an angular value from degrees to radians. */
	
	def d2r(degrees : Double) : Double = degrees/360.0*2*math.Pi
	
	/** converts an angular value from radians to degrees. */
	
	def r2d(radians : Double) : Double = radians/(2.0*math.Pi)*360.0
	
	/** computes the sine of an angle given in degrees. */
	
	def sin(degrees : Double) : Double = {
		math.sin(d2r(degrees))
	}
	
	/** computes the cosine of an angle given in degrees. */
	
	def cos(degrees : Double) : Double = {
		math.cos(d2r(degrees))
	}
	
	
	/** Computes the angle, in degrees, of a ray from one point extending to another point. */
	
	def angleTo(x1 : Double, y1: Double, x2: Double, y2 : Double) : Double = {
		val dx : Double = {
			if (x2 == x1) 0.00001 
			else x2 - x1
		}
		val dy : Double = y1 - y2
		
		if (dx > 0)
			r2d(math.atan(dy/dx))
		else
			180 + r2d(math.atan(dy/dx))
	}
	
	/** Computes the angle, in degrees, of a ray from one point extending to another point. */
	
	def angleTo(p1 : java.awt.geom.Point2D, p2 : java.awt.geom.Point2D) : Double = {
		angleTo(p1.getX, p1.getY, p2.getX, p2.getY)
	}
	
	
	/** computes the distance between two points. */
	
	def dist(x1 : Double, y1 : Double, x2 : Double, y2 : Double) : Double = {
		math.sqrt(distSqr(x1,y1,x2,y2))
	}
	
	/** computes the distance between two points. */
	
	def dist(p1 : java.awt.geom.Point2D, p2 : java.awt.geom.Point2D) : Double = {
		dist(p1.getX, p1.getY, p2.getX, p2.getY)
	}
	
	/** computes the square distance between two points. This is faster than dist. */
	
	def distSqr(x1 : Double, y1 : Double, x2 : Double, y2 : Double) : Double = {
		val dx = x2-x1
		val dy = y2-y1
		dx*dx + dy*dy
	}
	
	/** computes the square distance between two points. This is faster than dist. */
	
	def distSqr(p1 : java.awt.geom.Point2D, p2 : java.awt.geom.Point2D) : Double = {
		distSqr(p1.getX, p1.getY, p2.getX, p2.getY)
	}
	

}


