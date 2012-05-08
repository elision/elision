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

import java.awt.geom.AffineTransform
import java.awt.Graphics2D
import java.awt.Color
import java.awt.geom.Rectangle2D

/** 
 * An abstract class used to represent some sort of renderable object.
 * @param x		the Sprite's x-coordinate
 * @param y		the Sprite's y-coordinate
 */

abstract class Sprite(var x : Double = 0, var y : Double = 0) {
	
	/** controls the central X coordinate of the Sprite relative to its image */
	var focalX : Double = 0 
	
	/** controls the central Y coordinate of the Sprite relative to its image */
	var focalY : Double = 0
	
	/** controls the scale transform of the Sprite on its x-axis */
	var scaleX : Double = 1.0
	
	/** controls the scale transform of the Sprite on its y-axis */
	var scaleY : Double = 1.0
	
	/** controls the overall scale transform of the sprite on its x and y-axises */
	var scaleMaster : Double = 1.0
	
	/** controls the Sprite's rotational transform relative to its parent context */
	var angle : Double = 0.0
	
	/** used to store the Sprite's current rotate-scale transform */
	private var transform : AffineTransform = new AffineTransform
	
	/** 
	 * This becomes true whenever any of the methods for manipulating 
	 * this Sprite's rotation or scale are called. It lets the render method know to update 
	 * its rotate-scale transform before drawing.
	 */
	private var transNeedsUpdate : Boolean = true	
	
	/** controls the alpha values for the Sprite's pixels */
	var alphaMaster : Double = 1.0
	
	/** a flag to let everyone know that this Sprite is destroyed and scheduled to be discarded. */
	var isDestroyed : Boolean = false
	
	/** controls whether this Sprite should be rendered. */
	var isVisible : Boolean = true
	
	
	/**	
	 * Marks this Sprite as destroyed. Override this method if you need to perform additional 
	 * processing when destroying the Sprite.
	 */
	
	def destroy : Unit = {
		isDestroyed = true
	}
	
	/**
	 * Updates the transforms for drawing the Sprite if needed and then calls the Sprite's draw method.
	 * @param g		The graphics context this is being rendered on.
	 */
	
	def render(g : Graphics2D) : Unit = {
		if(isDestroyed || !isVisible || alphaMaster <= 0.0)
			return
		if(transNeedsUpdate)
			updateTransform
		transNeedsUpdate = false
		
		val origTrans = g.getTransform
		var curTrans = g.getTransform
		
		curTrans.translate(x,y)
		curTrans.concatenate(transform)
		curTrans.translate(0-focalX,0-focalY)
		g.setTransform(curTrans)
		
		this.draw(g)
		
		g.setTransform(origTrans)
		
	}

	/**	
	 * Called by the render method to draw the image for the Sprite. 
	 * @param g		The graphics context this is being rendered on.
	 **/
	
	def draw(g : Graphics2D) : Unit
	
	/**	
	 * sets the alpha value for the Sprite's translucency
	 * @param a		Some value in the range [0.0, 1.0] which we would like to set the sprite's master alpha color to.
	 *				0.0 makes the Sprite completely transparent and 1.0 makes the Sprite use its normal alpha values for each of its pixels.
	 */
	
	def setAlpha(a : Double) : Unit = {
		// enforce the alpha value to be in the range [0.0, 1.0]
		alphaMaster = math.max(0.0, math.min(1.0,a))
	}
	
	
	
	
	/////////// Rotation/Scale transform methods
	
	
	/**	
	 * Changes the values for the Sprite's scale transform and turns on the flag to let render knows it needs to update the Sprite's transform.
	 * @param master	Some value to change scaleMaster to.
	 * @param sx		Some value to change scaleX to.
	 * @param sy		Some value to change scaleY to.
	 */
	
	def scale(master : Double, sx : Double, sy : Double) : Unit = {
		scaleMaster = master
		scaleX = sx
		scaleY = sy
		transNeedsUpdate = true
	}
	
	/**	
	 * Changes the values for the Sprite's scale transform and turns on the flag to let render knows it needs to update the Sprite's transform. 
	 * This method only modifies the master scale without modifying the x and y scales.
	 * @param master	Some value to change scaleMaster to.
	 */
	
	def scale(master : Double) : Unit = {
		scaleMaster = master
		transNeedsUpdate = true
	}
	
	/**
	 * Changes the Sprite's angle for its Rotation transform and turns on the flag to let render knows it needs to update the Sprite's transform. 
	 * Note: degrees in the range (0,180) will face in the direction of the negative y axis.
	 * @param degrees	Some value to change angle to.
	 */
	
	def rotate(degrees : Double) : Unit = {
		angle = degrees
		transNeedsUpdate = true
	}
	
	/**
	 * Updates the rotation-scale transform for the Sprite based on its current state.
	 * This is called by render(g) if the transform state has been changed, but it is here
	 * also for the user's convenience.
	 */
	
	def updateTransform : Unit = {
		transform = new AffineTransform
		transform.rotate(0-GameMath.d2r(angle))
		transform.scale(scaleX*scaleMaster, scaleY*scaleMaster)
	}
	
	
	/////////////	Collision methods
	
	/**
	 * Returns the bounding box of this sprite for the purpose of collision detection.
	 * A default definition is provided here for the convenience of writing Sprites where collisions are not needed.
	 * If you plan to do any collision detection with your Sprite, you should override this method.
	 */
	
	def getCollisionBox : Rectangle2D = new Rectangle2D.Double(x,y,1,1)
	
	
}



