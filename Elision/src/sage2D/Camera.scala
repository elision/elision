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
import java.awt.geom.Point2D


/** 
 * Used to create a 2D Camera affine transform for use with graphical projects. 
 * The camera's state stores information for the central focus position for the Camera, 
 * its rotation angle, and its zoom level.
 *
 * @param x		The x-position of the camera's focal center in world coordinates
 * @param y		The y-position of the camera's focal center in world coordinates
 * @param pWidth	The width of the parent screen (a Panel or some similar Component) for the camera.
 * @param pHeight	The height of the parent screen (a Panel or some similar Component) for the camera.
 */

class Camera(var x : Double = 0, var y : Double = 0, var pWidth : Double, var pHeight : Double) {
	
	/** the camera's angle is stored as degrees. */
	var angle : Double = 0;

	/** Controls the camera's zoom. 1.0 is the standard zoom level. < 1.0 is zoomed out. > 1.0 is zoomed in */
	var zoom : Double = 1.0;	
	
	/** The current x-position of the camera's focal center in screen coordinates. By default, this is set to the center of the parent panel. */
	var cx : Double = pWidth/2
	
	/** The current y-position of the camera's focal center in screen coordinates. By default, this is set to the center of the parent panel. */
	var cy : Double = pHeight/2
	
	private var transform : AffineTransform = new AffineTransform
	private var inverse : AffineTransform = new AffineTransform
	
	private var dragStartPos : Point2D = new Point2D.Double(x,y)
	private var dragAnchor : Point2D = new Point2D.Double(x,y)
	private var dragInverse : AffineTransform = new AffineTransform
	
	/** Resets the camera to its original state. */
	def reset : Unit = {
		x = 0
		y = 0
		angle = 0
		cx = pWidth/2
		cy = pHeight/2
		zoom = 1.0
		
		updateTransform
	}	
	
	/**	
	 * Updates the Camera's transform based on its current state values. 
	 * Call this after you make any changes to the camera's state variables  
	 * sometime before rendering.
	 */
	
	def updateTransform : Unit = {
		transform = new AffineTransform
		transform.translate(cx,cy)
		transform.scale(zoom,zoom)
		transform.rotate(GameMath.d2r(angle))
		transform.translate(0-x, 0-y)
		
		inverse = transform.createInverse
	}
	
	/**
	 * Returns a copy of the Camera's current transform.
	 */
	
	def getTransform : AffineTransform = new AffineTransform(transform)
	
	/**	
	 * Returns a copy of the Camera's current inverse transform.
	 */
	
	def getInverse : AffineTransform = new AffineTransform(inverse)
	
	/**	
	 * Converts a point in screen coordinates to its equivalent in the Camera's world coordinates.
	 * @param p		A point in screen coordinates
	 * @return		p transformed to world coordinates
	 */
	
	def screenToWorldCoords(p : Point2D) : Point2D = {
		inverse.transform(p,null)
	}
	
	/**	
	 * Converts a point in world coordinates to its equivalent in the Camera's screen coordinates.
	 * @param p		A point in world coordinates
	 * @return		p transformed to screen coordinates
	 */
	
	def worldToScreenCoords(p : Point2D) : Point2D = {
		transform.transform(p,null)
	}
	
	/**	
	 * moves the camera's focal center to screenPos without altering the current camera transform.
	 * @param screenPos	The point in screen coordinates where we are moving the camera's focal center to.
	 */
	
	def moveCenter(screenPos : Point2D) : Unit = {
		val dcx = screenPos.getX - cx
		val dcy = screenPos.getY - cy
		cx = screenPos.getX
		cy = screenPos.getY
		
		val dc = new Point2D.Double(dcx,dcy)
		val sr = new AffineTransform
		sr.scale(zoom,zoom)
		sr.rotate(GameMath.d2r(angle))
		val srInv = sr.createInverse
		val dc2 = srInv.transform(dc, null)
		
		this.x += dc2.getX
		this.y += dc2.getY
		
		updateTransform
	}
	
	/**
	 * Call this when you are initiating mouse-controlled drag and drop on the camera.
	 * @param mousePos	the mouse's screen coordinates.
	 */
	
	def startDrag(mousePos : Point2D) : Unit = {
		dragAnchor = screenToWorldCoords(mousePos)
		dragStartPos = new Point2D.Double(x,y)
		dragInverse = new AffineTransform(inverse)
	}
	
	/**
	 * This drags the camera with the mouse. Call startDrag before this when the user just begins dragging the mouse.
	 * @param mousePos	the mouse's screen coordinates.
	 */
	
	def drag(mousePos : Point2D) : Unit = {
		val mwp : Point2D = dragInverse.transform(mousePos,null) // obtain the mouse's current world coordinates
		val vector : Point2D = new Point2D.Double(dragAnchor.getX - mwp.getX, dragAnchor.getY - mwp.getY)

		this.x = dragStartPos.getX + vector.getX
		this.y = dragStartPos.getY + vector.getY
		
		moveCenter(mousePos)
	}
	
	/**
	 * Zooms the Camera toward or away from some point in screen coordinates.
	 * @param zoomAmt	The amount to multiply the current zoom by. 1.0 will cause the zoom to stay the same. 
	 * 		< 1.0 will cause the camera to zoom out. > 1.0 will cause the camera to zoom in.
	 * @param screenPos		The point in screen coordinates from/toward which we are zooming the camera.
	 */
	
	def zoomAtScreen(zoomAmt : Double, screenPos : Point2D) : Unit = {
		moveCenter(screenPos)
		zoom *= zoomAmt
		
		updateTransform
	}
	
	/**
	 * Zooms the Camera toward or away from some point in world coordinates.
	 * @param zoomAmt	The amount to multiply the current zoom by. 1.0 will cause the zoom to stay the same. 
	 * 		< 1.0 will cause the camera to zoom out. > 1.0 will cause the camera to zoom in.
	 * @param p		The point in world coordinates from/toward which we are zooming the camera.
	 */
	
	def zoomAt(zoomAmt : Double, p : Point2D) : Unit = {
		val tp = transform.transform(p,null)
		zoomAtScreen(zoomAmt, tp)
	}
	
	/**
	 * Zooms the Camera toward or away from some point in world coordinates.
	 * @param zoomAmt	The amount to multiply the current zoom by. 1.0 will cause the zoom to stay the same. 
	 * 		< 1.0 will cause the camera to zoom out. > 1.0 will cause the camera to zoom in.
	 * @param xx		The x-coordinate of the point in world coordinates from/toward which we are zooming the camera.
	 * @param yy		The y-coordinate of the point in world coordinates from/toward which we are zooming the camera.
	 */
	
	def zoomAt(zoomAmt : Double, xx : Double, yy : Double) : Unit = {
		zoomAt(zoomAmt, new Point2D.Double(xx,yy))
	}
	
	
	// create our initial Camera transform from the constructor values.
	
	updateTransform
}










