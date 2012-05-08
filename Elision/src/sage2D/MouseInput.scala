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

import swing.event._
import java.awt.geom.Point2D


/** 
 * An interface for polling mouse input in a Panel. 
 * @param listener		The Panel that will be using the mouse input
 */

class MouseInput(listener : swing.Panel) {
	
	/** The mouse's current position in the listening Panel relative to the Panel's topleft corner */
	var position : Point2D = new Point2D.Double(0,0)
	
	// flags for detecting any mouse presses
	
	var isAnyPressed = false
	var justAnyPressed = false
	var justAnyClicked = false
	
	private var pressedAnySinceLastFrame = false
	private var releasedAnySinceLastFrame = false
	
	
	// flags for left mouse presses

	var isLeftPressed = false
	var justLeftPressed = false
	var justLeftClicked = false
	
	private var pressedLeftSinceLastFrame = false
	private var releasedLeftSinceLastFrame = false
	
	
	// flags for right mouse presses
	
	var isRightPressed = false
	var justRightPressed = false
	var justRightClicked = false
	
	private var pressedRightSinceLastFrame = false
	private var releasedRightSinceLastFrame = false

	
	// flags for middle mouse presses
	
	var isMiddlePressed = false
	var justMiddlePressed = false
	var justMiddleClicked = false
	
	private var pressedMiddleSinceLastFrame = false
	private var releasedMiddleSinceLastFrame = false
	
	
	// flags for mouse wheel
	
	/** 
	 * Indicates which direction the mouse wheel was moved since the last frame. 
	 * 0 indicates no wheel movement. -1 means the wheel was moved up. 1 means the wheel was moved down.
	 */
	var wheel : Int = 0
	
	private var wheelDownSinceLastFrame = false
	private var wheelUpSinceLastFrame = false
	
	
	/** 
	 * Updates the state of the MouseInput object based on the input data it processed since the last frame. 
	 * Call this once (and only once) before processing any mouse input in your application's timerLoop method.
	 */
	
	def poll : Unit = {
		// Any button
		
		justAnyPressed = false
		justAnyClicked = false
		
		if(pressedAnySinceLastFrame) {
			justAnyPressed = true
			isAnyPressed = true
		}
		
		if(releasedAnySinceLastFrame) {
			justAnyClicked = true
			isAnyPressed = false
		}
		
		// Left button
		
		justLeftPressed = false
		justLeftClicked = false
		
		if(pressedLeftSinceLastFrame) {
			justLeftPressed = true
			isLeftPressed = true
		}
		
		if(releasedLeftSinceLastFrame) {
			justLeftClicked = true
			isLeftPressed = false
		}
		
		// Middle button
		
		justMiddlePressed = false
		justMiddleClicked = false
		
		if(pressedMiddleSinceLastFrame) {
			justMiddlePressed = true
			isMiddlePressed = true
		}
		
		if(releasedMiddleSinceLastFrame) {
			justMiddleClicked = true
			isMiddlePressed = false
		}
		
		// Right button
		
		justRightPressed = false
		justRightClicked = false
		
		if(pressedRightSinceLastFrame) {
			justRightPressed = true
			isRightPressed = true
		}
		
		if(releasedRightSinceLastFrame) {
			justRightClicked = true
			isRightPressed = false
		}
		
		// Wheel
		wheel = 0
		if(wheelDownSinceLastFrame) wheel = 1
		if(wheelUpSinceLastFrame) wheel = -1
		
		// reset all SinceLastFrame flags
		pressedAnySinceLastFrame = false
		releasedAnySinceLastFrame = false
		
		pressedLeftSinceLastFrame = false
		releasedLeftSinceLastFrame = false
		
		pressedMiddleSinceLastFrame = false
		releasedMiddleSinceLastFrame = false
		
		pressedRightSinceLastFrame = false
		releasedRightSinceLastFrame = false
		
		wheelDownSinceLastFrame = false
		wheelUpSinceLastFrame = false
	}
	
	
	
	// register the listening panel to listen to mouse events.
	
	listener.listenTo(listener.mouse.clicks)
	listener.listenTo(listener.mouse.moves)
	listener.listenTo(listener.mouse.wheel)
	
	// mouse event handling
	
	listener.reactions += {
		case e : MouseMoved =>
			position = new java.awt.geom.Point2D.Double(e.point.x,e.point.y)
		case e : MouseDragged => 
			position = new java.awt.geom.Point2D.Double(e.point.x,e.point.y)
		case e : MousePressed =>
			mousePress(e)
		case e : MouseReleased =>
			mouseRelease(e)
		case e : MouseWheelMoved =>
			if(e.rotation < 0) wheelUpSinceLastFrame = true
			if(e.rotation > 0) wheelDownSinceLastFrame = true
	}
	
	
	private def mousePress(e : MouseEvent) : Unit = {
		val button = e.peer.getButton
		pressedAnySinceLastFrame = true
		
		if(button == java.awt.event.MouseEvent.BUTTON1)
			pressedLeftSinceLastFrame = true
		if(button == java.awt.event.MouseEvent.BUTTON2)
			pressedMiddleSinceLastFrame = true
		if(button == java.awt.event.MouseEvent.BUTTON3)
			pressedRightSinceLastFrame = true
		
	}
	
	private def mouseRelease(e : MouseEvent) : Unit = {
		val button = e.peer.getButton
		releasedAnySinceLastFrame = true
		
		if(button == java.awt.event.MouseEvent.BUTTON1)
			releasedLeftSinceLastFrame = true
		if(button == java.awt.event.MouseEvent.BUTTON2)
			releasedMiddleSinceLastFrame = true
		if(button == java.awt.event.MouseEvent.BUTTON3)
			releasedRightSinceLastFrame = true
	}
}



