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
import collection.mutable.HashMap

/**
 * An interface for polling keyboard input in a Panel. 
 * @param listener		The Panel that will be using the keyboard input
 */

class KeyboardInput(listener : swing.Panel) {

	// flags for detecting any key presses
	
	/** True if any key was pressed or has remained pressed since the last frame */
	var isAnyPressed = false
	
	/** True if the user just began pressing any key since the last frame */
	var justAnyPressed = false
	
	/** Like justAnyPressed, but it repeats firing true after any key has been held for at least a few seconds. */
	var justAnyPressedRep = false
	
	/** True if the user just released any key */
	var justAnyTyped = false
	
	private var pressedAnySinceLastFrame = false
	private var releasedAnySinceLastFrame = false
	
	
	
	// key,flag hashes for detecting key presses
	
	/** 
	 * Maintains a record of what keys have been pressed since the last frame. 
	 * The map uses Scala key code enumeration values (scala.swing.event.key) as keys. 
	 * A key is true if it was pressed or has remained pressed since the last frame.
	 */
	val isPressed = new HashMap[Key.Value, Boolean]
	
	/** 
	 * Maintains a record of what keys have just been pressed since the last frame. 
	 * The map uses Scala key code enumeration values (scala.swing.event.key) as keys. 
	 * A key is true if the user just began pressing that key since the last frame.
	 */
	val justPressed = new HashMap[Key.Value, Boolean]
	
	/** 
	 * Maintains a record of what keys have just been pressed since the last frame. 
	 * The map uses Scala key code enumeration values (scala.swing.event.key) as keys. 
	 * Like justPressed, but it repeats firing true after the key has been held for at least a few seconds
	 */
	val justPressedRep = new HashMap[Key.Value, Boolean]
	
	/** 
	 * Maintains a record of what keys have just been pressed since the last frame. 
	 * The map uses Scala key code enumeration values (scala.swing.event.key) as keys. 
	 * A key is true if the user just released that key
	 */
	val justTyped = new HashMap[Key.Value, Boolean]
	
	private var pressedSinceLastFrame = new HashMap[Key.Value, Boolean]
	private var releasedSinceLastFrame = new HashMap[Key.Value, Boolean]
	
	// initialize the hash maps
	
	for(key <- Key.values) {
		isPressed.put(key, false)
		justPressed.put(key, false)
		justPressedRep.put(key, false)
		justTyped.put(key, false)
		pressedSinceLastFrame.put(key, false)
		releasedSinceLastFrame.put(key, false)
	}
	
	// last pressed
	
	var lastKeyPressed : Key.Value = null
	
	/** 
	 * Updates the state of the KeyboardInput object based on the input data it processed since the last frame. 
	 * Call this once (and only once) before processing any leyboard input in your application's timerLoop method.
	 */
	
	def poll : Unit = {
		// Any key
		
		justAnyPressed = false
		justAnyPressedRep = false
		justAnyTyped = false
		
		if(pressedAnySinceLastFrame) {
			justAnyPressedRep = true
			if(!isAnyPressed) justAnyPressed = true
			isAnyPressed = true
		}
		
		if(releasedAnySinceLastFrame) {
			justAnyTyped = true
			isAnyPressed = false
		}
		
		pressedAnySinceLastFrame = false
		releasedAnySinceLastFrame = false
		
		// Process all individual keys previously used
		
		for(key <- pressedSinceLastFrame.keys) {
			justPressed.put(key, false)
			justPressedRep.put(key, false)
			justTyped.put(key, false)
			
			if(pressedSinceLastFrame(key)) {
				justPressedRep.put(key, true)
				if(!isPressed(key)) justPressed.put(key, true)
				isPressed.put(key, true)
				
				lastKeyPressed = key
			}
			
			if(releasedSinceLastFrame(key)) {
				justTyped.put(key, true)
				isPressed.put(key, false)
			}
			
			pressedSinceLastFrame.put(key, false)
			releasedSinceLastFrame.put(key, false)
		}
		
	}
	
	
	// register the listening panel to listen to mouse events.
	
	listener.listenTo(listener.keys)
	
	
	// mouse event handling
	
	listener.reactions += {
		case e : KeyPressed =>
			keyPress(e)
		case e : KeyReleased =>
			keyRelease(e)
	}
	
	private def keyPress(e : KeyPressed) : Unit = {
		pressedAnySinceLastFrame = true
		
		pressedSinceLastFrame += ((e.key, true))
	}
	
	private def keyRelease(e : KeyReleased) : Unit = {
		releasedAnySinceLastFrame = true
		
		releasedSinceLastFrame += ((e.key, true))
	}
}



