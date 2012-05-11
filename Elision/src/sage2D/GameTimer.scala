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

import javax.swing.Timer


/** 
 * A Timer wrapper class with some functions that are convenient for a gaming context.
 *
 * @param delay		milliseconds for the initial and between-event delay
 * @param listener	an initial listener; can be null
 */

class GameTimer(delay : Int, listener : java.awt.event.ActionListener) extends Timer(delay,listener) {
	private var ticks : Int = 0
	private var startTime : Long = java.lang.System.currentTimeMillis
	
	/** Keeps track of the current average frame rate (in frames per second) for the application the timer is running for. */
	var fpsCounter : Double = 0.0		
	
	/**	
	 * Performs internal processing for keeping fpsCounter updated. 
	 * You should call this at the end of your application's timer event handler.
	 */
	
	def updateFrameRateCounter : Unit = {
		ticks += 1
		val currentTime = java.lang.System.currentTimeMillis
		
		if(currentTime - startTime >= 500) {
			fpsCounter = math.round(10*(1000.0 * ticks)/(currentTime - startTime))/10.0
			startTime = currentTime
			ticks = 0
		}
	}
	
	/**	
	 * Sets the GameTimer's delay to reflect a desired frame rate. 
	 * @param fps	The desired frame rate in frames per second.
	 */
	
	def setFPS(fps : Int) : Unit = {
		val millis : Int = (1000.0 / fps + 0.5).toInt
		setDelay(millis)
	}
	
	
}



