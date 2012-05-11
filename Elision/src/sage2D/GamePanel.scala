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

import swing.FlowPanel
import java.awt.Graphics2D

/** 
 * A swing panel object for containing your application and rendering it. 
 * It also provides several helpful methods for managing logic flow, the timer, and painting.
 */

abstract class GamePanel extends FlowPanel with java.awt.event.ActionListener {
	
	/** The panel's timer object for timing the execution of logic iterations and repaint calls */
	val timer : GameTimer = new GameTimer(0,this)
	
	/** Changing this to >0 will cause multiple timerLoop iterations to be processed before repainting each time the timer fires. Good for fast-forwarding effects. */
	var skipRate : Int = 0
	
	/** A flag for telling the panel whether it needs to wait to load something and display the loading animation instead of its normal execution */
	var isLoading : Boolean = false
	
	/** 
	 * A flag for telling components outside the panel that it is currently busy handling a timerLoop iteration or a repaint. 
	 * This is meant to encourage programming with concurrency in mind, but it may be replaced with some other concurrency mechanism
	 * in a later SAGE 2D build.
	 */
	var timerLock : Boolean = false
	
	/** A flag to tell paint whether or not to call FlowPanel's paint to clear the panel's canvas before each frame.*/
	var repaintBackground = true
	
	/** 
	 * Event handler for the timer object. 
	 * It runs an iteration through the timerLoop method if it isn't currently loading something, and it tells the panel to repaint.
	 */
	
	def actionPerformed(event : java.awt.event.ActionEvent) : Unit = event.getSource() match {
		case gt : GameTimer => {
			timerLock = true
			
			if(!isLoading) {
				// perform 1 or more iteration through the timer loop, depending on the skip rate.
				val numLoops = math.max(skipRate + 1, 1)
				for(i <- 1 to numLoops) timerLoop	
			}
			repaint
			gt.updateFrameRateCounter
			
			timerLock = false
		}
		case _ =>
	}
	
	
	
	/** 
	 * Performs one iteration through the application's logic. 
	 * The user is expected to implement their own timerLoop for their application's needs.
	 */
	
	def timerLoop : Unit
	
	
	/**
	 * Gets called whenever the application needs to repaint. 
	 * If isLoading is true, it calls the loadingPaint method.
	 * Otherwise, it calls the mainPaint method.
	 * @param g		This panel's graphics context passed down by repaint.
	 */
	 
	override def paint(g : Graphics2D) : Unit = {
		if(repaintBackground)	super.paint(g)
		
		if(!isLoading) 	mainPaint(g)
		else 			loadingPaint(g)
	}
	
	
	/**
	 * The top-level rendering method for the application's normal (nonloading) execution.
	 * The user is expected to implement their own mainPaint method for their application's needs.
	 * @param g		This panel's graphics context passed down by repaint.
	 */
	
	def mainPaint(g : Graphics2D) : Unit
	
	
	/**
	 * The top-level rendering method for displaying some sort of loading animation while the 
	 * application is busy loading or waiting on something.
	 * The user is expected to implement their own loadingPaint method for their application's needs.
	 * @param g		This panel's graphics context passed down by repaint.
	 */
	
	def loadingPaint(g : Graphics2D) : Unit
	
	
	/** 
	 * Begins the application and starts the timer 
	 * @param fps	The desired frame rate for the application's timer. 60 by default.
	 */
	
	def start(fps : Int = 60) : Unit = {
		timer.setFPS(fps)
		timer.start
	}
	
}




