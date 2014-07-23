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

package ornl.elision.gui

import java.awt._
import java.awt.datatransfer.DataFlavor
import java.io.File
import java.util.List
import javax.swing.TransferHandler
import sage2D._


class EvaVisPanel extends GamePanel {
    background = new Color(0xffffff)
	preferredSize = new Dimension(640,480)
    
    /** A variable used only by the loading screen animation. It's not important. */
	var loadingThingAngle = 0
    
    /** Painting just delegates to the current level's rendering. */
    def mainPaint(g : Graphics2D) : Unit = {
        if(curLevel != null)
            curLevel.render(g)
    }
	
    /** Loading animation. */
	def loadingPaint(g : Graphics2D) : Unit = {
        val centerX = this.size.width/2
		val centerY = this.size.height/2
		val radius = centerX / 4
		
		for(i <- 0 to 360 if i % 60 == 0) {
			val alpha : Float = i/360.0f
			val red : Float = alpha*0.8f + (1-alpha)*0.3f
			val green : Float = alpha*0.8f + (1-alpha)*0.0f
			val blue = 1.0f
			
			import java.awt.geom.Ellipse2D
			val circle = new Ellipse2D.Double(centerX + radius*GameMath.cos(i + loadingThingAngle) - 20, centerY + radius*GameMath.sin(i + loadingThingAngle) - 20, 40, 40)
			g.setColor(new Color(red,green,blue))
			g.fill(circle)
		}
		
		g.setColor(new Color(0x000000))
		if(isLoading)
			g.drawString("Loading...", centerX-50, centerY)
		
		loadingThingAngle += 5
    }
    
    
    /** Each "Level" represents one of Eva's modes. */
    override def makeLevelInstance(levelName : String) : Level = {
        if(levelName == "EliTreeVis")
            new elision.EliTreeVisLevel(this)
        else // default takes the user to the Welcome screen.
            new welcome.WelcomePanel(this)
    }
    
    
    /** Handler for drag and drop */
    val transferHandler = new TransferHandler {
        override def canImport(info : TransferHandler.TransferSupport) : Boolean = {
            true
        }
        
        
        override def importData(info : TransferHandler.TransferSupport) : Boolean = {
            if(!canImport(info))
                return false
                
            val t = info.getTransferable
            val droppedObj = t.getTransferData(DataFlavor.javaFileListFlavor)
            
            droppedObj match {
              case list : java.util.List[_] =>
                val file = list.get(0).asInstanceOf[File]
                val name = file.getName
                val lastDot = name.lastIndexOf('.')
                val ext = name.drop(lastDot+1)
                if(ext == "eli") 
                  GUIActor ! file
                if(ext == "treexml" || ext == "treejson") {
                  GUIActor ! ("OpenTree", file)
                }
                true
              case _ => 
                false 
            }
        }
    }
    
    this.peer.setTransferHandler(transferHandler)
    
    start()
}


