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

package ornl.elision.gui.trees

import scala.swing.BorderPanel.Position._
import scala.concurrent.ops._
import sys.process._

import java.io._
import java.lang.Object
import java.awt.Color
import java.awt.Image
import java.awt.Graphics2D
import java.awt.geom.Point2D
import java.awt.image.BufferedImage
import java.awt.image.VolatileImage
import javax.swing.SwingWorker

import scala.swing.Dialog

import sage2D._
import sage2D.input._

import ornl.elision.gui.EvaConfig
import ornl.elision.gui.mainGUI

/**
 * This level shall display a tree structure showing the rewriting hierarchy 
 * of the last atom string passed to the REPL as input.
 */
class TreeVisLevel(game : GamePanel) extends Level(game, null) {
  
  var timerLock = false
  
  /** A SwingWorker thread used for concurrently rendering the panel's image. */
  val renderThread = new TreeVisThread(this,game.timer)
  renderThread.changeTree(DefaultTreeSprite)
  renderThread.start
  
  /** 
   * We actually run the logic for this level is a thread separate from the EDT. 
   * Here, we just tell the thread that it's safe to run another iteration.
   */
  def logic : Unit = {
    // All the logic for this level is done inside the renderThread.
  }
    
  def loadData : Unit = {}
    
  /** 
   * Cleans up the TreeVis panel when we're done with it. 
   * Most importantly, it causes its worker thread to exit after its 
   * current iteration.
   */
  def clean : Unit = {
    renderThread.exitFlag = true
  }
    
    
  /** Draws the tree visualization. */
  def render(g : Graphics2D) : Unit = {
    // render the most recent image produced by our rendering thread.
    g.drawImage(renderThread.getImage, null, null)
    
    // display HUD information
    g.setColor(new Color(0x000000))
    g.drawString("frame rate: " + game.timer.fpsCounter, 10,32)
  //  g.drawString("camera zoom: " + camera.zoom, 10,52)
  }
  
  
  /**
   * Selects a node in the current tree visualization, 
   * does some fancy camera work to make sure that that node stays in the same place onscreen
   * after the graph is re-expanded, and displays that node's information in the atom properties panel.
   * @param node    The node being selected in our tree.
   */
  def selectNode(node : NodeSprite) : Unit = {
    renderThread.selectNode(node)
  }
  
  /** Gets the current treesprite in the visualization. */
  def treeSprite : TreeSprite = {
    renderThread.getTreeSprite
  }
  
  
  /** Changes the tree currently being viewed. */
  def changeTree(newTree : TreeSprite) : Unit = {
    renderThread.changeTree(newTree)
  }
  
  /** Returns the mouse's position in the panel's view coordinates. */
  def mouseScreenPosition : Point2D = {
    renderThread.getMouseScreenPosition
  }
  
}


/** 
 * A worker thread for running the Tree Visualization interaction logic and 
 * rendering, so that the EDT doesn't get tied up during large tree renderings. 
 */
class TreeVisThread(val treeVis : TreeVisLevel, val gt : GameTimer) extends Thread { 
  
  /** The sprite representing the visualization of the rewrite tree */
  var treeSprite : TreeSprite = null
  val treeLock = new Object
  
  /** The Image containing the latest rendering of the visualization tree. */
  var renderedImage : Image = new BufferedImage(640,480, BufferedImage.TYPE_INT_ARGB)
  val imgLock = new Object
  
  /** The panel's camera for panning around and zooming in the visualization */
  val camera = new Camera(0, 0, 640, 480)
  
  /** Keeps track of the mouse's position in world coordinates */
  var mouseWorldPosition : Point2D = new Point2D.Double(0,0)
  
  /** Keeps track of the mouse's position in screen coordinates. Necessary because of multi-threadedness. */
  var mouseScreenPosition : Point2D = new Point2D.Double(0,0)
  
  /** Gets the decompression depth of trees in this visualization. */
  def decompDepth : Int = EvaConfig.decompDepth
  
  /** run exits safely when this becomes true. */
  var exitFlag = false
  
  
  override def run {
    
    // The thread does a logic-render loop as fast as possible.
    while(!exitFlag) {
      Thread.sleep(1)
      
      // Run the panel's logic and then render it.
      logic
      setImage(createdRenderedFrame)
    } // endwhile
  }
  
  
  
  /**
   * Selects a node in the current tree visualization, 
   * does some fancy camera work to make sure that that node stays in the same place onscreen
   * after the graph is re-expanded, and displays that node's information in the atom properties panel.
   * @param node    The node being selected in our tree.
   */
  def selectNode(node : NodeSprite) : Unit = {
    // don't allow null parameter.
    if(node == null) {
      return
    }
    
    
    treeLock.synchronized {
      try {
        val clickedNodeScreenPos = node.getScreenPosition
        camera.moveCenter(clickedNodeScreenPos)
        treeSprite.selectNode(node, decompDepth)
        
        // Let registered Reactors know that the node has been clicked.
        treeVis.publish(new NodeClickedEvent(node))
        
        camera.x = node.worldX
        camera.y = node.worldY
      }
      catch {
        case e : Throwable => 
          Dialog.showMessage(mainGUI.visPanel, e.toString, "Error", Dialog.Message.Error)
      }
    }
  }
  
  /** 
   * Performs one iteration through the visualization panel's logic. 
   * It's mostly just processing mouse input for controlling the camera and selecting nodes.
   */
  def logic : Unit = {
    val mouse = treeVis.mouse
    val game = treeVis.game
    
    // There could be a change in the mouse's position between calls to mouse due
    // to the threaded nature of this Level. So, make sure we're using only one
    // mouse screen position per iteration.
    treeLock.synchronized {
      mouseScreenPosition = mouse.position
    }
    
    // select and begin dragging nodes
    if(mouse.justLeftPressed || mouse.justRightPressed) {
      var clickedNode : NodeSprite = null
      treeLock.synchronized {
        clickedNode = treeSprite.detectMouseOver(mouseWorldPosition)
      }
      
      selectNode(clickedNode)
      
      treeLock.synchronized {
        camera.startDrag(mouseScreenPosition)
      }
    }
    
    treeLock.synchronized {
      
      // drag-pan camera
      if(mouse.isLeftPressed) {
        camera.drag(mouseScreenPosition)
        camera.startDrag(mouseScreenPosition)
      }
      
      // Zooming
      if(mouse.wheel == -1) {
        camera.zoomAtScreen(10.0/7.0, mouseScreenPosition)
      }
      if(mouse.wheel == 1) {
        camera.zoomAtScreen(7.0/10.0, mouseScreenPosition)
      }
      
      // update the camera's transform based on its new state.
      camera.pWidth = game.size.width
      camera.pHeight = game.size.height
      camera.updateTransform
      
      // update the mouse's current world coordinates
      mouseWorldPosition = camera.screenToWorldCoords(mouseScreenPosition)
    }
  }
  
  
  /** Changes the tree currently being viewed. */
  def changeTree(newTree : TreeSprite) : Unit = {
    treeLock.synchronized {
      treeSprite = newTree
      treeSprite.selectNode(treeSprite.root, decompDepth) 
      camera.reset
    }
  }
  
  
  /** Returns the current tree sprite. */
  def getTreeSprite : TreeSprite = {
    treeLock.synchronized {
      treeSprite
    }
  }
  
  /** Returns the mouse's screen position. */
  def getMouseScreenPosition : Point2D = {
    treeLock.synchronized {
      mouseScreenPosition
    }
  }
  
  
  
  
  
  /** Prepares an image containing the current frame rendering for the tree visualization.*/
  def createdRenderedFrame : Image = {
    val game = treeVis.game
    
    val image = game.peer.createVolatileImage(game.size.width, 
                      game.size.height)
    var g = image.createGraphics

    // white background
    g.setColor(new Color(0xffffff))
    g.fillRect(0, 0, game.size.width, game.size.height)
    
    treeLock.synchronized {
      // apply the Camera transform and then render.
      g.setTransform(camera.getTransform)
      treeVis.treeSprite.camera = camera
      treeVis.treeSprite.render(g)
      g.dispose
    }
    
    // return the completed image
    image
  }
  
  
  /** Sets the rendered image available for use after rendering. */
  def setImage(img : Image) : Unit = {
    imgLock.synchronized {
      renderedImage = img
    }
  }
  
  /** Gets the latest rendered image available for use. */
  def getImage : Image = {
    imgLock.synchronized {
      renderedImage
    }
  }
}



/** 
 * Eva's welcome message tree for Elision mode.
 */
object DefaultTreeSprite extends TreeSprite(0,0) {
  makeRoot("root")
    root.makeChild("This ") 
    root.makeChild("tree") 
      root(1).makeChild("is") 
      root(1).makeChild("just")
        root(1)(1).makeChild("an") 
        root(1)(1).makeChild("example.") 
}



