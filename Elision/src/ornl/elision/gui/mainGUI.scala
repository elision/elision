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


import concurrent.ops._
import java.io._
import java.awt.Color
import java.awt.Dimension
import swing._
import swing.BorderPanel.Position._
import sys.process._


/**  
 * This is the Eva's main application  window.
 */
object mainGUI extends SimpleSwingApplication {
  
  /** The universal background color for the GUI's panels */
  val bgColor = new Color(0xBBBBff)
  
  /** The default title bar text. */
  val defaultTitle = "Elision Visualization Assistant"
  
  /** Eva's configuration settings */
//  val config = new EvaConfig
  GUIActor.treeBuilder.treeMaxDepth = EvaConfig.maxTreeDepth
  
  /** 
   * This string controls what mode Eva is currently running in. 
   * Currently supported modes are: 
   * Welcome
   * Elision
   */
  var mode = ""
  
  /** The panel housing the onboard console */
  val consolePanel = new console.ConsolePanel
  
  /** The tabbed panel housing information about the visualization properties. */
  val sidePanel = new SidePanel
  
  /** The panel housing Eva's current visualization. */
  var visPanel = new EvaVisPanel
  
  GUIActor.start
  
  /** The menu bar */
  val evaMenuBar = new EvaMenuBar
  
  /** The window's Frame object */
  val frame = new MainFrame {
    title = defaultTitle // This may change depending on Eva's mode.
    menuBar = evaMenuBar
        
    contents = new BorderPanel {
      layout( visPanel) = Center
      layout( consolePanel) = South
      layout( sidePanel) = East
    }
        
    size = new Dimension(1024,800)
    visible = true
  }
  def top = frame
    
    
  /** Changes the mode that the GUI is currently running in. */
  def changeMode(mmode : String) : Unit = {
    mode = mmode
    
    // Change the SyntaxFormatter's regex set and the visualization panel
    if(visPanel != null) visPanel.clean
    mode match {
      case "Elision" => 
        visPanel.changeLevel("EliTreeVis")
      case "Welcome" =>
        visPanel.changeLevel("welcome")
      case _ =>
        visPanel.changeLevel("welcome")
        mode = "Welcome"
    }
    
    // save the new current mode in our config file.
    EvaConfig.bootMode = mode
    EvaConfig.save
    
    // Change the mode for mode-dependent components.
    sidePanel.changeMode(mode)
    consolePanel.changeMode(mode)
    evaMenuBar.changeMode(mode)
    frame.title = defaultTitle + " (" + mode + " mode)"
    
    // get focus in the REPL panel
    consolePanel.console.requestFocusInWindow
    
    frame.repaint
  }
  
  // start in whatever mode was used last.
  changeMode(EvaConfig.bootMode)
}


