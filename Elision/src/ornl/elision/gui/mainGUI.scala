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

import swing._
import swing.BorderPanel.Position._
import concurrent.ops._
import sys.process._
import java.io._
import java.awt.Color
import java.awt.Dimension
import sage2D.GamePanel


/**	
 * This is the Elision GUI's main window.
 */
object mainGUI extends SimpleSwingApplication {
	
	/** The universal background color for the GUI's panels */
	val bgColor = new Color(0xBBBBff)
	
	/** Eva's configuration settings */
	val config = new EvaConfig
    GUIActor.treeBuilder.treeMaxDepth = config.maxTreeDepth
    
    /** 
     * This string controls what mode Eva is currently running in. 
     * Currently supported modes are: 
     * Elision
     */
     
    var mode = ""
    
    val defaultTitle = "Elision Visualization Assistant"
	
	/** The panel housing the onboard console */
	val consolePanel = new ConsolePanel
	
	/** The tabbed panel housing information about the visualization properties. */
	val sidePanel = new SidePanel
	
	/** The panel housing Eva's current visualization. */
	var visPanel : GamePanel = null
	
	GUIActor.start
	
    /** The menu bar */
    val guiMenuBar = new GuiMenuBar
    
    /** The window's Frame object */
    val frame = new MainFrame {
		title = defaultTitle // This may change depending on Eva's mode.
		menuBar = guiMenuBar
        
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
                syntax.SyntaxFormatter.regexes = elision.EliRegexes
                visPanel = new trees.TreeVisPanel
            case "Welcome" =>
                syntax.SyntaxFormatter.regexes = null
                visPanel = new welcome.WelcomePanel
            case _ =>
                syntax.SyntaxFormatter.regexes = null
                visPanel = new welcome.WelcomePanel
                mode = "Welcome"
        }
        
        config.bootMode = mode
        config.save
        
        // Change the tabs on the side panel
        sidePanel.changeMode(mode)
        
        // Change the REPL panel
        consolePanel.changeMode(mode)
        
        // Change the menu bar
        guiMenuBar.changeMode(mode)
        
        // Change the window's title
        frame.title = defaultTitle + " (" + mode + " mode)"
        
        frame.contents = new BorderPanel {
			layout( visPanel) = Center
			layout( consolePanel) = South
			layout( sidePanel) = East
        }
        frame.size = new Dimension(1024,800)
        
        // get focus in the REPL panel
		consolePanel.console.requestFocusInWindow
    }
    
    // start in whatever mode was used last.
    changeMode(config.bootMode)
}


/**	This is the menu bar for the GUI */
class GuiMenuBar extends MenuBar {

	// File menu
	
	val fileMenu = new Menu("File")
	fileMenu.mnemonic = event.Key.F
	this.contents += fileMenu

		// Open : opens an Elision script file to be immediately processed by the Repl as input.
		
		var openDirectory = mainGUI.config.lastOpenPath //"."
		
		val openItem = new MenuItem(Action("Open      Ctrl+ O") {
			val fc = new FileChooser(new File(openDirectory))
			val result = fc.showOpenDialog(null)
			val selFile = fc.selectedFile
			if(selFile != null && result == FileChooser.Result.Approve) {
				openDirectory = selFile.getParent
				mainGUI.config.lastOpenPath = openDirectory
				mainGUI.config.save
				GUIActor ! selFile
			}
		} )
		openItem.mnemonic = event.Key.O
		fileMenu.contents += openItem
		
		// Quit : exits the GUI
		
		val quitItem = new MenuItem(Action("Quit") {
			System.exit(0)
		} )
		quitItem.mnemonic = event.Key.Q
		fileMenu.contents += quitItem
		
	// View menu	
		
	val viewMenu = new Menu("View")
	viewMenu.mnemonic = event.Key.V
	this.contents += viewMenu
		
		// Reset Camera : reset's the camera in the visualization panel.
		
		val resetCameraItem = new MenuItem(Action("Reset Camera") {
            mainGUI.visPanel match {
                case camPanel : HasCamera =>
                    camPanel.camera.reset
            }
		} )
		resetCameraItem.mnemonic = event.Key.R
		
	
    // Mode menu (Does not change. Its contents remain the same for ALL Eva modes.)
		
	val modeMenu = new Menu("Mode")
	modeMenu.mnemonic = event.Key.M
	this.contents += modeMenu
        
        // Welcome
        
        val welcomeModeItem = new MenuItem(Action("Welcome") {
			GUIActor ! ("changeMode", "Welcome")
		} )
		welcomeModeItem.mnemonic = event.Key.W
		modeMenu.contents += welcomeModeItem
        
        // Elision
		
		val elisionModeItem = new MenuItem(Action("Elision") {
			GUIActor ! ("changeMode", "Elision")
		} )
		elisionModeItem.mnemonic = event.Key.E
		modeMenu.contents += elisionModeItem
	

	// Help menu	
		
	val helpMenu = new Menu("Help")
	helpMenu.mnemonic = event.Key.H
	this.contents += helpMenu
	
		// Help : Opens help documents for the GUI
		
		val helpItem = new MenuItem(Action("Help") {
			val helpDia = mainGUI.mode match {
                    case "Elision" => new elision.EliHelpDialog
                    case _ => null
                }
		} )
		helpItem.mnemonic = event.Key.F1
		
		// Help : Opens help documents for the GUI
		
		val aboutItem = new MenuItem(Action("About") {
			val helpDia = mainGUI.mode match {
                    case "Elision" => new elision.EliAboutDialog
                    case _ => null
                }
		} )
		aboutItem.mnemonic = event.Key.A
	

    
    /** Changes the menu bar for the GUI according to the new mode. */
    def changeMode(mode : String) : Unit = {
        this.contents.clear
        
        mode match {
            case "Elision" =>
                this.contents += fileMenu
                    fileMenu.contents += openItem
                    fileMenu.contents += quitItem
                
                this.contents += viewMenu
                    viewMenu.contents += resetCameraItem
                
                this.contents += modeMenu
                
                this.contents += ConsoleMenu.apply(mode)
                
                this.contents += trees.TreeVisMenu.apply(mode)
                
                this.contents += helpMenu
                    helpMenu.contents += helpItem
                    helpMenu.contents += aboutItem
            case _ => // default menu bar
                this.contents += fileMenu
                    fileMenu.contents += openItem
                    fileMenu.contents += quitItem
                
                this.contents += modeMenu
                
                this.contents += helpMenu
                    helpMenu.contents += helpItem
                    helpMenu.contents += aboutItem
        }
    }
    
    
    
	
	
	
}











