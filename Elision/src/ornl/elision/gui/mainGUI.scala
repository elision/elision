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

import sage2D._



/**	
 * This is the Elision GUI's main window.
 */
object mainGUI extends SimpleSwingApplication {
	
	/** The universal background color for the GUI's panels */
	val bgColor = new Color(0xBBBBff)
	
	/** Eva's configuration settings */
	val config = new EvaConfig
    GUIActor.treeBuilder.treeMaxDepth = config.maxTreeDepth
	
	/** The panel housing the onboard Elision REPL */
	val consolePanel = new ConsolePanel
	
	/** The panel housing the atom properties display */
	val propsPanel = new PropertiesPanel
	
	/** The panel housing the rewrite tree visualization */
	val treeVisPanel = new TreeVisPanel
	
	GUIActor.start
	
    val guiMenuBar = new GuiMenuBar
    
	/** The window's Frame object */
	def top = new MainFrame {
		title = "Elision Visualization Assistant"
		menuBar = guiMenuBar
		contents = new BorderPanel {
			layout( treeVisPanel) = Center
			layout( consolePanel) = South
			layout( propsPanel) = East
		}
		size = new Dimension(1024,800)
		visible = true
		
		// get focus in the REPL panel
		consolePanel.console.requestFocusInWindow
		
		
	}
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
			mainGUI.treeVisPanel.camera.reset
		} )
		resetCameraItem.mnemonic = event.Key.R
		viewMenu.contents += resetCameraItem
		
		// Set Decompression Depth : Opens dialog to change the tree visualization's decompression depth.
		
		val setDepthItem = new MenuItem(Action("Set Decompression Depth") {
			val depthDia = new DepthDialog
		} )
		setDepthItem.mnemonic = event.Key.D
		viewMenu.contents += setDepthItem
		
		// Set REPL Maximum Lines : Opens dialog to change the maximum lines in the onboard REPL
		
		val setMaxLinesItem = new MenuItem(Action("Set REPL Maximum Lines") {
			val maxLinesDia = new MaxLinesDialog
		} )
		setMaxLinesItem.mnemonic = event.Key.L
		viewMenu.contents += setMaxLinesItem
        
        // Set Maximum Tree Depth : 
        
        val setMaxDepthItem = new MenuItem(Action("Set Maximum Tree Depth") {
			val maxDepthDia = new MaxDepthDialog
		} )
		setMaxDepthItem.mnemonic = event.Key.M
		viewMenu.contents += setMaxDepthItem
        
        // Disable Tree Construction : 
        
        val disableTreeItem = new CheckMenuItem("Disable Tree Construction")
        disableTreeItem.peer.setState(mainGUI.config.disableTree)
        GUIActor.disableTreeBuilder = disableTreeItem.peer.getState
        disableTreeItem.listenTo(disableTreeItem)
        disableTreeItem.reactions += {
            case _ => 
                GUIActor.disableTreeBuilder = disableTreeItem.peer.getState
                mainGUI.config.disableTree = disableTreeItem.peer.getState
                mainGUI.config.save
        }
		disableTreeItem.mnemonic = event.Key.T
		viewMenu.contents += disableTreeItem
        
        // Disable Node Syntax Coloring : 
        
        val disableNodeColoringItem = new CheckMenuItem("Disable Node Syntax Coloring")
        disableNodeColoringItem.peer.setState(mainGUI.config.disableNodeSyntaxColoring)
        
        disableNodeColoringItem.listenTo(disableNodeColoringItem)
        disableNodeColoringItem.reactions += {
            case _ => 
                mainGUI.config.disableNodeSyntaxColoring = disableNodeColoringItem.peer.getState
                mainGUI.config.save
        }
		disableNodeColoringItem.mnemonic = event.Key.N
		viewMenu.contents += disableNodeColoringItem
	
	// Help menu	
		
	val helpMenu = new Menu("Help")
	helpMenu.mnemonic = event.Key.H
	this.contents += helpMenu
	
		// Help : Opens help documents for the GUI
		
		val helpItem = new MenuItem(Action("Help") {
			val helpDia = new HelpDialog
		} )
		helpItem.mnemonic = event.Key.F1
		helpMenu.contents += helpItem
		
		// Help : Opens help documents for the GUI
		
		val aboutItem = new MenuItem(Action("About") {
			val helpDia = new AboutDialog
		} )
		aboutItem.mnemonic = event.Key.A
		helpMenu.contents += aboutItem
	
	

	/** The dialog window for the "View > Set Decompression Depth" menu item */
	class DepthDialog extends Dialog {
		this.title = "Set Decompression Depth"
		val inset = 3
		border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
		background = new Color(0xBBBBff)
		
		val depthInput = new TextField(10) { 
			listenTo(keys) 
			reactions += { case e : swing.event.KeyTyped => if(e.char == '\n') enterInput(text) }
			text = "" + mainGUI.treeVisPanel.decompDepth
		}
		val okBtn = new Button(Action("OK") {enterInput(depthInput.text)})
		val cancelBtn = new Button(Action("Cancel") { close } )
		
		contents = new BorderPanel {
			border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
			layout(new Label("Enter new node decompression depth: (integer > 0)")) = North
			layout(depthInput) = Center
			layout(new FlowPanel {
				contents += okBtn
				contents += cancelBtn
			} ) = South
		}
		
		
		/** 
		 * processes the input for the dialog when the user clicks OK or presses Enter 
		 * @param input		The input string being evaluated as the new decompression depth
		 */
		private def enterInput(input : String) : Unit = {
			// if the input is an integer > 0, proceed to set the decompression depth to the input. 
			// Otherwise, just close the dialog.
			
			try {
				val fieldInt = input.toInt
				if(fieldInt > 0) {
					mainGUI.treeVisPanel.decompDepth = fieldInt
					mainGUI.config.decompDepth = fieldInt
					mainGUI.config.save
					mainGUI.treeVisPanel.selectNode(mainGUI.treeVisPanel.treeSprite.selectedNode)
				}
			} catch {
				case _ =>
			}
			
			// close the dialog when we finish processing input
			close
		}
		
		// open the dialog when it is finished setting up
		open
	}
	
	/** The dialog window for the "View > Set REPL Maximum Lines" menu item */
	class MaxLinesDialog extends Dialog {
		this.title = "Set REPL Maximum Lines"
		val inset = 3
		border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
		
		val linesInput = new TextField(10) { 
			listenTo(keys) 
			reactions += { case e : swing.event.KeyTyped => if(e.char == '\n') enterInput(text) }
			text = "" + mainGUI.consolePanel.tos.maxLines
		}
		val okBtn = new Button(Action("OK") {enterInput(linesInput.text)})
		val cancelBtn = new Button(Action("Cancel") { close } )
		
		contents = new BorderPanel {
			border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
			val minLines = ConsolePanel.infiniteMaxLines
			layout( new GridPanel(2,1) { 
						contents += new Label("Enter max lines: (integer >= " + minLines + ")")
						contents += new Label("(<" + minLines + " will make there be no maximum)") 
					} ) = North
			layout(linesInput) = Center
			layout(new FlowPanel {
				contents += okBtn
				contents += cancelBtn
			} ) = South
		}
		
		
		/** 
		 * processes the input for the dialog when the user clicks OK or presses Enter 
		 * @param input		The input string being evaluated as the new value for the REPL's maximum lines.
		 */
		private def enterInput(input : String) : Unit = {
			// if the input is an integer > 0, proceed to set the decompression depth to the input. 
			// Otherwise, just close the dialog.
			
			try {
				val fieldInt = input.toInt
				mainGUI.consolePanel.tos.maxLines = fieldInt
				mainGUI.config.replMaxLines = fieldInt
				mainGUI.config.save
				// close the dialog when we finish processing input
				close
			} catch {
				case _ =>
			}
		}
		
		// open the dialog when it is finished setting up
		open
	}
	
    
    
    /** The dialog window for the "View > Set Maximum Tree Depth" menu item */
	class MaxDepthDialog extends Dialog {
		this.title = "Set Maximum Tree Depth"
		val inset = 3
		border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
		
		val linesInput = new TextField(10) { 
			listenTo(keys) 
			reactions += { case e : swing.event.KeyTyped => if(e.char == '\n') enterInput(text) }
			text = "" + GUIActor.treeBuilder.treeMaxDepth
		}
		val okBtn = new Button(Action("OK") {enterInput(linesInput.text)})
		val cancelBtn = new Button(Action("Cancel") { close } )
		
		contents = new BorderPanel {
			border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
			val minLines = ConsolePanel.infiniteMaxLines
			layout( new GridPanel(2,1) { 
						contents += new Label("Enter max depth: (integer)")
						contents += new Label("(< 0 will make there be no depth limit)") 
					} ) = North
			layout(linesInput) = Center
			layout(new FlowPanel {
				contents += okBtn
				contents += cancelBtn
			} ) = South
		}
		
		
		/** 
		 * processes the input for the dialog when the user clicks OK or presses Enter 
		 * @param input		The input string being evaluated as the new value for the REPL's maximum lines.
		 */
		private def enterInput(input : String) : Unit = {
			// if the input is an integer > 0, proceed to set the decompression depth to the input. 
			// Otherwise, just close the dialog.
			
			try {
				val fieldInt = input.toInt
				GUIActor.treeBuilder.treeMaxDepth = fieldInt
				mainGUI.config.maxTreeDepth = fieldInt
				mainGUI.config.save
				// close the dialog when we finish processing input
				close
			} catch {
				case _ =>
			}
		}
		
		// open the dialog when it is finished setting up
		open
	}
	
	
	
}











