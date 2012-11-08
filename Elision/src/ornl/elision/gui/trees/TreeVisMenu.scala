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

import swing._
import swing.BorderPanel.Position._

import ornl.elision.gui._

/** The Tree menu for Eva's menu bar. This only shows up for modes that use a TreeVisPanel for visualization. */
object TreeVisMenu {
    
    // File menu
        
        var openDirectory = mainGUI.config.lastOpenPath //"."
        
        val openTreeItem = new MenuItem(Action("Open Tree Visualization") {
			val fc = new FileChooser(new java.io.File(openDirectory))
            fc.fileFilter = new TreeFileFilter
			val result = fc.showOpenDialog(null)
			val selFile = fc.selectedFile
			if(selFile != null && result == FileChooser.Result.Approve) {
				openDirectory = selFile.getParent
				mainGUI.config.lastOpenPath = openDirectory
				mainGUI.config.save
                GUIActor ! ("OpenTree", selFile)
			}
		} )
        
        val saveTreeItem = new MenuItem(Action("Save Tree Visualization as XML") {
			val fc = new FileChooser(new java.io.File(openDirectory))
            fc.fileFilter = new TreeXMLFileFilter
			val result = fc.showSaveDialog(null)
			val selFile = fc.selectedFile
			if(selFile != null && result == FileChooser.Result.Approve) {
				openDirectory = selFile.getParent
				mainGUI.config.lastOpenPath = openDirectory
				mainGUI.config.save
				GUIActor ! ("SaveTree", selFile)
			}
		} )
        
        val saveJSONTreeItem = new MenuItem(Action("Save Tree Visualization as JSON") {
			val fc = new FileChooser(new java.io.File(openDirectory))
            fc.fileFilter = new TreeJSONFileFilter
			val result = fc.showSaveDialog(null)
			val selFile = fc.selectedFile
			if(selFile != null && result == FileChooser.Result.Approve) {
				openDirectory = selFile.getParent
				mainGUI.config.lastOpenPath = openDirectory
				mainGUI.config.save
				GUIActor ! ("SaveTreeJSON", selFile)
			}
		} )

    // Tree menu		
	val treeMenu = new Menu("Tree")
	treeMenu.mnemonic = event.Key.T

        // Set Decompression Depth : Opens dialog to change the tree visualization's decompression depth.
		val setDepthItem = new MenuItem(Action("Set Decompression Depth") {
			val depthDia = new DepthDialog
		} )
		setDepthItem.mnemonic = event.Key.D
		
        
        // Set Node Limit : 
        val setNodeLimitItem = new MenuItem(Action("Set Node Limit") {
			val dia = new NodeLimitDialog
		} )
		setNodeLimitItem.mnemonic = event.Key.O
		
        
        // Set Maximum Tree Depth : 
        val setMaxDepthItem = new MenuItem(Action("Set Maximum Tree Depth") {
			val maxDepthDia = new MaxDepthDialog
		} )
		setMaxDepthItem.mnemonic = event.Key.M
		
        
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
		
        
    /** Constructs a Tree menu structure appropriate for the given mode. */
    def apply(mode : String) : Menu = {
        treeMenu.contents.clear
        
        mode match {
            case "Elision" =>
                treeMenu.contents += setDepthItem
                treeMenu.contents += setNodeLimitItem
                treeMenu.contents += setMaxDepthItem
                treeMenu.contents += disableNodeColoringItem
                treeMenu.contents += disableTreeItem
            case _ =>
                treeMenu.contents += setDepthItem
                treeMenu.contents += disableTreeItem
        }
        
        treeMenu
    }
}



/** The dialog window for the "Tree > Set Decompression Depth" menu item */
class DepthDialog extends Dialog {
    this.title = "Set Decompression Depth"
    val inset = 3
    background = new Color(0xBBBBff)
    
    val depthInput = new TextField(10) { 
        listenTo(keys) 
        reactions += { case e : swing.event.KeyTyped => if(e.char == '\n') enterInput(text) }
        text = "" + mainGUI.config.decompDepth
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
                mainGUI.config.decompDepth = fieldInt
                mainGUI.config.save
                mainGUI.visPanel.curLevel match {
                    case treeVisPanel : TreeVisPanel => 
                        treeVisPanel.selectNode(treeVisPanel.treeSprite.selectedNode)
                }  
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





/** The dialog window for the "Tree > Set Maximum Tree Depth" menu item */
class MaxDepthDialog extends Dialog {
    this.title = "Set Maximum Tree Depth"
    val inset = 3
    
    val linesInput = new TextField(10) { 
        listenTo(keys) 
        reactions += { case e : swing.event.KeyTyped => if(e.char == '\n') enterInput(text) }
        text = "" + GUIActor.treeBuilder.treeMaxDepth
    }
    val okBtn = new Button(Action("OK") {enterInput(linesInput.text)})
    val cancelBtn = new Button(Action("Cancel") { close } )
    
    contents = new BorderPanel {
        border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
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
     * @param input		The input string being evaluated as the new value for the maximum tree depth.
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




/** The dialog window for the "Tree > Set Node Limit" menu item */
class NodeLimitDialog extends Dialog {
    this.title = "Set Node Limit"
    val inset = 3
    
    val linesInput = new TextField(10) { 
        listenTo(keys) 
        reactions += { case e : swing.event.KeyTyped => if(e.char == '\n') enterInput(text) }
        text = "" + mainGUI.config.nodeLimit
    }
    val okBtn = new Button(Action("OK") {enterInput(linesInput.text)})
    val cancelBtn = new Button(Action("Cancel") { close } )
    
    contents = new BorderPanel {
        border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
        layout( new GridPanel(2,1) { 
                    contents += new Label("Enter node limit: (integer)")
                    contents += new Label("(< 2 will make there be no node limit)") 
                } ) = North
        layout(linesInput) = Center
        layout(new FlowPanel {
            contents += okBtn
            contents += cancelBtn
        } ) = South
    }
    
    
    /** 
     * processes the input for the dialog when the user clicks OK or presses Enter 
     * @param input		The input string being evaluated as the new value for the node limit.
     */
    private def enterInput(input : String) : Unit = {
        try {
            val fieldInt = input.toInt
            mainGUI.config.nodeLimit = fieldInt
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


/** A FileFilter that only accepts .treexml files */
class TreeXMLFileFilter extends javax.swing.filechooser.FileFilter  {
    def accept(f : java.io.File) : Boolean = {
        if(f.isDirectory) return true
        
        val name = f.getName
        val lastDot = name.lastIndexOf('.')
        val ext = name.drop(lastDot+1)
        if(ext == "treexml") true
        else false
    }
    
    def getDescription : String = {
        "Eva treexml files"
    }
}

/** A FileFilter that only accepts .treejson files */
class TreeJSONFileFilter extends javax.swing.filechooser.FileFilter  {
    def accept(f : java.io.File) : Boolean = {
        if(f.isDirectory) return true
        
        val name = f.getName
        val lastDot = name.lastIndexOf('.')
        val ext = name.drop(lastDot+1)
        if(ext == "treejson") true
        else false
    }
    
    def getDescription : String = {
        "Eva treejson files"
    }
}


/** A FileFilter that only accepts .treexml and .treejson files */
class TreeFileFilter extends javax.swing.filechooser.FileFilter  {
    def accept(f : java.io.File) : Boolean = {
        if(f.isDirectory) return true
        
        val name = f.getName
        val lastDot = name.lastIndexOf('.')
        val ext = name.drop(lastDot+1)
        if(ext == "treejson" || ext == "treexml") true
        else false
    }
    
    def getDescription : String = {
        "Eva treejson and treexml files"
    }
}

