package ElisionGUI

import swing._
import scala.swing.BorderPanel.Position._
import scala.concurrent.ops._
import sys.process._
import java.io._
import java.awt.Graphics2D

import sage2D._



/**	
 * This is the Elision GUI's main window.
 */

object mainGUI extends SimpleSwingApplication {
	
	/** The universal background color for the GUI's panels */
	val bgColor = new Color(0xBBBBff)
	
	/** The panel housing the onboard Elision REPL */
	val consolePanel = new ConsolePanel
	
	/** The panel housing the atom properties display */
	val propsPanel = new PropertiesPanel
	
	/** The panel housing the rewrite tree visualization */
	val treeVisPanel = new TreeVisPanel

	GUIActor.start
	
	/**
	 * The window's Frame object
	 */
	
	def top = new MainFrame {
		title = "Elision Visualization Assistant"
		menuBar = guiMenuBar
		contents = new BorderPanel {
			layout( treeVisPanel) = Center
			layout( consolePanel) = South
			layout( propsPanel) = East
		}
		
		visible = true
	}
	
	
	
	// get focus in the REPL panel
	consolePanel.console.requestFocusInWindow
	// treeVisPanel.requestFocusInWindow
	// treeVisPanel.requestFocus
}


/**	
 * This is the menu bar for the GUI
 */

object guiMenuBar extends MenuBar {
	
	// File menu
	
	val fileMenu = new Menu("File")
	fileMenu.mnemonic = event.Key.F
	this.contents += fileMenu
		
		// Open : opens an Elision script file to be immediately processed by the Repl as input.
		
		val openItem = new MenuItem(Action("Open") {
			val fc = new FileChooser(new File("."))
			fc.showOpenDialog(null)
			val selFile = fc.selectedFile
			if(selFile != null) {
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
		 */
		
		private def enterInput(input : String) : Unit = {
			// if the input is an integer > 0, proceed to set the decompression depth to the input. 
			// Otherwise, just close the dialog.
			
			try {
				val fieldInt = input.toInt
				if(fieldInt > 0) {
					mainGUI.treeVisPanel.decompDepth = fieldInt
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
			val minLines = TextAreaOutputStream.infiniteMaxLines
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
		 */
		
		private def enterInput(input : String) : Unit = {
			// if the input is an integer > 0, proceed to set the decompression depth to the input. 
			// Otherwise, just close the dialog.
			
			try {
				val fieldInt = input.toInt
				mainGUI.consolePanel.tos.maxLines = fieldInt
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



/**
 * This panel shall display a tree structure showing the rewriting hierarchy 
 * of the last atom string passed to the REPL as input.
 */

class TreeVisPanel extends GamePanel {
	background = new Color(0xffffff)
	preferredSize = new Dimension(640,480)
	
	/** The panel's camera for panning around and zooming in the visualization */
	val camera = new Camera(0,0,640,480)
	
	/** The mouse input interface for the panel */
	val mouseIn = new MouseInput(this)
	
	val keyboardIn = new KeyboardInput(this)
	//focusable = true
	
	/** Keeps track of the mouse's position in world coordinates */
	var mouseWorldPosition : java.awt.geom.Point2D = new java.awt.geom.Point2D.Double(0,0)
	
	/** The sprite representing the visualization of the rewrite tree */
	var treeSprite = TreeSprite.buildTouhouTree
	treeSprite.selectNode(treeSprite.root,2) 
	
	/** The current decompression depth for the visualization trees */
	var decompDepth = 2

	/** A variable used only by the loading screen animation. It's not important. */
	var loadingThingAngle = 0
	
	
	/** 
	 * test painting function.
	 * This draws a grid of red lines centered at the origin spaced 30 pixels apart.
	 * It is no longer invoked anywhere. It was used during early development of the GUI to test the SAGE 2D library. 
	 * @param g		The graphics context of the component this is being painted to. In this case, it's the TreeVisPanel's context.
	 */
	
	def testPaint(g : Graphics2D) : Unit = {
		
		import java.awt.geom.CubicCurve2D
	
		for(i <- -300 to 300 if(i % 30 == 0))	{
			g.setColor(new Color(0xFF9999))
			
			val curve1 = new CubicCurve2D.Double(-300,0,-100,0,100,i,300,i)
			val curve2 = new CubicCurve2D.Double(0,-300,0,-100,i,100,i,300)
			g.draw(curve1)
			g.draw(curve2)
			g.drawLine(-300,i,300,i)
			g.drawLine(i,-300,i,300)
		}
		
		g.setColor(new Color(0xFF0000))
		g.drawLine(-300,0,300,0)
		g.drawLine(0,-300,0,300)
	}
	
	/** 
	 * The method invoked for painting the tree visualization.
	 * @param g		The graphics context of the component this is being painted to. In this case, it's the TreeVisPanel's context.
	 */
	
	def mainPaint(g : Graphics2D) : Unit = {
		// store affine transforms for later use
		val camTrans = camera.getTransform
		val origTrans = g.getTransform
		
		// apply the Camera transform
		g.setTransform(camTrans)
		
		//testPaint(g)
		treeSprite.render(g)
		
		// restore the original transform
		g.setTransform(origTrans)
		
		// display HUD information
		g.setColor(new Color(0x000000))
		g.drawString("" + timer.fpsCounter, 10,32)
	}
	
	
	/** 
	 * A paint method for displaying a loading animation while the application is loading something (such as a new TreeSprite).
	 * @param g		The graphics context of the component this is being painted to. In this case, it's the TreeVisPanel's context.
	 */
	
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
	
	/**
	 * Selects a node in the current tree visualization, 
	 * does some fancy camera work to make sure that that node stays in the same place onscreen
	 * after the graph is re-expanded, and displays that node's information in the atom properties panel.
	 */
	
	def selectNode(clickedNode : NodeSprite) : Unit = {
		if(clickedNode != null)	{
			
			val clickedNodeScreenPos = camera.worldToScreenCoords(clickedNode.getWorldPosition)
			camera.moveCenter(clickedNodeScreenPos)
			treeSprite.selectNode(clickedNode, decompDepth)
			mainGUI.propsPanel.textArea.text = clickedNode.properties
			
			camera.x = clickedNode.worldX
			camera.y = clickedNode.worldY
		}
	}
	
	/** 
	 * Performs one iteration through the visualization panel's logic. 
	 * It's mostly just processing mouse input for controlling the camera and selecting nodes.
	 */
	
	def timerLoop : Unit = {

		keyboardIn.poll
		
		/*
		if(keyboardIn.justAnyPressed) println("just pressed " + keyboardIn.lastKeyPressed)
		if(keyboardIn.isAnyPressed) println("pressed " + keyboardIn.lastKeyPressed)
		if(keyboardIn.justAnyTyped) println("just released " + keyboardIn.lastKeyPressed)
		*/
		
		// handle mouse input.
		
		mouseIn.poll
		
		if(mouseIn.justLeftPressed) {
			val clickedNode = treeSprite.detectMouseOver(mouseWorldPosition)
			selectNode(clickedNode)
			camera.startDrag(mouseIn.position)
		}
		if(mouseIn.isLeftPressed) {
			camera.drag(mouseIn.position)
			camera.startDrag(mouseIn.position)
		}
		if(mouseIn.wheel == -1)
			camera.zoomAtScreen(1.25, mouseIn.position)
		if(mouseIn.wheel == 1)
			camera.zoomAtScreen(0.8, mouseIn.position)
		
		// update the camera's transform based on its new state.
		
		camera.updateTransform
		
		// update the mouse's current world coordinates
		
		mouseWorldPosition = camera.screenToWorldCoords(mouseIn.position)
		
	}
	
	// once the panel is set up, start the timer.
	// the timer will call timerLoop and repaint periodically
	
	start()
}





/** Used to display the properties of the currently selected node. */

class PropertiesPanel extends BoxPanel(Orientation.Vertical) {
	background = mainGUI.bgColor
	val inset = 3
	border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
	
	val label = new Label("Atom properties: ")
	label.border = new javax.swing.border.EmptyBorder(0,0,inset,0)
	contents += label
	
	/** The TextArea that displays the currently selected node's properties */
	val textArea = new TextArea("",20,50) {
		wordWrap = true
		lineWrap = true
		editable = false
		border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
		font = new java.awt.Font("Lucida Console", java.awt.Font.PLAIN, 12 )
		focusable = false
	}
	
	/** The scrolling pane that contains textArea */
	val taPanel = new ScrollPane {
		contents = textArea
		horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never
	}
	contents += taPanel
}






import scala.actors.Actor

/** The Actor object used to receive and process communications from the REPL */

object GUIActor extends Actor {
	def act() = {
		loop {
			react {
				case root : ornl.elision.core.RWTreeNode => {
					// The actor reacts to RWTreeNodes by constructing a tree visualization of it in the TreeVisPanel.
					
					mainGUI.treeVisPanel.isLoading = true
					Thread.sleep(100)
					mainGUI.treeVisPanel.treeSprite = TreeSprite.buildRWTree(root)
					
					// once the tree visualization is built, select its root node and center the camera on it.
					
					mainGUI.treeVisPanel.selectNode(mainGUI.treeVisPanel.treeSprite.root)
					mainGUI.treeVisPanel.camera.reset
					
					mainGUI.treeVisPanel.isLoading = false
				}
				case selFile : java.io.File => {
					// The actor reacts to a File by passing the file's contents to the REPL to be processed as input.
					mainGUI.treeVisPanel.isLoading = true
					Thread.sleep(100)
					
					// here we accumulate the text of the file into one big string.
					
					var str : String = ""
					val br = new BufferedReader(new FileReader(selFile))
					while(br.ready) {
						str += br.readLine + "\n"
					}
					br.close
					
					// now we send the accumulated string to the REPL's actor so that the REPL will process it as input.
					println("Reading REPL input from file: " + selFile.getPath)
					println()
					ornl.elision.repl.ReplActor ! str
				}
				case "quit" => 
					System.exit(0)
				case _ => // discard anything else that comes into the mailbox.
			}
		}
	}
}






