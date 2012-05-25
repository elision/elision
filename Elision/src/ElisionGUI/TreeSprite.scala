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

package ElisionGUI

import java.awt._
import sage2D._
import collection.mutable.ArrayBuffer


/** 
 * Contrary to what the name suggests, this class is not some sort of mystical nature spirit.
 * Rather, it represents the rewrite tree structure for an Elision term as a 
 * renderable sprite.
 */

class TreeSprite(x : Double, y : Double, val root : NodeSprite) extends Sprite(x,y) {
	
	/** The node currently selected in the tree */
	var selectedNode : NodeSprite = null
	
	/** Begins recursive rendering of the tree beginning at the root. */
	override def draw(g : Graphics2D) : Unit = {
		root.render(g)
	}
	
	
	/**	
	 * Detects collisions between the mouse and the tree's decompressed nodes.
	 * @param mouseWorld	The mouse's position in world coordinates.
	 * @return	If the mouse is over a node, that node is returned. Otherwise null is returned.
	 */
	def detectMouseOver(mouseWorld : geom.Point2D) : NodeSprite = {
		detectMouseOverRec(mouseWorld, root)
	}
	
	/**	
	 * Recursively detects collisions between the mouse and the tree's decompressed nodes.
	 * @param mouseWorld	The mouse's position in world coordinates.
	 * @param node		The root of the current subtree.
	 * @return	If the mouse is over a node, that node is returned. Otherwise null is returned.
	 */
	private def detectMouseOverRec(mouseWorld : geom.Point2D, node : NodeSprite) : NodeSprite = {
		// check if the mouse is overlapping this node.
		
		if(node.getCollisionBox.contains(mouseWorld.getX, mouseWorld.getY))
			node
		else {
			// recursively check if the mouse is overlapping one of its children.
			
			for(i <- 0 to node.children.size - 1) {
				val child = node.children(i)
				if(!child.isCompressed && mouseWorld.getY >= child.subTreeUpperY && mouseWorld.getY <= child.subTreeLowerY) {
					val result = detectMouseOverRec(mouseWorld, child)
					if(result != null)
						return result
				}
			}
			null
		}
	}
	

	/**	
	 * Causes node to become selected. When a node is selected, its children nodes are decompressed
	 * and the immediate children of its ancestor nodes are decompressed out to depth n. 
	 * Non-immediate children of node and its ancestors are compressed.
	 * @param node		The node being selected.
	 * @param n			The decompression depth for expanding the tree relative to node.
	 */
	
	def selectNode(node : NodeSprite, n : Int = 1) : Unit = {
		
		// don't allow leaf nodes to be selected
		
		if(node == null)
			return
		
		// mark node as selected
		
		if(selectedNode != null) selectedNode.isSelected = false
		node.isSelected = true
		selectedNode = node
		
		// do the n-depth decompression starting at node and working up to root.
		
		var ancestor : NodeSprite = node
		var skipped : NodeSprite = null
		
		while(ancestor != null) {
			nDepthDecompress(ancestor, skipped, n)
			skipped = ancestor
			ancestor = ancestor.parent
		}
		
		countLeaves(root)
		computeYOffsets(root)
	}
	
	
	/**
	 * Attempts to select a node located at the given mouse world coordinates. 
	 * If the mouse is not over a node, nothing happens.
	 * @param mouseWorld	The mouse's position in world coordinates.
	 * @param n				The decompression depth for expanding the tree relative to node.
	 */
	
	def selectNode(mouseWorld : geom.Point2D, n : Int) : Unit = {
		selectNode(detectMouseOver(mouseWorld),n)
	}
	
	
	/**
	 * Recursively decompresses node and its children out to depth n.
	 * @param node		the root of the current subtree being decompressed
	 * @param n			the current depth we are recursively decompressing out to
	 */
	
	private def nDepthDecompress(node : NodeSprite, skipped : NodeSprite, n : Int) : Unit = {
		if(node.isCompressed) {
			node.isCompressed = false
		}
		if (n <= 0) compressChildrenOf(node,skipped)
		else {
			if(node.expansion < 0.01) node.expansion = 0.01
			
			for(child <- node.children) {
				if(child != skipped)
					nDepthDecompress(child, skipped, n-1)
			}
		}
	}
	
	
	/**
	 * Compresses all the children (if any) of node via depth-first search. 
	 * It prunes if it encounters a child that is already compressed. 
	 * @param node		This NodeSprite's children will be compressed.
	 * @param skipped	Is the child of node that we just came up from in the bottom-up select node method. 
	 *					We don't want to compress it or its children. It already compressed any of its children that needed to be compressed.
	 */
	
	private def compressChildrenOf(node : NodeSprite, skipped : NodeSprite = null) : Unit = {
		import collection.mutable.Stack
		
		val stack = new Stack[NodeSprite]
		
		for(child <- node.children if (!child.isCompressed && child != skipped)) {
			stack.push(child)
		}
		
		while(!stack.isEmpty) {
			val child = stack.pop
			child.isCompressed = true
			for(grandChild <- child.children if (!grandChild.isCompressed && grandChild != skipped)) {
				stack.push(grandChild)
			}
		}
	}
	
	
	/**
	 * Recursively counts the number of leaf nodes among node's descendants.
	 * A node is counted as a leaf node as far as this method is concerned if it is decompressed 
	 * and either it has no children or all its children are compressed.
	 * This method recursively computes the number of leaves for all decompressed nodes in the tree and stores them 
	 * inside the nodes for easy reuse.
	 * @param node		The node whose leaf descendants we are currently counting
	 * @return			The number of node's leaf descendants
	 */
	
	def countLeaves(node : NodeSprite) : Int = {
		node.numLeaves = 0
		
		var numDecompChildren = 0
		
		for(child <- node.children if !child.isCompressed) {
			numDecompChildren += 1
			val childsLeaves = countLeaves(child)
			node.numLeaves += math.max(1,childsLeaves)
		}
		
		node.numLeaves = math.max(node.numLeaves, node.excessHeight/(NodeSprite.font.getSize+5) + 0.5).toInt 
		
	/*	if(numDecompChildren == 0)
			node.numLeaves = math.max(node.numLeaves, node.excessHeight/(NodeSprite.font.getSize+5)) //node.excessHeight/(NodeSprite.font.getSize+5) // 0
		else
			math.max(node.numLeaves, node.excessHeight/(NodeSprite.font.getSize+5)) // node.numLeaves
		*/
		node.numLeaves
	}
	
	
	/**
	 * Recursively determines what the correct y-0ffsets for the nodes should be after decompression.
	 * These y-offsets will help the tree to look nice when expanded.
	 * They are stored internally in the tree's NodeSprites.
	 * @param node		The node for whose children we are currently computing the y-offsets for.
	 */
	
	def computeYOffsets(node : NodeSprite) : (Double, Double) = {
		// The number of leaves of previous sibling nodes will be used to determine what node's current child
		// node's y-offset should be. The first child has no siblings before it. Therefore initialize this to 0.
		var lastSibsLeaves = 0
		
		var firstChild = true
		
		// the root node has no y-offset. Just set its world coordinates to that of the tree.
		
		if(node == root) {
			node.worldX = this.x
			node.worldY = this.y
		}
		
		val nodeLeaves = math.max(node.numLeaves - 1, 0)
		
		// compute the y coordinates of the area bounding node's subtree. This will be used by the detectMouseOver method 
		// for efficient mouse collision detection with the tree's nodes.
		
		node.subTreeUpperY = node.worldY + node.box.y // node.worldY - (nodeLeaves*TreeSprite.defY + node.box.height)/2 //
		node.subTreeLowerY = node.worldY + node.box.y + node.box.height // node.worldY + (nodeLeaves*TreeSprite.defY + node.box.height)/2 //
		
		// iterate over all of this node's decompressed children and compute their y-offsets.
		
		for(child <- node.children if !child.isCompressed) {

			val childLeaves = math.max(child.numLeaves - 1,0)
			
			// compute the child's y-offset by using magic
			child.offsetY = (childLeaves + lastSibsLeaves - nodeLeaves)*TreeSprite.defY/2
			
			// accumulate this child's leaves onto lastSibsLeaves.
			lastSibsLeaves += math.max(child.numLeaves - 1 , 0)*2
			
			// Now that we have the child's y-offset, compute its world coordinates and store them inside it.
			
			val childPos : geom.Point2D = node.getChildPosition(child.index)
			child.worldX = node.worldX + childPos.getX + node.box.width
			child.worldY = node.worldY + childPos.getY
			
			// recursively compute the child's children's y-offsets.
			
			val (subUpper, subLower) = computeYOffsets(child)
			node.subTreeUpperY = math.min(node.subTreeUpperY, subUpper)
			node.subTreeLowerY = math.max(node.subTreeLowerY, subLower)
		}
		(node.subTreeUpperY, node.subTreeLowerY)
	}
	
	
}


/** Contains some static properties used by TreeSprite as well as factory methods for constructing a treesprite. */

object TreeSprite {
	
	/** The base x-offset for child nodes from their parent node */
	val defX = 50
	
	/** The minimum y-offset between two sibling nodes */
	val defY = 40
	
	
	/**
	 * Factory method builds a fabricated tree structure with a friendly 
	 * welcome message and some basic instructions.
	 */
	 
	 def buildWelcomeTree : TreeSprite = {
		val realroot = new NodeSprite("root")
			val root0 = addChild("Welcome to the ",realroot)
			val root = addChild("Elision Visualization Assistant (Eva)!",realroot)
				val node1 = addChild("To create a rewrite tree visualization,", root)
				val node2 = addChild("simply do one of the following: ",root)
					addChild("Enter input into the ", node2)
					addChild("onboard Elision REPL, below.", node2)
					addChild("OR",node2)
					addChild("Use File -> Open to open a file ",node2)
					addChild("containing Elision input.", node2)
		
		new TreeSprite(0,0,realroot)
	 }
	 
	
	/** 
	 * Factory method builds a fabricated tree structure for testing purposes. 
	 * The Touhou Project and its characters are copyright (c) ZUN, Team Shanghai Alice.
	 */
	
	/*
	def buildTouhouTree : TreeSprite = {
		val root = new NodeSprite("Touhou Characters")
		var parent : NodeSprite = root
		
		val ts = new collection.mutable.Stack[NodeSprite]
		
		ts.push(parent)
		parent = addChild("Main Characters", parent)
			addChild("Reimu Hakurei",parent)
			addChild("Marisa Kirisame",parent)
			addChild("Sakuya Izayoi",parent)
			addChild("Youmu Konpaku",parent)
			addChild("Sanae",parent)
		parent = ts.pop
		
		ts.push(parent)
		parent = addChild("Embodiment of the Scarlet Devil",parent)
			ts.push(parent)
			parent = addChild("Stage 1",parent)
				ts.push(parent)
				parent = addChild("Rumia",parent)
					addChild("Youkai of darkness",parent)
					addChild("Power: Control of darkness",parent)
				parent = ts.pop
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Stage 2",parent)
				addChild("Daiyousei",parent)
				
				ts.push(parent)
				parent = addChild("Cirno",parent)
					addChild("Ice fairy of the lake",parent)
					addChild("Power: Manipulation of cold",parent)
					addChild("Power: Regeneration",parent)
				parent = ts.pop
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Stage 3",parent)
				ts.push(parent)
				parent = addChild("Hong Meiling",parent)
					addChild("Scarlet Mansion guard",parent)
					addChild("Weapon mastery: Martial arts master",parent)
				parent = ts.pop
			parent = ts.pop
		
			ts.push(parent)
			parent = addChild("Stage 4",parent)
				addChild("Koakuma",parent)
				
				ts.push(parent)
				parent = addChild("Patchouli Knowledge",parent)
					addChild("Scarlet Mansion librarian",parent)
					addChild("Power: 7-Elemental wizard",parent)
				parent = ts.pop
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Stage 5",parent)
				ts.push(parent)
				parent = addChild("Sakuya Izayoi",parent)
					addChild("Maid of the Scarlet Mansion",parent)
					addChild("Power: Time control",parent)
					addChild("Weapon mastery: Knives",parent)
				parent = ts.pop
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Stage 6",parent)
				addChild("Sakuya Izayoi",parent)
				
				ts.push(parent)
				parent = addChild("Remilia Scarlet",parent)
					addChild("Scarlet devil of the Mansion",parent)
					addChild("Power: Manipulation of fate",parent)
				parent = ts.pop
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Extra Stage",parent)
				addChild("Patchouli Knowledge",parent)
				
				ts.push(parent)
				parent = addChild("Flandre Scarlet",parent)
					addChild("Lunatic sister of the scarlet devil",parent)
					addChild("Power: Destruction of anything",parent)
				parent = ts.pop
			parent = ts.pop
		parent = ts.pop
		
		
		
		ts.push(parent)
		parent = addChild("Perfect Cherry Blossom",parent)
			ts.push(parent)
			parent = addChild("Stage 1",parent)
				ts.push(parent)
				parent = addChild("Cirno",parent)
					addChild("the STRONGEST",parent)
				parent = ts.pop
				
				ts.push(parent)
				parent = addChild("Letty Whiterock",parent)
					addChild("Youkai of Winter",parent)
					addChild("Power: Manipulation of Cold",parent)
				parent = ts.pop
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Stage 2",parent)
				ts.push(parent)
				parent = addChild("Chen",parent)
					addChild("Nekomata of evil omens",parent)
					addChild("Power: Dark wizardry",parent)
				parent = ts.pop
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Stage 3",parent)
				ts.push(parent)
				parent = addChild("Alice Margatroid",parent)
					addChild("Magician doll-maker",parent)
					addChild("Power: Animation of magic dolls",parent)
				parent = ts.pop
			parent = ts.pop
		
			ts.push(parent)
			parent = addChild("Stage 4",parent)
				addChild("Lily White",parent)
				
				ts.push(parent)
				parent = addChild("Lunasa Prismriver",parent)
					addChild("Musical poltergeist",parent)
					addChild("Power: Ghost viola",parent)
				parent = ts.pop
				
				ts.push(parent)
				parent = addChild("Merlin Prismriver",parent)
					addChild("Musical poltergeist",parent)
					addChild("Power: Ghost saxophone",parent)
				parent = ts.pop
				
				ts.push(parent)
				parent = addChild("Lyrica Prismriver",parent)
					addChild("Musical poltergeist",parent)
					addChild("Power: Ghost keyboard",parent)
				parent = ts.pop
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Stage 5",parent)
				ts.push(parent)
				parent = addChild("Youmu Konpaku",parent)
					addChild("Half-ghost gardener of the Netherworld",parent)
					addChild("Power: Ghost samurai powers",parent)
					addChild("Weapon mastery: Katana",parent)
				parent = ts.pop
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Stage 6",parent)
				addChild("Youmu Konpaku",parent)
				
				ts.push(parent)
				parent = addChild("Yuyuko",parent)
					addChild("Ghost princess of the Netherworld",parent)
					addChild("Power: Control of ghosts and death",parent)
				parent = ts.pop
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Extra Stage",parent)
				addChild("Chen",parent)
				
				ts.push(parent)
				parent = addChild("Ran",parent)
					addChild("Nine-tailed fox shikigami",parent)
					addChild("Power: Dark wizardry",parent)
				parent = ts.pop
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Phantasm Stage",parent)
				addChild("Ran",parent)
				addChild("Chen",parent)
				
				ts.push(parent)
				parent = addChild("Yukari",parent)
					addChild("Youkai of the Great Boundary",parent)
					addChild("Power: Planeshifting",parent)
					addChild("Power: Lovecraftian Elder One powers",parent)
				parent = ts.pop
			parent = ts.pop
		parent = ts.pop
		
		
		
		ts.push(parent)
		parent = addChild("Imperishable Night",parent)
			ts.push(parent)
			parent = addChild("Stage 1",parent)				
				ts.push(parent)
				parent = addChild("Wriggle Nightbug",parent)
					addChild("Firefly youkai",parent)
					addChild("Power: Command over insects",parent)
				parent = ts.pop
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Stage 2",parent)
				ts.push(parent)
				parent = addChild("Mystsia Lorelei",parent)
					addChild("Nightsparrow youkai",parent)
					addChild("Power: Nightvision theft",parent)
					addChild("Power: Sparrow's song",parent)
				parent = ts.pop
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Stage 3",parent)
				ts.push(parent)
				parent = addChild("Keine",parent)
					addChild("Were-hakutaku scholar",parent)
					addChild("Power: Seal away history",parent)
				parent = ts.pop
			parent = ts.pop
		
			ts.push(parent)
			parent = addChild("Stage 4",parent)
				ts.push(parent)
				parent = addChild("Reimu Hakurei",parent)
					addChild("Shrine maiden of paradise",parent)
					addChild("Power: Manipulation of auras",parent)
					addChild("Power: Shrine maiden exorcism powers",parent)
					addChild("Weapon mastery: Hakurei yin-yang orbs",parent)
				parent = ts.pop
				
				ts.push(parent)
				parent = addChild("Marisa Kirisame",parent)
					addChild("ordinary magician/treasure hunter",parent)
					addChild("Power: Witch magic",parent)
					addChild("Power: Master spark",parent)
				parent = ts.pop
				
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Stage 5",parent)
				addChild("Tewi Inaba",parent)
				ts.push(parent)
				parent = addChild("Reisen Inaba",parent)
					addChild("Moon rabbit",parent)
					addChild("Power: Manipulation of electromagnetic waves",parent)
					addChild("Power: Lunatic gaze",parent)
					addChild("Weapon mastery: Firearms",parent)
				parent = ts.pop
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Stage 6",parent)
				ts.push(parent)
				parent = addChild("Eirin",parent)
					addChild("Medical genius",parent)
					addChild("Power: Astronomical medicine/poisons",parent)
					addChild("Weapon mastery: bow and arrow",parent)
				parent = ts.pop
				ts.push(parent)
				parent = addChild("Kaguya",parent)
					addChild("Exiled moon princess",parent)
					addChild("Power: immortality",parent)
					addChild("Power: regeneration",parent)
					addChild("Power: \"The 5 impossible requests\"",parent)
				parent = ts.pop
			parent = ts.pop
			
			ts.push(parent)
			parent = addChild("Extra Stage",parent)
				addChild("Keine",parent)
				
				ts.push(parent)
				parent = addChild("Fujiwara no Mokou",parent)
					addChild("Kaguya's formerly human nemesis",parent)
					addChild("Power: immortality",parent)
					addChild("Power: regeneration",parent)
					addChild("Power: Phoenix powers",parent)
				parent = ts.pop
			parent = ts.pop
			
			
		
		parent = ts.pop
		
			
		
		
		new TreeSprite(0,0,root)
	}
	*/
	
	/**
	 * Used by some of the TreeSprite factory methods.
	 */
	
	private def addChild(term : String, parent : NodeSprite) : NodeSprite = {
		val node = new NodeSprite(term, parent)
		parent.addChild(node)
		node
	}
	
	/**
	 * Constructs a TreeSprite from an Elision rewrite tree structure. 
	 * @param rwRoot	The root node of the rewrite tree to be constructed as a TreeSprite.
	 * @return			The completed TreeSprite constructed from rwRoot
	 */
	
	def buildRWTree(rwRoot : ornl.elision.core.RWTreeNode) : TreeSprite = {
		val root = new NodeSprite(rwRoot.term)
		root.properties = rwRoot.properties
		
		for(child <- rwRoot.children) {
			buildRWTreeRec(child, root)
		}
		
		val tree = new TreeSprite(0,0,root)
		tree.selectNode(root,2)
		tree
	}
	
	/**
	 * Used by the buildRWTree method to recursively build a TreeSprite. 
	 * @param rwRoot	The root node of the current rewrite subtree.
	 * @param parent	rwRoot's immediate parent.
	 */
	 
	private def buildRWTreeRec(rwNode : ornl.elision.core.RWTreeNode, parent : NodeSprite) : Unit = {
		val node = new NodeSprite(rwNode.term, parent)
		node.properties = rwNode.properties
		parent.addChild(node)
		
		node.isComment = rwNode.isComment
		node.isStringAtom = rwNode.isStringAtom
		
		for(child <- rwNode.children) {
			buildRWTreeRec(child, node)
		}
	}
	
	
}


/**
 * A sprite used to represent a rewritten term node in a TreeSprite.
 * The x,y coordinates for NodeSprites are relative to their parent's x,y coordinates. In the case 
 * of a TreeSprite's root node, this should be 0,0.
 * @param term		A string to be used as this node's label. This will be the parse string of the atom this node represents.
 * @param parent	This node's parent NodeSprite.
 */

class NodeSprite(var term : String = "Unnamed Node", val parent : NodeSprite = null) extends Sprite(0,0) {
	
	import java.awt.geom.RoundRectangle2D
	import java.awt.geom.Rectangle2D
	
	/** This node's collection of children */
	val children = new ArrayBuffer[NodeSprite]
	
	/** This node's index in its parent's list of children */
	var index : Int = 0
	
	/** This node's y-offset in the tree visualization */
	var offsetY : Double = 0
	
	/** This node's current number of leaf nodes (any decompressed descendant node that either has no children or whose children are compressed counts as a leaf node) */
	var numLeaves : Int = 0
	
	/** The upper y-boundary of this node's subtree in world coordinates. Used for efficient mouse collisions with the tree's nodes. */
	var subTreeUpperY : Double = 0
	
	/** The lower y-boundary of this node's subtree in world coordinates. Used for efficient mouse collisions with the tree's nodes. */
	var subTreeLowerY : Double = 0
	
	/** This node's x-position in world coordinates */
	var worldX = 0.0
	
	/** This node's y-position in world coordinates */
	var worldY = 0.0
	
	/** flag indicates whether this node is selected. */
	var isSelected : Boolean = false
	
	/** flag indicates if this node is compressed. */
	var isCompressed : Boolean = false
	
	/** A parametric variable used to implement a smooth compression/decompression animation for the nodes. */
	var expansion : Double = 0.1 // used for a smooth decompression animation
	
	
	// if the term is very long, separate it into multiple lines.
	private var edibleTerm = "" + term
	
	val termLines = new ArrayBuffer[String]
	while(edibleTerm.length > NodeSprite.maxTermLength) {
		val (str1, str2) = edibleTerm.splitAt(NodeSprite.maxTermLength)
		termLines += str1
		edibleTerm = str2
	}
	termLines += edibleTerm
	
	/** The node's width */
	private val boxWidth = termLines(0).length * NodeSprite.font.getSize * 0.66 + 5
	
	/** The node's height */
	private val boxHeight = (NodeSprite.font.getSize+5)*termLines.size // NodeSprite.font.getSize + 5
	
	/** The node's renderable box shape. */
	val box = new RoundRectangle2D.Double(0, 0-boxHeight/2, boxWidth, boxHeight, 5, 5)
	
	/** The node's properties describing the Elision atom it represents */
	var properties : String = ""
	
	/** flag indicates that this node is just a documentation string and doesn't actually represent an atom */
	var isComment = true
	
	/** flag indicates that this node represents a StringLiteral atom. */
	var isStringAtom = false
	
	//////////////////// Rendering methods
	
	
	/**
	 * Draws this node and recursively calls its decompressed children to be rendered.
	 * @param g		The graphics context this node is being rendered on.
	 */
	
	override def draw(g : Graphics2D) : Unit = {

		// obtain the dimensions of this node's term label.
		
		val origFont = g.getFont	// store the graphics context's original font so we can restore it when we're done.
		val origTrans = g.getTransform
		g.setFont(NodeSprite.font)
		
		// draw the box
		
		g.setColor(alphaColor(
			if(!children.isEmpty) {
				if(this.isSelected)
					NodeSprite.selectedBoxColor
				else if(this.isComment)
					NodeSprite.comBoxColor
			//	else if(this.isStringAtom)
			//		NodeSprite.verbBoxColor
				else
					NodeSprite.boxColor
			} 
			else {
				if(this.isSelected)
					NodeSprite.selectedLeafBoxColor
				else
					NodeSprite.leafBoxColor
			}
		))
		
		val startPt = g.getTransform.transform(new geom.Point2D.Double(0,box.y), null)
		val endPt = g.getTransform.transform(new geom.Point2D.Double(box.width,box.y+box.height), null)
		val isOnScreen = (startPt.getX <= NodeSprite.camera.pWidth && endPt.getX >= 0 && startPt.getY <= NodeSprite.camera.pHeight && endPt.getY >= 0)
		
		if(isOnScreen) g.fill(box)
		
		if(this.isSelected)
			g.setColor(NodeSprite.selectedBorderColor)
		else if(this.isComment)
			g.setColor(NodeSprite.comBorderColor)
	//	else if(this.isStringAtom)
	//		g.setColor(NodeSprite.verbBorderColor)
		else 
			g.setColor(NodeSprite.borderColor)

		if(isOnScreen) g.draw(box)
		
		// draw the label
		
		g.setColor(alphaColor(NodeSprite.textColor))
		if(isOnScreen && NodeSprite.camera.zoom > 0.2) { 
			for(i <- 0 until termLines.size) 
				g.drawString(termLines(i), 3, (box.y - 3 + (NodeSprite.font.getSize + 3)*(i+1)).toInt)
		}
		
		// restore the graphics context's font
		
		g.setFont(origFont)

		// draw the edges
		
		g.translate(boxWidth,0)
		drawEdges(g)
		
		// recursively render the child nodes.
		
		val origTrans2 = g.getTransform
		
		for(i <- 0 to children.size - 1) {
			val childPos = getChildPosition(i)
			val child = children(i)
			
			g.scale(child.expansion,child.expansion)	
			g.translate(childPos.getX, childPos.getY)
			
			if(child.expansion > 0.01) child.render(g)
			
			g.setTransform(origTrans2)
			
			// gradually compress/decompress the node
		
			if(child.isCompressed)
				child.expansion += (0.0 - child.expansion)/10.0
			else 
				child.expansion += (1.0 - child.expansion)/10.0
		}
		
		// restore the original transform
		
		g.setTransform(origTrans)
	}
	
	
	
	
	/**
	 * Computes an ARGB Color from an RGB color using the NodeSprite's alphaMaster value.
	 * @param color		The color we are applying this sprite's alphaMaster to.
	 * @return		A new Color with RGB identical to color's RGB, but using alphaMaster as its alpha color.
	 */
	
	private def alphaColor(color : Color) : Color = {
		new Color(color.getRed, color.getGreen, color.getBlue, (255 * alphaMaster).toInt)
	}
	
	
	
	/**
	 * Draws the edges of this node to decompressed children.
	 * @param g		The graphics context the edges are being rendered to.
	 */
	
	private def drawEdges(g : Graphics2D) : Unit = {
		import java.awt.geom.CubicCurve2D
		import java.awt.geom.Rectangle2D
		
		// store the original transform
		val origTrans = g.getTransform
		
		// use the unselected border color as the color for the edges
		g.setColor(alphaColor(NodeSprite.comBorderColor))
		
		// iterate over the children and obtain their relative positions to determine how to draw the edges.
		for(i <- 0 to children.size - 1) {
			val child = children(i)
			val childPos = getChildPosition(i)
			
			// decide upon the edge's end coordinates and control point coordinates
			val endX = childPos.getX
			val endY = childPos.getY
			val ctrlX = math.max(endX/2,endX - 50)
			
			// scale the current graphics transform according to how compressed/decompressed this node's children are.
			g.scale(child.expansion,child.expansion)
			val startPt = g.getTransform.transform(new geom.Point2D.Double(0,0), null)
			val endPt = g.getTransform.transform(new geom.Point2D.Double(endX,0), null)
			
			if(startPt.getX <= NodeSprite.camera.pWidth && endPt.getX >= 0) {
				// create the cubic curve shape for the edge. //  Then draw the edge.
				val edge = new CubicCurve2D.Double(0, 0, ctrlX, 0, ctrlX, endY, endX, endY)
				if(child.expansion > 0.01) g.draw(edge)
			}
			
			// restore the original transform for the next edge drawing.
			g.setTransform(origTrans)
		}
		
		// restore the original transform at the end.
		g.setTransform(origTrans)
	}
	
	
	
	
	////////// children processing/positioning methods
	
	
	/**
	 * Adds a child node to this NodeSprite.
	 * @param node		The child being added to this node.
	 */
	
	def addChild(node : NodeSprite) : Unit = {
		node.index = children.size
		children += node
	}
	
	
	/**
	 * Obtains the position of a child node relative to this node.
	 * @param index		the index of the child whose position we need.
	 * @return			the child node's position relative to its parent.
	 */
	
	def getChildPosition(index : Int) : geom.Point2D = {
		val longestSib = getLongestSibling
		val childX = longestSib - box.width + TreeSprite.defX + 5*numLeaves
		val childY = TreeSprite.defY*index + children(index).offsetY
		
		new geom.Point2D.Double(childX,childY)
	}

	
	/**
	 * Gets the width of the longest sibling's box.
	 * @return		the length of this node's parent's longest child.
	 */
	
	def getLongestSibling : Double = {
		var longest = 0.0
		
		if(parent == null)
			return boxWidth
			
		for(child <- parent.children) {
			if(child.boxWidth > longest)	longest = child.boxWidth
		}
		longest
	}
	
	
	/**
	 * Returns this node's world coordinates as a Point2D.Double object.
	 */
	
	
	def getWorldPosition : geom.Point2D = {
		new geom.Point2D.Double(worldX,worldY)
	}
	
	
	def excessHeight : Int = {
		boxHeight - (NodeSprite.font.getSize+5)
	}
	
	////////////	collision methods
	
	/**
	 *	Returns the rectangle representing this node's bounding box in world coordinates.
	 */
	
	override def getCollisionBox : Rectangle2D = {
		new Rectangle2D.Double(worldX + box.x, worldY + box.y, boxWidth, boxHeight)
	}
	
	
}


/** Contains static data used by NodeSprites */

object NodeSprite {
	val font = new Font("Lucida Console", java.awt.Font.PLAIN, 12)
	
	val textColor = new Color(0x000000)
	
	// comment color: Twilight lavender
	val comBoxColor = new Color(0xddddff)
	val comBorderColor = new Color(0x5555aa)
	
	// rewritten atom colors: Dash blue
	val boxColor = new Color(0xd7e9ff)
	val borderColor = new Color(0x77a9dd)
	
	// verbatim atom colors: Apple orange
	val verbBoxColor = new Color(0xffeecc)
	val verbBorderColor = new Color(0xddaa77)
	
	// selected colors : Flutter yellow
	val selectedBoxColor = new Color(0xffffcc)
	val selectedBorderColor = new Color(0xaaaa55) 
	
	// leaf colors: Rare grey
	val leafBoxColor = new Color(0xf8f8ff) // leaves don't have a border color. They use the border color of their actual type.
	val selectedLeafBoxColor = new Color(0xffffee)
	
	val maxTermLength = 50
	
	var camera : Camera = null
}



