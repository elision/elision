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

import java.awt._
import sage2D.sprites.Sprite

/** 
 * Contrary to what the name suggests, this class is not some sort of mystical nature spirit.
 * Rather, it represents a tree data structure as a renderable sprite.
 */
class TreeSprite(x : Double, y : Double, var root : NodeSprite = null) extends Sprite(x,y) {
	
	/** The node currently selected in the tree */
	var selectedNode : NodeSprite = null
  
  /** A constant-width font used in the NodeSprites' labels. */
  val font = new Font("Lucida Console", java.awt.Font.PLAIN, 12)
  
  /** black, just black. */
  val textColor = new Color(0x000000)
  
  /** comment color: Twilight lavender */
  val comBoxColor = new Color(0xddddff)
  val comBorderColor = new Color(0x5555aa)
  
  /** rewritten atom colors: Dash blue */
  val boxColor = new Color(0xd7e9ff)
  val borderColor = new Color(0x77a9dd)
  
  /** verbatim atom colors: Apple orange */
  val verbBoxColor = new Color(0xffeecc)
  val verbBorderColor = new Color(0xddaa77)
  
  /** selected colors : Flutter yellow */
  val selectedBoxColor = new Color(0xffffcc)
  val selectedBorderColor = new Color(0xaaaa55) 
  
  /** leaf colors: Rare grey */
  val leafBoxColor = new Color(0xf8f8ff) // leaves don't have a border color. They use the border color of their actual type.
  val selectedLeafBoxColor = new Color(0xffffee)
  
  /** The maximum length of a line of text in a node's label. */
  val maxTermLength = 50
  
  /** A reference to a Camera object. This is used by NodeSprite only for clipping since the camera's transform is already applied before calling the NodeSprite's render method. */
  var camera : sage2D.Camera = null
  
  /** Elision syntax formatter. */
  val formatter = new ornl.elision.gui.syntax.SyntaxFormatter(ornl.elision.gui.elision.EliRegexes, true, true)
	
	/** Begins recursive rendering of the tree beginning at the root. */
	override def draw(g : Graphics2D) : Unit = {
		root.render(g)
	}
	
  
  /** 
   * Creates (or replaces) the root node for this tree. 
   * @param term      The root's label
   * @isComment       If true, the root's label won't use syntax coloring.
   * @return          The root node.
   */
  def makeRoot(term : String = "Root", isComment : Boolean = true) : NodeSprite = {
    root = new NodeSprite(term, this, null, isComment)
    root
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
        
        // check children that match our heuristics.
				if(!child.isCompressed && mouseWorld.getY >= child.subTreeUpperY && mouseWorld.getY <= child.subTreeLowerY) {
					val result = detectMouseOverRec(mouseWorld, child)
					if(result != null)
						return result
				} // endif
			} // endfor
			null
		} //endif
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
		_computeYOffsets(root)
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
    
		if (n <= 0) {
      compressChildrenOf(node,skipped)
    }
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
	 * @param skipped	  Is the child of node that we just came up from in the 
   *                  bottom-up select node method. We don't want to compress 
   *                  it or its children. It already compressed any of its 
   *                  children that needed to be compressed.
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
	 * Counts the number of leaf nodes among node's descendants.
   * Here, we define a leaf node as any decompressed node with either no 
   * children or whose children are all decompressed.
   * The number of leaves counted is returned and also stored inside each node
   * for convenient reuse.
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
		
		node.numLeaves = math.max(node.numLeaves, node.excessHeight/(font.getSize+5) + 0.5).toInt 
		node.numLeaves
	}
	
	
	/**
	 * Recursively determines what the correct y-0ffsets for the nodes should be after decompression.
	 * These y-offsets will help the tree to look nice when expanded.
	 * They are stored internally in the tree's NodeSprites.
	 * @param node		The node for whose children we are currently computing the y-offsets for.
	 */
	def _computeYOffsets(node : NodeSprite) : (Double, Double) = {
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
		
		// compute the y coordinates of the area bounding node's subtree. 
    // This will be used by the detectMouseOver method 
		// for efficient mouse collision detection with the tree's nodes.
		node.subTreeUpperY = node.worldY + node.box.y
		node.subTreeLowerY = node.worldY + node.box.y + node.box.height
		
		// iterate over all of this node's decompressed children and compute their y-offsets.
		for(child <- node.children if !child.isCompressed) {

			val childLeaves = math.max(child.numLeaves - 1,0)
			
			// compute the child's y-offset with magic
			child.offsetY = (childLeaves + lastSibsLeaves - nodeLeaves)*TreeSprite.defY/2
			
			// accumulate this child's leaves onto lastSibsLeaves.
			lastSibsLeaves += math.max(child.numLeaves - 1 , 0)*2
			
			// Now that we have the child's y-offset, compute its world coordinates and store them inside it.
			val childPos : geom.Point2D = node.getChildPosition(child.index)
			child.worldX = node.worldX + childPos.getX + node.box.width
			child.worldY = node.worldY + childPos.getY
			
			// recursively compute the child's children's y-offsets.
			val (subUpper, subLower) = _computeYOffsets(child)
			node.subTreeUpperY = math.min(node.subTreeUpperY, subUpper)
			node.subTreeLowerY = math.max(node.subTreeLowerY, subLower)
		} // endfor
    
    
		(node.subTreeUpperY, node.subTreeLowerY)
	}
}


/** 
 * Contains some static properties used by TreeSprite as well as factory 
 * methods for constructing a treesprite. 
 */
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
    val tree = new TreeSprite(0,0)
    
    val root = tree.makeRoot("root")
      root.makeChild("Welcome to the ") // addChild("Welcome to the ",realroot)
      root.makeChild("Elision Visualization Assistant (Eva)!") // addChild("Elision Visualization Assistant (Eva)!",realroot)
        root(1).makeChild("To create a rewrite tree visualization,") // addChild("To create a rewrite tree visualization,", root2)
        root(1).makeChild("simply do one of the following: ") // addChild("simply do one of the following: ",root2)
          root(1)(1).makeChild("Enter input into the ") // addChild("Enter input into the ", node2)
          root(1)(1).makeChild("onboard Elision REPL, below.") // addChild("onboard Elision REPL, below.", node2)
          root(1)(1).makeChild("OR") // addChild("OR",node2)
          root(1)(1).makeChild("Use File -> Open to open a file ") // addChild("Use File -> Open to open a file ",node2)
          root(1)(1).makeChild("containing Elision input.") // addChild("containing Elision input.", node2)

    tree
  }
	
	
	/**
	 * Used by some of the TreeSprite factory methods.
	 */
/*	private def addChild(term : String, parent : NodeSprite) : NodeSprite = {
		val node = new NodeSprite(term, parent)
		parent.addChild(node)
		node
	}
*/
}






