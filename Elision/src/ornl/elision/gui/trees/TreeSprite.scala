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
class TreeSprite(x : Double, y : Double) extends Sprite(x,y) {
  
  /** reference to root node. */
  var root : NodeSprite = null
  
  /** The node currently selected in the tree */
  var selectedNode : NodeSprite = null
  
  /** The base x-offset for child nodes from their parent node */
  val defX = 50
  
  /** The minimum y-offset between two sibling nodes */
  val defY = 40
  
  /** A constant-width font used in the NodeSprites' labels. */
  val font = new Font("Lucida Console", java.awt.Font.PLAIN, 12)
  
  /** black, just black. */
  val textColor = new Color(0x000000)
  
  /** comment node color: Twilight lavender */
  val comBoxColor = new Color(0xddddff)
  val comBorderColor = new Color(0x5555aa)
  
  /** expandable node colors: Dash blue */
  val boxColor = new Color(0xd7e9ff)
  val borderColor = new Color(0x77a9dd)
  
  /** debug node colors: Apple orange */
  val debugBoxColor = new Color(0xffeecc)
  val debugBorderColor = new Color(0xddaa77)
  
  /** selected node colors : Flutter yellow */
  val selectedBoxColor = new Color(0xffffcc)
  val selectedBorderColor = new Color(0xaaaa55) 
  
  /** leaf node colors: Rare grey */
  val leafBoxColor = new Color(0xf8f8ff) // leaves don't have a border color. They use the border color of their actual type.
  val selectedLeafBoxColor = new Color(0xffffee)
  
  /** The maximum length of a line of text in a node's label. */
  val maxTermLength = 50
  
  /** syntax formatter (default one only does line-wrapping). */
  val formatter = new ornl.elision.syntax.SyntaxFormatter()
  
  /** If false, syntax coloring is disabled for the entire tree. */
  var syntaxColoring = false
  
  /** A reference to a Camera object. This is used by NodeSprite only for clipping since the camera's transform is already applied before calling the NodeSprite's render method. */
  var camera : sage2D.Camera = null
  
  
  
  /** Begins recursive rendering of the tree beginning at the root. */
  override def draw(g : Graphics2D) : Unit = {
    root.render(g)
  }
  
  
  /** 
   * Creates (or replaces) the root node for this tree. 
   * @param term      The root's label
   * @param isComment If true, the root's label won't use syntax coloring.
   * @return          The root node.
   */
  def makeRoot(term : String = "Root", isComment : Boolean = true) : NodeSprite = {
    root = new NodeSprite(term, this, null, isComment)
    root
  }
  
  
  /**  
   * Detects collisions between the mouse and the tree's decompressed nodes.
   * @param mouseWorld  The mouse's position in world coordinates.
   * @return  If the mouse is over a node, that node is returned. Otherwise null is returned.
   */
  def detectMouseOver(mouseWorld : geom.Point2D) : NodeSprite = {
    detectMouseOverRec(mouseWorld, root)
  }
  
  /**  
   * Recursively detects collisions between the mouse and the tree's decompressed nodes.
   * @param mouseWorld  The mouse's position in world coordinates.
   * @param node    The root of the current subtree.
   * @return  If the mouse is over a node, that node is returned. Otherwise null is returned.
   */
  private def detectMouseOverRec(mouseWorld : geom.Point2D, node : NodeSprite) : NodeSprite = {
    
    // check if the mouse is overlapping this node.
    if(node.getCollisionBox.contains(mouseWorld.getX, mouseWorld.getY)) {
      node
    }
    else {
      // recursively check if the mouse is overlapping one of its children.
      for(i <- 0 to node.children.size - 1) {
        val child = node.children(i)
        
        // check children that match our heuristics.
        if(!child.isCompressed && mouseWorld.getY >= child.subTreeUpperY && mouseWorld.getY <= child.subTreeLowerY) {
          val result = detectMouseOverRec(mouseWorld, child)
          if(result != null) {
            return result
          }
        } 
      }
      null
    }
  }
  

  /**  
   * Causes node to become selected. When a node is selected, its children nodes are decompressed
   * and the immediate children of its ancestor nodes are decompressed out to depth n. 
   * Non-immediate children of node and its ancestors are compressed.
   * @param node    The node being selected.
   * @param n      The decompression depth for expanding the tree relative to node.
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
    
    root.computeSubtreeMetrics
  }
  
  
  /**
   * Attempts to select a node located at the given mouse world coordinates. 
   * If the mouse is not over a node, nothing happens.
   * @param mouseWorld  The mouse's position in world coordinates.
   * @param n        The decompression depth for expanding the tree relative to node.
   */
  def selectNode(mouseWorld : geom.Point2D, n : Int) : Unit = {
    selectNode(detectMouseOver(mouseWorld),n)
  }
  
  
  /**
   * Recursively decompresses node and its children out to depth n.
   * @param node    the root of the current subtree being decompressed
   * @param n      the current depth we are recursively decompressing out to
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
   * @param node    This NodeSprite's children will be compressed.
   * @param skipped    Is the child of node that we just came up from in the 
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
}



