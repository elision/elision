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

import java.awt.Color
import java.awt.Graphics2D
import java.awt.geom.CubicCurve2D
import java.awt.geom.Line2D
import java.awt.geom.Point2D
import java.awt.geom.Rectangle2D
import java.awt.geom.RoundRectangle2D
import sage2D.sprites.Sprite
import sage2D.Camera
import collection.mutable.ArrayBuffer

import scala.swing.Dialog

import ornl.elision.gui.mainGUI

/**
 * A sprite used to represent a rewritten term node in a TreeSprite.
 * The x,y coordinates for NodeSprites are relative to their parent's x,y 
 * coordinates. In the case of a TreeSprite's root node, this should be 0,0.
 * @param term    A string to be used as this node's label. This will be the 
 *                parse string of the atom this node represents.
 * @param tree    The TreeSprite this node belongs to.
 * @param parent  This node's parent NodeSprite.
 * @param isComment  Flag indicates that this node is just a documentation 
 *                   string and doesn't actually represent an atom.
 */
class NodeSprite(var term : String = "Unnamed Node", val tree : TreeSprite, val parent : NodeSprite = null, val isComment : Boolean = true) extends Sprite(0,0) {
  
  /** This node's collection of children */
  val children = new ArrayBuffer[NodeSprite]
  
  /** This node's index in its parent's list of children */
  var index : Int = 0
  
  /** This node's y-offset in the tree visualization */
  var offsetY : Double = 0
  
  /** 
   * This node's subtree's adjusted number of expanded leaf nodes. 
   * This isn't the actual number of leaf nodes and is really just used for
   * computing the layout of the nodes in the subtree.
   */
  var _adjustedLeafCount : Int = 0
  
  /**
   * The number of leaves in this node's currently expanded subtree.  
   */
  var numLeaves : Int = 0
  
  /** 
   * The upper y-boundary of this node's subtree in world coordinates. 
   * Used for efficient mouse collisions with the tree's nodes. 
   */
  var subTreeUpperY : Double = 0
  
  /** 
   * The lower y-boundary of this node's subtree in world coordinates. 
   * Used for efficient mouse collisions with the tree's nodes. 
   */
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
  var expansion : Double = 0.1
  
  /** data for syntax highlighting and formatting */
  val formattedString = tree.formatter.format(term, tree.maxTermLength)
  
  /** The node's width */
  val boxWidth = formattedString.width * tree.font.getSize * 0.66 + 5
  
  /** The node's height */
  val boxHeight = (tree.font.getSize+5)*formattedString.lines.size
  
  /** The node's renderable box shape. */
  val box = new RoundRectangle2D.Double(0, 0-boxHeight/2, boxWidth, boxHeight, 5, 5)
  
  /** The node's properties describing the object it represents */
  var properties : String = ""
  
  /** flag indicates that this node represents a StringLiteral atom. */
  var isStringAtom = false
  
  
  //////////////////// Rendering methods
  
  
  /**
   * Draws this node and recursively calls its decompressed children to be rendered.
   * @param g    The graphics context this node is being rendered on.
   */
  override def draw(g : Graphics2D) : Unit = {
    
    // store the graphics context's original state so we can restore it when we're done.
    val origFont = g.getFont  
    val origTrans = g.getTransform
    
    g.setFont(tree.font)
    
    // decide whether or not to skip drawing this node due to being offscreen.
    val startPt = g.getTransform.transform(new Point2D.Double(0,box.y), null)
    val endPt = g.getTransform.transform(new Point2D.Double(box.width,box.y+box.height), null)
    val isOnScreen = (tree.camera == null || (tree.camera.zoom > 0.01 && startPt.getX <= tree.camera.pWidth && endPt.getX >= 0 && startPt.getY <= tree.camera.pHeight && endPt.getY >= 0))
    
    if(isOnScreen) 
      drawThis(g)

    // draw the edges
    g.translate(boxWidth,0)
    drawEdges(g)
    
    // render the child nodes.
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
    } // endfor
    
    // restore the original graphics state
    g.setTransform(origTrans)
    g.setFont(origFont)
  }
  
  
  /** Draws this node. */
  def drawThis(g : Graphics2D) : Unit = {
    // Choose box's fill color
    g.setColor(alphaColor(
      if(!children.isEmpty) {
        if(this.isSelected)
          tree.selectedBoxColor
        else if(this.isComment)
          tree.comBoxColor
        else
          tree.boxColor
      } 
      else {
        if(this.isSelected)
          tree.selectedLeafBoxColor
        else
          tree.leafBoxColor
      }
    ))
    
    g.fill(box)
    
    // Choose box's border color.
    if(this.isSelected)
      g.setColor(tree.selectedBorderColor)
    else if(this.isComment)
      g.setColor(tree.comBorderColor)
    else 
      g.setColor(tree.borderColor)

    g.draw(box)
    
    // draw the label
    g.setColor(alphaColor(tree.textColor))
    if(tree.camera.zoom > 0.3) { 
      drawLabel(g)
    }
  }
  
  
  /**
   * Computes an ARGB Color from an RGB color using the NodeSprite's opacity value.
   * @param color    The color we are applying this sprite's opacity to.
   * @return    A new Color with RGB identical to color's RGB, but using opacity as its alpha component.
   */
  private def alphaColor(color : Color) : Color = {
    new Color(color.getRed, color.getGreen, color.getBlue, (255 * opacity).toInt)
  }
  
  
  /**
   * Draws the node's label. It applies syntax highlighting if the syntaxColoring flag is true.
   * @param g     The graphics context to draw the label with.
   */
  private def drawLabel(g : Graphics2D) : Unit = {
    if(tree.syntaxColoring && !isComment) {
      for(i <- 0 until formattedString.lines.size) {
        val line = formattedString.lines(i)
        for((j, substr) <- line.substrings) {
          g.setColor(new Color(Integer.parseInt(substr.color.replace("#",""), 16)))
          g.drawString(substr.toString, (3 + j*(tree.font.getSize*0.6)).toInt, (box.y - 3 + (tree.font.getSize + 3)*(i+1)).toInt)
        } // endfor
      } // endfor
      
    } 
    else {
      for(i <- 0 until formattedString.lines.size) {
        val line = formattedString.lines(i)
        g.drawString(line.toString, 3, (box.y - 3 + (tree.font.getSize + 3)*(i+1)).toInt)
      } // endfor
    } // endif
  }
    
    
  /**
   * Draws the edges of this node to decompressed children.
   * @param g    The graphics context the edges are being rendered to.
   */
  private def drawEdges(g : Graphics2D) : Unit = {
    
    
    // store the original transform
    val origTrans = g.getTransform
    
    // use the unselected border color as the color for the edges
    g.setColor(alphaColor(tree.comBorderColor))
    
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
      val startPt = g.getTransform.transform(new Point2D.Double(0,0), null)
      val endPt = g.getTransform.transform(new Point2D.Double(endX,0), null)
      
      if(startPt.getX <= tree.camera.pWidth && endPt.getX >= 0) {
        // create the cubic curve shape for the edge. //  Then draw the edge.
        val edge = new CubicCurve2D.Double(0, 0, ctrlX, 0, ctrlX, endY, endX, endY)
                
        if(child.expansion > 0.01) g.draw(edge)
      } // endif
      
      // restore the original transform for the next edge drawing.
      g.setTransform(origTrans)
    } // endfor
    
    // restore the original transform at the end.
    g.setTransform(origTrans)
  }
  
  

  ////////// children processing/positioning methods
  
  /** Apply method returns this node's child at index i. */
  def apply(i : Int) : NodeSprite = {
    children(i)
  }
  
  /**
   * Adds a child node to this NodeSprite.
   * @param node    The child being added to this node.
   */
  def addChild(node : NodeSprite) : Unit = {
    node.index = children.size
    children += node
  }
  
  /** 
   * Produces a child node for this NodeSprite that is appended to its 
   * children list. 
   * @param term        The term contained by the child node.
   * @param isComment   true if the child node's term will not be syntax colored.
   * @return            The new child node.
   */
  def makeChild(term : String = "child node", isComment : Boolean = true) : NodeSprite = {
    val child = new NodeSprite(term, tree, this, isComment)
    addChild(child)
    child
  }
    
  /** Removes the last child of this NodeSprite. */
  def remLastChild : Boolean = {
      if(children.size == 0) return false
      children.remove(children.size - 1)
      true
  }
  
  
  /**
   * Counts the number of expanded leaves in this node's subtree.
   * This is recursively computed for all nodes in the subtree and the leaf
   * counts are stored in the numLeaves variable of each node for later use.
   * @return    The number of "leaves" in this node's expanded subtree.
   */
  def countLeaves : Int = {
    this.numLeaves = 0
    
    for(child <- this.children if !child.isCompressed) {
      this.numLeaves += math.max(1, child.countLeaves)
    }
    
    this.numLeaves
  }
  
  
  /**
   * Obtains the expanded position of a child node relative to this node.
   * @param index    the index of the child whose position we need.
   * @return      the child node's position relative to its parent.
   */
  def getChildPosition(index : Int) : Point2D = {
    val longestSib = getLongestSibling
    val childX = longestSib - box.width + tree.defX + 5*_adjustedLeafCount
    val childY = tree.defY*index + children(index).offsetY
    
    new Point2D.Double(childX,childY)
  }
  
  
  /** 
   * Computes the layout metrics for this subtree.
   */
  def computeSubtreeMetrics : Unit = {
    _countAdjustedLeaves
    _computeYOffsets
  }
  
  /**
   * Recursively computes what the world coordinates for all nodes in this node's 
   * expanded subtree. These coordinates are stored in their nodes for later use.
   *
   * @return    The upper and lower bounding y-coordinates of this node's 
   *            expanded subtree.
   */
  def _computeYOffsets : (Double, Double) = {
    // the root node has no y-offset. Just set its world coordinates to that of the tree.
    if(this == tree.root) {
      worldX = tree.x
      worldY = tree.y
    }
    
    // The bounds of this node's box are a good place to start.
    this.subTreeUpperY = this.worldY + this.box.y
    this.subTreeLowerY = this.worldY + this.box.y + this.box.height
    
    // The number of leaves of previous sibling nodes will be used to determine 
    // what the child's y-offset should be. 
    var accumulatedChildYOffset = 0 - math.max(this._adjustedLeafCount - 1, 0)/2
    
    // Compute the y-bounds of the expanded children too.
    for(child <- this.children if !child.isCompressed) {

      val leafOffset = math.max(child._adjustedLeafCount - 1,0)
      
      // compute the child's y-offset with magic
      child.offsetY = (accumulatedChildYOffset + leafOffset/2)*tree.defY
      
      // add this child's adjusted leaves onto accumulatedChildYOffset.
      accumulatedChildYOffset += leafOffset
      
      // Compute the child's world coordinates.
      val childPos : Point2D = this.getChildPosition(child.index)
      child.worldX = this.worldX + childPos.getX + this.box.width
      child.worldY = this.worldY + childPos.getY
      
      // recursively compute the y-offsets.
      val (subUpper, subLower) = child._computeYOffsets
      this.subTreeUpperY = math.min(this.subTreeUpperY, subUpper)
      this.subTreeLowerY = math.max(this.subTreeLowerY, subLower)
    } // endfor
    
    (this.subTreeUpperY, this.subTreeLowerY)
  }
  
  
  /**
   * Counts the number of expanded leaves in this node's subtree for the 
   * purpose of determining layout metrics. Multilined nodes may count 
   * as more than 1 leaf.
   * @return    An adjusted number of "leaves" in this node's expanded subtree.
   */
  def _countAdjustedLeaves : Int = {
    this._adjustedLeafCount = 0
    
    var numDecompChildren = 0
    
    for(child <- this.children if !child.isCompressed) {
      numDecompChildren += 1
      this._adjustedLeafCount += math.max(1,child._countAdjustedLeaves)
    }
    
    // count multi-lined nodes as more than 1 node. There is probably a better way to do this...
    this._adjustedLeafCount = math.max(this._adjustedLeafCount, this._excessHeight/(tree.font.getSize+5) + 0.5).toInt 
    this._adjustedLeafCount
  }
  
  
  /**
   * Gets the width of the longest sibling's box.
   * @return    the length of this node's parent's longest child.
   */
  def getLongestSibling : Double = {
    var longest = 0.0
    
    if(parent == null)
      return boxWidth
      
    for(child <- parent.children) {
      if(child.boxWidth > longest)  longest = child.boxWidth
    }
    longest
  }
  
  
  /**
   * Returns this node's world coordinates as a Point2D.Double object.
   */
  def getWorldPosition : Point2D = {
    new Point2D.Double(worldX,worldY)
  }

  
  /** 
   * Returns the excess height of this node in pixels beyond what it would 
   * normally be if its label were only 1 line. 
   */
  def _excessHeight : Int = {
    boxHeight - (tree.font.getSize+5)
  }
  
  ////////////  collision methods
  
  /**
   *  Returns the rectangle representing this node's bounding box in world coordinates.
   */
  override def getCollisionBox : Rectangle2D = {
    new Rectangle2D.Double(worldX + box.x, worldY + box.y, boxWidth, boxHeight)
  }
  
  /** 
   * Checks to see if the bounding box of this node contains a screen coordinate.
   */
  def containsScreenPt(screenPt : Point2D) : Boolean = {
    val scrBox = getScreenBox
    if(scrBox == null)
      return false
    
    (scrBox.contains(screenPt.getX, screenPt.getY))
  }
  
  
  /** Returns the rectangle of this node's bounding box in screen coordinates. */
  def getScreenBox : Rectangle2D = {
    val pt1 = new Point2D.Double(box.getX, box.getY)
    val pt2 = new Point2D.Double(boxWidth, boxHeight)
    
    val scrPt1 = curTrans.transform(pt1, null)
    val scrPt2 = curTrans.transform(pt2, null)
    
    val scrW = scrPt2.getX - scrPt1.getX
    val scrH = scrPt2.getY - scrPt1.getY
    
    new Rectangle2D.Double(scrPt1.getX, scrPt1.getY, scrW, scrH)
  }
}

