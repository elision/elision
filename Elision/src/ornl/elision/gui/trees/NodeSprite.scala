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
import java.awt.geom._
import sage2D.sprites.Sprite
import sage2D.Camera
import collection.mutable.ArrayBuffer

import ornl.elision.gui._

/**
 * A sprite used to represent a rewritten term node in a TreeSprite.
 * The x,y coordinates for NodeSprites are relative to their parent's x,y 
 * coordinates. In the case of a TreeSprite's root node, this should be 0,0.
 * @param term    A string to be used as this node's label. This will be the 
 *                parse string of the atom this node represents.
 * @param parent  This node's parent NodeSprite.
 * @param isComment  Flag indicates that this node is just a documentation 
 *                   string and doesn't actually represent an atom.
 */
class NodeSprite(var term : String = "Unnamed Node", val parent : NodeSprite = null, val isComment : Boolean = true) extends Sprite(0,0) {
  
  /** This node's collection of children */
  val children = new ArrayBuffer[NodeSprite]
  
  /** This node's index in its parent's list of children */
  var index : Int = 0
  
  /** This node's y-offset in the tree visualization */
  var offsetY : Double = 0
  
  /** 
   * This node's current number of leaf nodes (any decompressed descendant 
   * node that either has no children or whose children are compressed counts 
   * as a leaf node) 
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
  private var (edibleTerm, txtClrStarts, txtClrColors, txtClrEnds) = NodeSprite.formatter.format(term, NodeSprite.maxTermLength)
  
  /** The longest line of text in this node's label. */
  var longestLine= ""
  
  /** An ArrayBuffer containing the lines of text in this node's label */
  var termLines = new ArrayBuffer[String]
    
  // if the term is very long, separate it into multiple lines.
  val allLines = edibleTerm.split('\n')
  while(termLines.size < 9 && allLines.size > termLines.size) {
    val str = allLines(termLines.size)
    if(str.size > longestLine.size) longestLine = str
    termLines += str
    
  }
  if(allLines.size > termLines.size) termLines += "..."
  
  /** Flag for drawing the node's label with syntax coloring. */
  var syntaxColoring = !mainGUI.config.disableNodeSyntaxColoring
  
  /** The node's width */
  private val boxWidth = longestLine.length * NodeSprite.font.getSize * 0.66 + 5
  
  /** The node's height */
  private val boxHeight = (NodeSprite.font.getSize+5)*termLines.size // NodeSprite.font.getSize + 5
  
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
    
    g.setFont(NodeSprite.font)
    
    // decide whether or not to skip drawing this node due to being offscreen.
    val startPt = g.getTransform.transform(new Point2D.Double(0,box.y), null)
    val endPt = g.getTransform.transform(new Point2D.Double(box.width,box.y+box.height), null)
    val isOnScreen = (NodeSprite.camera.zoom > 0.01 && startPt.getX <= NodeSprite.camera.pWidth && endPt.getX >= 0 && startPt.getY <= NodeSprite.camera.pHeight && endPt.getY >= 0)
    
    if(isOnScreen) 
      drawThis(g)
    
    // restore the graphics context's font
    g.setFont(origFont)

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
    
    // restore the original transform
    g.setTransform(origTrans)
  }
  
  
  /** Draws this node. */
  def drawThis(g : Graphics2D) : Unit = {
    // Choose box's fill color
    g.setColor(alphaColor(
      if(!children.isEmpty) {
        if(this.isSelected)
          NodeSprite.selectedBoxColor
        else if(this.isComment)
          NodeSprite.comBoxColor
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
    
    g.fill(box)
    
    // Choose box's border color.
    if(this.isSelected)
      g.setColor(NodeSprite.selectedBorderColor)
    else if(this.isComment)
      g.setColor(NodeSprite.comBorderColor)
    else 
      g.setColor(NodeSprite.borderColor)

    g.draw(box)
    
    // draw the label
    g.setColor(alphaColor(NodeSprite.textColor))
    if(NodeSprite.camera.zoom > 0.3) { 
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
    if(syntaxColoring && !isComment) {
      var colorStack = new collection.mutable.Stack[java.awt.Color]
      var chompedChars = 0
            
      val startsCpy = txtClrStarts.clone
      val endsCpy = txtClrEnds.clone
      val colorsCpy = txtClrColors.clone
      
      /** Changes the current color if our current index in the text requires it. */
      def checkForNewColor(pos : Int) : Unit = {
        // are we at a color end?
        if(!endsCpy.isEmpty && pos == endsCpy(0) - chompedChars) {
          val prevColor = colorStack.pop
          g.setColor(prevColor)
        
          // get next color end
          val thisEnd = endsCpy(0)
          endsCpy.remove(0)
        } // endif
        
        // are we at a color start?
        if(!startsCpy.isEmpty && pos == startsCpy(0) - chompedChars) {
          val newColor = colorsCpy(0)
          colorStack = colorStack.push(g.getColor)
          g.setColor(newColor)
          
          // get next color start
          val thisStart = startsCpy(0)
          startsCpy.remove(0)
          
          // get next color
          colorsCpy.remove(0)
        } // endif
      } // enddef
            
      // draw each line
      for(i <- 0 until termLines.size) {
        var j : Int = 0
        val curLine = termLines(i)

        // process the characters in the current line until they have all been drawn.
        checkForNewColor(j)    
        while(j < curLine.size) {       
          // figure out the indices for this draw.
          var k = curLine.size
            val nextStart = if(startsCpy.isEmpty) -9999 else startsCpy(0) - chompedChars
            val nextEnd = if(endsCpy.isEmpty) -9999 else endsCpy(0) - chompedChars
            if(nextStart >= j && nextStart < k) k = nextStart
            if(nextEnd >= j && nextEnd < k) k = nextEnd
            
            // use the indices we obtained to create a substring of the current string and draw it with our current color.
            val substr = curLine.substring(j,k)
            g.drawString(substr, (3 + j*(NodeSprite.font.getSize*0.6)).toInt, (box.y - 3 + (NodeSprite.font.getSize + 3)*(i+1)).toInt)

            j = k
            checkForNewColor(j)
        } // endwhile
        
        chompedChars += curLine.size+1
      } // endfor
      
    } else {
      for(i <- 0 until termLines.size) 
        g.drawString(termLines(i), 3, (box.y - 3 + (NodeSprite.font.getSize + 3)*(i+1)).toInt)
    } // endif
  }
    
    
  /**
   * Draws the edges of this node to decompressed children.
   * @param g    The graphics context the edges are being rendered to.
   */
  private def drawEdges(g : Graphics2D) : Unit = {
    import java.awt.geom.CubicCurve2D
        import java.awt.geom.Line2D
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
      } // endif
      
      // restore the original transform for the next edge drawing.
      g.setTransform(origTrans)
    } // endfor
    
    // restore the original transform at the end.
    g.setTransform(origTrans)
  }
  
  

  ////////// children processing/positioning methods
  
  /**
   * Adds a child node to this NodeSprite.
   * @param node    The child being added to this node.
   */
  def addChild(node : NodeSprite) : Unit = {
    node.index = children.size
    children += node
  }
  
    
    /** Removes the last child of this NodeSprite. */
    def remLastChild : Boolean = {
        if(children.size == 0) return false
        children.remove(children.size - 1)
        true
    }
  
  /**
   * Obtains the position of a child node relative to this node.
   * @param index    the index of the child whose position we need.
   * @return      the child node's position relative to its parent.
   */
  def getChildPosition(index : Int) : geom.Point2D = {
    val longestSib = getLongestSibling
    val childX = longestSib - box.width + TreeSprite.defX + 5*numLeaves
    val childY = TreeSprite.defY*index + children(index).offsetY
    
    new geom.Point2D.Double(childX,childY)
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
  def getWorldPosition : geom.Point2D = {
    new geom.Point2D.Double(worldX,worldY)
  }
  
  /** Returns the excess height of this node in pixels beyond what it would normally be if its label were only 1 line. */
  def excessHeight : Int = {
    boxHeight - (NodeSprite.font.getSize+5)
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
    try {
      val pt1 = new Point2D.Double(box.getX, box.getY)
      val pt2 = new Point2D.Double(boxWidth, boxHeight)
      
      val scrPt1 = curTrans.transform(pt1, null)
      val scrPt2 = curTrans.transform(pt2, null)
      
      val scrW = scrPt2.getX - scrPt1.getX
      val scrH = scrPt2.getY - scrPt1.getY
      
      new Rectangle2D.Double(scrPt1.getX, scrPt1.getY, scrW, scrH)
    }
    catch {
      case _ =>
        null
    }
  }
}


/** Contains static data used by NodeSprites representing Elision data. */
object NodeSprite {
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
  var camera : Camera = null
  
  /** Elision syntax formatter. */
  val formatter = new syntax.SyntaxFormatter(elision.EliRegexes, true, true)
}