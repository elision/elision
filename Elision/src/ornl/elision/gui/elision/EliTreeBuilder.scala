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

package ornl.elision.gui.elision

import collection.mutable.Stack
import scala.actors.Actor

import ornl.elision.core.AtomWalker
import ornl.elision.core.BasicAtom
import ornl.elision.gui._
import ornl.elision.gui.elision.sprites._
import ornl.elision.gui.trees._

/** A factory used to construct TreeSprite visualizations from Elision atoms. */
class EliTreeBuilder(atom : BasicAtom, typ : Boolean) extends Thread {
  
  /** The resulting TreeSprite. */
  var result : TreeSprite = null
  
  /** Maximum Eva tree depth. If this is < 0, then there is assumed to be no maximum depth. */
  var maxTreeDepth = -1
  
  /** A count of the nodes made so far for the current TreeSprite. */
  var nodeCount = 0
  
  /** The visitor passed to elision.core.AtomWalker. */
  def visitor(atom : BasicAtom, typ : Boolean) : Boolean = {
    
    if(atom.depth <= maxTreeDepth) {
      // build the NodeSprite(s) for this atom.
      _createAtomNode(atom, result.root)
    }
        
    if(nodeCount >= EvaConfig.nodeLimit) {
      return false
    }
    
    true
  }
  
  
  
  
  
  /** Helper method used to create a comment NodeSprite */
  private def createCommentNode(commentAtom : String, parent : NodeSprite) : NodeSprite = {
    val node = parent.makeChild(commentAtom, true)
    nodeCount += 1
    enforceNodeLimit
    node
  }
  
  
  /** Helper method used to create an atom NodeSprite */
  private def _createAtomNode(atom : ornl.elision.core.BasicAtom, parent : NodeSprite) : NodeSprite = {
    val node = parent.makeChild(atom.toParseString, false) // new NodeSprite(atom.toParseString, parent, false)
    nodeCount += 1
    
    // Set the node's properties String with the atom's basic properties.
    // Later it might be a good idea to use matching to set the properties according to the type of BasicAtom used.
    node.properties = "Class: " + atom.getClass + "\n\n"
    node.properties = "Class: " + atom.getClass + "\n\n"
    node.properties += "Type: " + atom.theType + "\n\n"
    node.properties += "De Bruijn index: " + atom.deBruijnIndex + "\n\n"
    node.properties += "Depth: " + atom.depth + "\n\n"
    
    node.properties += "Is bindable: " + atom.isBindable + "\n\n"
    node.properties += "Is false: " + atom.isFalse + "\n\n"
    node.properties += "Is true: " + atom.isTrue + "\n\n"
    node.properties += "Is De Bruijn index: " + atom.isDeBruijnIndex + "\n\n"
    node.properties += "Is constant: " + atom.isConstant + "\n\n"
    node.properties += "Is term: " + atom.isTerm + "\n\n"
    
    enforceNodeLimit
    node
  }
  
  
  
  /** Enforces the node limit by causing a fatal error when we've reached our node limit. */
  private def enforceNodeLimit : Boolean = {
    if(nodeCount >= EvaConfig.nodeLimit && EvaConfig.nodeLimit > 1) {
      val node = result.root.makeChild("Eva tree node limit " + EvaConfig.nodeLimit + " has been reached! Halting further tree construction. ", true)
      System.err.println("Error during EliTreeBuilder tree construction. \n\tEva tree node limit " + EvaConfig.nodeLimit + " has been reached!")
//      val node2 = subroot.makeChild("Eva tree node limit " + EvaConfig.nodeLimit + " has been reached! Halting further tree construction. ", true)
      true
    }
    false
  }
  
  
  /** Construct the TreeSprite in the background. */
  override def run() {
    result = new ElisionTreeSprite
    result.makeRoot(atom.toParseString)
    AtomWalker(atom, visitor, typ)
    TreeBuilderActor ! ("finish", result)
  }
}





object TreeBuilderActor extends Actor {  
  def act() = {
    loop {
      receive {
        case ("visualize", atom : BasicAtom) =>
          GUIActor ! ("loading", true) 
          val builder = new EliTreeBuilder(atom, false)
          builder.start
        case ("OpenTree", file : java.io.File) =>
          openTree(file)
        case ("SaveTreeXML", file : java.io.File) =>
          saveTreeXML(file)
        case ("SaveTreeJSON", file : java.io.File) =>
          saveTreeJSON(file)
        case ("finish", result : TreeSprite) =>
          finishTree(result)
        case _ =>
      }
    }
  }
  
  
  /** Loads a TreeSprite from an xml or json file. */
  def openTree(file : java.io.File) {
    if(EvaConfig.disableTree) {
      GUIActor ! ("loading", false)
      return
    }
      
    System.out.println("\nLoading tree from: " + file.getPath + "\n")
     
    // get a reference to the tree visualization panel
    val treeVisPanel : TreeVisLevel = mainGUI.visPanel.curLevel match {
      case tvp : TreeVisLevel =>
        tvp
      case _ =>
        null
    }
    if(treeVisPanel != null) {
      GUIActor ! ("loading", true)
      
      var filePath = file.getPath
      var treeSprite : TreeSprite = null
      if(filePath.endsWith(".treexml"))
        treeSprite = TreeFileIO.loadFromFile(file)
      if(filePath.endsWith(".treejson"))
        treeSprite = TreeFileIO.loadFromFileJSON(file)
      
      if(treeSprite != null) {
        treeVisPanel.changeTree(treeSprite)
      }
      
      GUIActor ! ("loading", false)
    }
    GUIActor ! "newPrompt"
  }
  
  
  
  /** Saves the current TreeSprite to an XML file. */
  def saveTreeXML(file : java.io.File) {
    // make the correct treexml file path to save the tree to.
    var filePath = file.getPath
    if(!filePath.endsWith(".treexml")) filePath += ".treexml"
    
    // get a reference to the tree visualization panel
    val treeVisPanel : TreeVisLevel = mainGUI.visPanel.curLevel match {
        case tvp : TreeVisLevel =>
            tvp
        case _ =>
            null
    }
    if(treeVisPanel != null) {
        GUIActor ! ("loading", true)
        val success = TreeFileIO.saveToFileXML(treeVisPanel.treeSprite, filePath)
        GUIActor ! ("loading", false)
        
        if(success) 
            System.out.println("\nSaved the current tree to: " + filePath + "\n")
        else 
            System.out.println("Failed to save the current tree.\n")
    }
    GUIActor ! "newPrompt"
  }
  
  
  /** Saves the current TreeSprite to a JSON file. */
  def saveTreeJSON(file : java.io.File) {
    // make the correct treexml file path to save the tree to.
    var filePath = file.getPath
    if(!filePath.endsWith(".treejson")) filePath += ".treejson"
    
    // get a reference to the tree visualization panel
    val treeVisPanel : TreeVisLevel = mainGUI.visPanel.curLevel match {
        case tvp : TreeVisLevel =>
            tvp
        case _ =>
            null
    }
    if(treeVisPanel != null) {
        GUIActor ! ("loading", true)
        val success = TreeFileIO.saveToFileJSON(treeVisPanel.treeSprite, filePath)
        GUIActor ! ("loading", false)
        if(success) System.out.println("\nSaved the current tree to: " + filePath + "\n")
        else System.out.println("Failed to save the current tree.\n")
    }
    GUIActor ! "newPrompt"
  }
  
  
  
  
  /** Load a completed TreeSprite into the visualization. */
  def finishTree(result : TreeSprite) {
    if(EvaConfig.disableTree) {
      GUIActor ! ("loading", false)
      return
    }
    
    // get a reference to the tree visualization panel
    val treeVisPanel : TreeVisLevel = mainGUI.visPanel.curLevel match {
        case tvp : TreeVisLevel =>
            tvp
        case _ =>
            null
    }
    
    GUIActor ! ("loading", true)
    if(treeVisPanel != null) {
      treeVisPanel.changeTree(result)
    }
    
    GUIActor ! ("loading", false)
    
  }
}


