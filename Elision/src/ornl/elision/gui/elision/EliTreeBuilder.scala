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

import scala.actors.Actor
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Stack

import ornl.elision.core.AtomWalker
import ornl.elision.core.BasicAtom
import ornl.elision.gui._
import ornl.elision.gui.elision.sprites._
import ornl.elision.gui.trees._

/** A factory used to construct TreeSprite visualizations from Elision atoms. */
class EliTreeBuilder(atoms : ListBuffer[(BasicAtom, String)], typ : Boolean) extends Thread {
  
  /** The resulting TreeSprite. */
  var result : TreeSprite = null
  
  /** The current handler tree. */
  var handlerRoot : NodeSprite = null
  
  /** A count of the nodes made so far for the current TreeSprite. */
  var nodeCount = 0
  
  /** A stack of the current atom's ancestors. The tuples consist of the BasicAtom, its NodeSprite, its depth, and whether it's a type. */
  val parentStack = new Stack[(BasicAtom, NodeSprite, Int, Boolean)]
  
  /** The visitor passed to elision.core.AtomWalker. */
  def visitor(atom : BasicAtom, typ : Boolean) : Boolean = { 
    // Figure out the parent of this atom using our parentStack.
    var parent = _getParent(atom, typ)
    
    if(EvaConfig.maxTreeDepth < 0 || atom.depth <= EvaConfig.maxTreeDepth) {
      // build the NodeSprite(s) for this atom.
      val atomNode = _createAtomNode(atom, parent._2)
      val tuple = (atom, atomNode, atom.depth, typ)
      parentStack.push(tuple)
    }
        
    if(EvaConfig.nodeLimit < 0 || nodeCount >= EvaConfig.nodeLimit) {
      return false
    }
    
    true
  }
  
  
  def _getParent(atom : BasicAtom, typ : Boolean) : (BasicAtom, NodeSprite, Int, Boolean) = {
    var parent = parentStack.top
    
    while(parent._2 != handlerRoot) {
      if(atom.depth < parent._3 || typ) {
        return parent
      }
      else {
        parentStack.pop
        parent = parentStack.top
      } 
    }
    
    parent
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
    val node = parent.makeChild(atom.toParseString, false)
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
    
    result.makeRoot(atoms(0)._1.toParseString)
    
    for((atom : BasicAtom, label : String) <- atoms) {
      handlerRoot = result.root.makeChild(label)
      
      parentStack.clear
      val tuple = (null, handlerRoot, Int.MaxValue, false)
      parentStack.push(tuple)
      
      AtomWalker(atom, visitor, typ)
    }
    
    
    
    TreeBuilderActor ! ("finish", result)
  }
}





object TreeBuilderActor extends Actor {  
  
  /** Our current batch of atom, handler label pairs. */
  var atomBatch : ListBuffer[(BasicAtom, String)] = new ListBuffer[(BasicAtom, String)]
  
  def act() = {
    loop {
      receive {
        
        // Start building a batch of atoms to contstruct a TreeSprite for.
        case "startBatch" =>
          atomBatch = new ListBuffer[(BasicAtom, String)]
        
        // Add a labeled atom to our batch.
        case (atom : BasicAtom, str : String) =>
          val pair = (atom, str)
          atomBatch += pair
        
        // Add an unlabeled atom to our batch.
        case atom : BasicAtom =>
          val pair = (atom, "")
          System.err.println(atom.toParseString)
          atomBatch += pair
        
        // We're done building our atom batch. Create a TreeSprite of it.
        case "endBatch" =>
          GUIActor ! ("loading", true)
          val builder = new EliTreeBuilder(atomBatch, true)
          builder.start
          atomBatch = null
          
        // Open a TreeSprite from an XML or JSON file.
        case ("OpenTree", file : java.io.File) =>
          openTree(file)
        
        // Save a TreeSprite as XML.
        case ("SaveTreeXML", file : java.io.File) =>
          saveTreeXML(file)
          
        // Save a TreeSprite as JSON.
        case ("SaveTreeJSON", file : java.io.File) =>
          saveTreeJSON(file)
          
        // Receive a completed TreeSprite from an EliTreeBuilder.
        case ("finish", result : TreeSprite) =>
          finishTree(result)
          GUIActor ! ("loading", false)
          
        // Discard any unrecognized messages.
        case msg =>
          System.err.println("TreeBuilderActor received unrecognized message:\n" + msg)
      }
    }
  }
  
  
  /** Loads a TreeSprite from an xml or json file. */
  def openTree(file : java.io.File) {
    if(EvaConfig.disableTree) {
      return
    }
      
    mainGUI.consolePanel.console.emitln("\nLoading tree from: " + file.getPath + "\n")
     
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
    GUIActor ! ("newPrompt", "e> ")
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
            mainGUI.consolePanel.console.emitln("\nSaved the current tree to: " + filePath + "\n")
        else 
            mainGUI.consolePanel.console.emitln("Failed to save the current tree.\n")
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
        if(success) mainGUI.consolePanel.console.emitln("\nSaved the current tree to: " + filePath + "\n")
        else mainGUI.consolePanel.console.emitln("Failed to save the current tree.\n")
    }
    GUIActor ! "newPrompt"
  }
  
  
  
  
  /** Load a completed TreeSprite into the visualization. */
  def finishTree(result : TreeSprite) {
    if(EvaConfig.disableTree) {
      return
    }
    
    // get a reference to the tree visualization panel
    val treeVisPanel : TreeVisLevel = mainGUI.visPanel.curLevel match {
        case tvp : TreeVisLevel =>
            tvp
        case _ =>
            null
    }
    
    if(treeVisPanel != null) {
      treeVisPanel.changeTree(result)
    }
  }
}


