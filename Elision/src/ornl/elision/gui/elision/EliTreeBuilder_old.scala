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

import collection.mutable.ArrayStack
import collection.mutable.{OpenHashMap => HashMap}
import scala.actors.Actor
import scala.xml._

import ornl.elision.gui._
import ornl.elision.gui.trees._


/** 
 * @deprecated 
 * A factory class used to contruct TreeSprites. 
 */
class EliTreeBuilderOld extends Thread {
  /** Maintains a stack of id->NodeSprite tables used to obtain the local NodeSprite variables for a particular method scope during Elision's process. */
  val scopeStack = new ArrayStack[HashMap[String, NodeSprite]]
  
  /** The current id->NodeSprite table being used by Elision's current method scope for referencing local NodeSprites. */
  var curScope : HashMap[String, NodeSprite] = null
  
  /** The tree currently being built. */
  var treeSprite : TreeSprite = null
  
  /** The root NodeSprite for the tree currently being built. */
  var root : NodeSprite = null
  
  /** A reference to the NodeSprite current being used as the subroot for an Elision method. */
  var subroot : NodeSprite = null
  /** The current subroot's ID */
  var subrootID : String = null
  
  /** Maximum Eva tree depth. If this is < 0, then there is assumed to be no maximum depth. */
  var maxTreeDepth = -1
  
  /** If true, this flag makes the EliTreeBuilder skip all processing commands until it is given a finishTree command. */
  var fatalError = false
  
  /** A reference for the EliTreeBuilder's actor. All operations with the EliTreeBuilder should be done through this actor to ensure concurrency. */
//  val tbActor = new EliTreeBuilderActor(this)
  
  /** A count of the nodes currently in the tree. */
  var nodeCount = 0
  
  /** Used by saveNodeCount and restoreNodeCount allow removal of subtrees while maintaining the correct nodeCount for this tree. */
  val savedNodeCount = new ArrayStack[Int]
  
  /** A flag that causes the treeBuilder to ignore most commands. */
  var ignoreCmds = false
    
  /** Clears the EliTreeBuilder's members. */
  def clear : Unit = {
    root = null
    subroot = null
    curScope = null
    scopeStack.clear()
    fatalError = false
    nodeCount = 0
    savedNodeCount.clear()
    ignoreCmds = false
  }
    
  /** 
   * Clears the EliTreeBuilder and creates a new tree containing only a root node. 
   * A scope table is added to the stack with only "root" in it which maps to the root node. 
   * @param rootLabel     The label for the root node of the new tree. This node will automatically be a comment node.
   */
  def newTree(rootLabel : String) : Unit = {
//  System.err.println("\nMaking a new tree")
    
    clear
    treeSprite = new elision.sprites.ElisionTreeSprite
    root = treeSprite.makeRoot(rootLabel)
    root.properties = ""
    pushTable("root")
    curScope += ("root" -> root)
    curScope += ("subroot" -> root)
    setSubroot("root")
    nodeCount = 1
      
  }
    
  /**
   * Creates a TreeSprite from the EliTreeBuilder and then clears the EliTreeBuilder.
   * @return      A TreeSprite corresponding to the structure of NodeSprites in the EliTreeBuilder with root as its root NodeSprite.
   */
  def finishTree : TreeSprite = {
//  System.err.println("Finishing current tree")
      
      clear
      treeSprite
  }
    
  /** 
   * Creates a new scope table and pushes it onto scopeStack. 
   * curScope is set to this new scope table. 
   * The new scope starts with one mapping: "subroot" -> the current subroot. 
   */
  def pushTable(args : Any) : Unit = {
    if(fatalError || ignoreCmds) return
    
//        printIndent("Pushing new table - " + args)
    
    curScope = new HashMap[String, NodeSprite]
    scopeStack.push(curScope)
    curScope += ("root" -> root)
    curScope += ("subroot" -> subroot)
  }
    
  /** 
   * Pops and discards the current scope from scopeStack. 
   * curScope is set to the scopeStack's new top. 
   */
  def popTable(args : Any) : Unit = {
    if(fatalError || ignoreCmds || scopeStack.size == 1) return
    
//        printIndent("Popping current table - " + args)
    
    // set the current subroot to the subroot currently in the table.
    subroot = curScope("subroot")
    // pop and discard the current scope.
    scopeStack.pop 
    // use whatever scope is on the top of the stack.
    curScope = scopeStack.top
  }
    
  /**
   * Sets the current subroot of the EliTreeBuilder.
   * @param id        The key ID for our desired NodeSprite in the current scope table.
   */
  def setSubroot(id : String) : Unit = {
    if(this.isMaxDepth || fatalError || ignoreCmds) return
//    printIndent("Setting new subroot: " + id)
    var keepgoing = true
    while(keepgoing) {
      try {
        subroot = curScope(id)
        subrootID = id
        keepgoing = false
      } 
      catch {
        case _: Throwable => 
          System.err.println("EliTreeBuilder.setSubroot error: key \"" + id + "\" does not exist in current scope table.")
          
          keepgoing = attemptStackRecovery
      }
    }
  }
    
  /**
   * Adds a new comment and atom NodeSprite to the current subroot's children list.
   * If an id is given, the new NodeSprite will be mapped from that id in the current scope table.
   * @param id            Optional id that the new atom node will be mapped with in the current scope table. It won't be mapped if id == "".
   * @param comment       The String being used as the new comment node's label.
   * @param atom          The BasicAtom the new atom node is being constructed from 
   */
  def addToSubroot(id : String, comment : String, atom : ornl.elision.core.BasicAtom) : Unit = {
    if(this.isMaxDepth || fatalError || ignoreCmds || subroot == null) return
    
//    printIndent("addToSubroot: " + id)
    
    val parent = subroot
    val node = createCommentNode(comment, parent)
    val node2 = createAtomNode(atom, node)
    if(id != "") curScope += (id -> node2)
  }
    
    
  /** 
   * Adds a new comment NodeSprite to the current subroot's children list. 
   * If an id is given, the new NodeSprite will be mapped from that id in the current scope table.
   * @param id            Optional id that the new node will be mapped with in the current scope table. It won't be mapped if id == "".
   * @param commentAtom   The String being used as the new node's label.
   */
  def addToSubroot(id : String, commentAtom : String) : Unit = {
    if(this.isMaxDepth || fatalError || ignoreCmds || subroot == null) return

//    printIndent("addToSubroot: " + id)

    val parent = subroot
    val node = createCommentNode(commentAtom, parent)
    if(id != "") curScope += (id -> node)
  }
    
  /** 
   * Adds a new atom NodeSprite to the current subroot's children list. 
   * If an id is given, the new NodeSprite will be mapped from that id in the current scope table.
   * @param id            Optional id that the new node will be mapped with in the current scope table. It won't be mapped if id == "".
   * @param atom          The BasicAtom the new node is being constructed from.
   */
  def addToSubroot(id : String, atom : ornl.elision.core.BasicAtom) : Unit = {
    if(this.isMaxDepth || fatalError || ignoreCmds || subroot == null) return

//    printIndent("addToSubroot: " + id)

    val parent = subroot
    val node = createAtomNode(atom, parent)
    if(id != "") curScope += (id -> node)
  }
    
  /**
   * Adds a new comment and atom NodeSprite to another NodeSprite's children list.
   * If an id is given, the new NodeSprite will be mapped from that id in the current scope table.
   * @param parentID      The id key for the parent node in the current scope table.
   * @param id            Optional id that the new atom node will be mapped with in the current scope table. It won't be mapped if id == "".
   * @param comment       The String being used as the new comment node's label.
   * @param atom          The BasicAtom the new atom node is being constructed from 
   */
  def addTo(parentID : String, id : String, comment : String, atom : ornl.elision.core.BasicAtom) : Unit = {
    if(this.isMaxDepth || fatalError || ignoreCmds) return

//    printIndent("addTo: " + (parentID, id))

    var keepgoing = true
    while(keepgoing) {
      try {
        val parent = curScope(parentID)
        val node = createCommentNode(comment, parent)
        val node2 = createAtomNode(atom, node)
        if(id != "") curScope += (id -> node2)
        keepgoing = false
      } 
      catch {
        case _: Throwable => 
          System.err.println("EliTreeBuilder.addTo error: key \"" + parentID + "\" does not exist in current scope table.")
          
          keepgoing = attemptStackRecovery
      }
    }
  }
    
    
  /**
   * Adds a new comment NodeSprite to another NodeSprite's children list.
   * If an id is given, the new NodeSprite will be mapped from that id in the current scope table.
   * @param parentID      The id key for the parent node in the current scope table.
   * @param id            Optional id that the new node will be mapped with in the current scope table. It won't be mapped if id == "".
   * @param commentAtom   The String being used as the new node's label.
   */
  def addTo(parentID : String, id : String, commentAtom : String) : Unit = {
    if(this.isMaxDepth || fatalError || ignoreCmds) return

  //    printIndent("addTo: " + (parentID, id))

    var keepgoing = true
    while(keepgoing) {
      try {
        val parent = curScope(parentID)
        val node = createCommentNode(commentAtom, parent)
        if(id != "") curScope += (id -> node)
        keepgoing = false
      } 
      catch {
        case _: Throwable => 
          System.err.println("EliTreeBuilder.addTo error: key \"" + parentID + "\" does not exist in current scope table.")
          
          keepgoing = attemptStackRecovery
      }
    }
  }
    
  /**
   * Adds a new atom NodeSprite to another NodeSprite's children list.
   * If an id is given, the new NodeSprite will be mapped from that id in the current scope table.
   * @param parentID      The id key for the parent node in the current scope table.
   * @param id            Optional id that the new node will be mapped with in the current scope table. It won't be mapped if id == "".
   * @param atom          The BasicAtom the new node is being constructed from.
   */
  def addTo(parentID : String, id : String, atom : ornl.elision.core.BasicAtom) : Unit = {
    if(this.isMaxDepth || fatalError || ignoreCmds) return

  //    printIndent("addTo: " + (parentID, id))

    var keepgoing = true
    while(keepgoing) {
      try {
        val parent = curScope(parentID)
        val node = createAtomNode(atom, parent)
        if(id != "") curScope += (id -> node)
        keepgoing = false
      } 
      catch {
        case _: Throwable => 
          System.err.println("EliTreeBuilder.addTo error: key \"" + parentID + "\" does not exist in current scope table.")
          
          keepgoing = attemptStackRecovery
      }
    }
  }
    
    
    
  /**
   * Removes the last child of a NodeSprite
   * @param parentID      The id key for the parent node we are removing the last child from.
   */
   def remLastChild(parentID : String) : Unit = {
      if(fatalError || ignoreCmds) return
      
      var keepgoing = true
      while(keepgoing) {
        try {
          val parent = curScope(parentID)
          parent.remLastChild
          keepgoing = false
        } 
        catch {
          case _: Throwable => 
            System.err.println("EliTreeBuilder.remLastChild error: key \"" + parentID + "\" does not exist in current scope table.")
            
            keepgoing = attemptStackRecovery
        }
      }
   }
    
  /**
   * Saves the current node count.
   */
  def saveNodeCount : Unit = {
    if(fatalError || ignoreCmds) return
    savedNodeCount.push(nodeCount)
  }
    
  /**
   * Sets the current node count to the top value of savedNodeCount. This is useful for restoring the correct node count for the tree when a subtree is removed.
   */
  def restoreNodeCount(flag : Boolean) : Unit = {
    if(fatalError || ignoreCmds) return
    if(flag) nodeCount = savedNodeCount.pop
    else savedNodeCount.pop
  }
    
    
  /** Toggles the ignoreCmds flag */
  def toggleIgnore(flag : Boolean) : Unit = {
    if(fatalError) return
    ignoreCmds = flag
  }
  
  
  /** Helper method used to create a comment NodeSprite */
  private def createCommentNode(commentAtom : String, parent : NodeSprite) : NodeSprite = {
    val node = parent.makeChild(commentAtom, true)
    nodeCount += 1
    enforceNodeLimit
    node
  }
  
  /** Helper method used to create an atom NodeSprite */
  private def createAtomNode(atom : ornl.elision.core.BasicAtom, parent : NodeSprite) : NodeSprite = {
    val node = parent.makeChild(atom.toParseString, false) // new NodeSprite(atom.toParseString, parent, false)
    
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
    
    // if(parent != null) parent.addChild(node)
    nodeCount += 1
    enforceNodeLimit
    node
  }
    
  /** Helper method checks to if we've reached our depth limit for tree building. */
  private def isMaxDepth : Boolean = {
    if(maxTreeDepth < 0) false
    else (scopeStack.size >= maxTreeDepth)
  }
  
  /** A handy deubgging helper method that prefixes a number of spaces to a message equal to the current size of the scopeStack. */
  private def printIndent(str : String) : Unit = {
    for(i <- 0 until scopeStack.size) {
      System.err.print(" ")
    }
    System.err.print(scopeStack.size + "")
    System.err.println(str)
  }
  
  /** A helper method that was used to try recover the EliTreeBuilder's scope if for some reason a popTable command was forgotten somewhere. 
  Now it just halts further tree construction until Elision is done processing its current input. */
  private def attemptStackRecovery : Boolean = {
    if(false && scopeStack.size > 1) {
      popTable("n/a")
      true
    }
    else {
      if(subroot != null) 
        subroot.makeChild("The tree building error happened here. ", true)
      
      System.err.println("current scope (depth: " + scopeStack.size + ")") 
      for(key <- curScope.keys) {
          System.err.println(key)
      }
      System.err.println(curScope.toString)
      
      if(root != null)
        root.makeChild("Fatal error during EliTreeBuilder tree construction. \n\tI just don't know what went wrong!", true)
      System.err.println("Fatal error during EliTreeBuilder tree construction. \n\tI just don't know what went wrong!")
      fatalError = true
      false
    }
  }
    
    
  /** Enforces the node limit by causing a fatal error when we've reached our node limit. */
  private def enforceNodeLimit : Boolean = {
    if(nodeCount >= EvaConfig.nodeLimit && EvaConfig.nodeLimit > 1) {
      val node = root.makeChild("Eva tree node limit " + EvaConfig.nodeLimit + " has been reached! Halting further tree construction. ", true)
      System.err.println("Error during EliTreeBuilder tree construction. \n\tEva tree node limit " + EvaConfig.nodeLimit + " has been reached!")
      val node2 = subroot.makeChild("Eva tree node limit " + EvaConfig.nodeLimit + " has been reached! Halting further tree construction. ", true)
      fatalError = true
      true
    }
    false
  }
    

  /** Starts a new thread in which the EliTreeBuilder's actor will run. */
  override def run : Unit = {
//    tbActor.start
  }
    
}



/** An actor object for doing concurrent operations with a EliTreeBuilder. */
class EliTreeBuilderOldActor(val treeBuilder : EliTreeBuilderOld) extends Actor {
    var ignoreNextTree = false
    
    def act() = {
    loop {
      receive {
                case ("Eva", cmd : String, args : Any) => 
                    // process a EliTreeBuilder command received from the Elision.
                 //   processEliTreeBuilderCommands(cmd, args)
                case "IgnoreNextTree" =>
                    ignoreNextTree = true
                case cmd => System.err.println("Bad tree builder command: " + cmd)
            }
        }
    }
    
}
