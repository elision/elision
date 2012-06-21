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

package ornl.elision.gui

import collection.mutable.ArrayStack
import collection.mutable.HashMap

/** A factory class used to contruct TreeSprites. */
class TreeBuilder {
    /** Maintains a stack of id->NodeSprite tables used to obtain the local NodeSprite variables for a particular method scope during Elision's process. */
    val scopeStack = new ArrayStack[HashMap[String, NodeSprite]]
    
    /** The current id->NodeSprite table being used by Elision's current method scope for referencing local NodeSprites. */
    var curScope : HashMap[String, NodeSprite] = null
    
    /** The root NodeSprite for the tree currently being built. */
    var root : NodeSprite = null
    
    /** A reference to the NodeSprite current being used as the subroot for an Elision method. */
    var subroot : NodeSprite = null
    /** The current subroot's ID */
    var subrootID : String = null
    
    /** Maximum Eva tree depth. If this is < 0, then there is assumed to be no maximum depth. */
    var treeMaxDepth = -1
    
    /** If true, this flag makes the TreeBuilder skip all processing commands until it is given a finishTree command. */
    var fatalError = false
    
    /** Clears the TreeBuilder's members. */
    def clear : Unit = {
        root = null
        subroot = null
        curScope = null
        scopeStack.clear()
        fatalError = false
    }
    
    /** 
     * Clears the TreeBuilder and creates a new tree containing only a root node. 
     * A scope table is added to the stack with only "root" in it which maps to the root node. 
     * @param rootLabel     The label for the root node of the new tree. This node will automatically be a comment node.
     */
    def newTree(rootLabel : String) : Unit = {
        System.err.println("\nMaking a new tree")
        
        clear
        root = new NodeSprite(rootLabel)
        root.properties = ""
        pushTable("root")
        curScope += ("root" -> root)
        setSubroot("root")
    }
    
    /**
     * Creates a TreeSprite from the TreeBuilder and then clears the TreeBuilder.
     * @return      A TreeSprite corresponding to the structure of NodeSprites in the TreeBuilder with root as its root NodeSprite.
     */
    def finishTree : TreeSprite = {
        System.err.println("Finishing current tree")
        
        val treeSprite = new TreeSprite(0,0,root)
        clear
        treeSprite
    }
    
    /** 
     * Creates a new scope table and pushes it onto scopeStack. 
     * curScope is set to this new scope table. 
     * The new scope starts with one mapping: "subroot" -> the current subroot. 
     */
    def pushTable(args : Any) : Unit = {
        if(fatalError) return
        
        printIndent
        System.err.println("Pushing new table - " + args)
        
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
        if(fatalError || scopeStack.size == 1) return
        
        printIndent
        System.err.println("Popping current table - " + args)
        
        // set the current subroot to the subroot currently in the table.
        subroot = curScope("subroot")
        // pop and discard the current scope.
        scopeStack.pop 
        // use whatever scope is on the top of the stack.
        curScope = scopeStack.top
    }
    
    /**
     * Sets the current subroot of the TreeBuilder.
     * @param id        The key ID for our desired NodeSprite in the current scope table.
     */
    def setSubroot(id : String) : Unit = {
        if(this.isMaxDepth || fatalError) return
    //    printIndent
    //    System.err.println("Setting new subroot: " + id)
        var keepgoing = true
        while(keepgoing) {
            try {
                subroot = curScope(id)
                subrootID = id
                keepgoing = false
            } 
            catch {
                case _ => System.err.println("TreeBuilder.setSubroot error: key \"" + id + "\" does not exist in current scope table.")
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
        if(this.isMaxDepth || fatalError) return
        
    //    printIndent
    //    System.err.println("addToSubroot: " + id)
        
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
        if(this.isMaxDepth || fatalError) return
    //    printIndent
    //    System.err.println("addToSubroot: " + id)

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
        if(this.isMaxDepth || fatalError) return
    //    printIndent
    //    System.err.println("addToSubroot: " + id)

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
        if(this.isMaxDepth || fatalError) return
    //    printIndent
    //    System.err.println("addTo: " + (parentID, id))

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
                case _ => System.err.println("TreeBuilder.addTo error: key \"" + parentID + "\" does not exist in current scope table.")
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
        if(this.isMaxDepth || fatalError) return
    //    printIndent
    //    System.err.println("addTo: " + (parentID, id))

        var keepgoing = true
        while(keepgoing) {
            try {
                val parent = curScope(parentID)
                val node = createCommentNode(commentAtom, parent)
                if(id != "") curScope += (id -> node)
                keepgoing = false
            } 
            catch {
                case _ => System.err.println("TreeBuilder.addTo error: key \"" + parentID + "\" does not exist in current scope table.")
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
        if(this.isMaxDepth || fatalError) return
    //    printIndent
    //    System.err.println("addTo: " + (parentID, id))

        var keepgoing = true
        while(keepgoing) {
            try {
                val parent = curScope(parentID)
                val node = createAtomNode(atom, parent)
                if(id != "") curScope += (id -> node)
                keepgoing = false
            } 
            catch {
                case _ => System.err.println("TreeBuilder.addTo error: key \"" + parentID + "\" does not exist in current scope table.")
                    keepgoing = attemptStackRecovery
            }
        }
    }
    
    
    
    
    
    
    /** Helper method used to create a comment NodeSprite */
    private def createCommentNode(commentAtom : String, parent : NodeSprite) : NodeSprite = {
        val node = new NodeSprite(commentAtom, parent, true)
        parent.addChild(node)
        node
    }
    
    /** Helper method used to create an atom NodeSprite */
    private def createAtomNode(atom : ornl.elision.core.BasicAtom, parent : NodeSprite) : NodeSprite = {
        val node = new NodeSprite(atom.toParseString, parent, false)
        
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
		
		try {
			node.properties += "constant pool: \n"
			for(i <- atom.constantPool.get) node.properties += "\t" + i + "\n"
		} catch {
			case _ => {}
		}
        
        if(parent != null) parent.addChild(node)
        node
    }
    
    
    private def isMaxDepth : Boolean = {
        if(treeMaxDepth < 0) false
        else (scopeStack.size >= treeMaxDepth)
    }
    
    private def printIndent : Unit = {
        for(i <- 0 until scopeStack.size) {
            System.err.print(" ")
        }
        System.err.print(scopeStack.size + "")
    }
    
    
    private def attemptStackRecovery : Boolean = {
        if(false && scopeStack.size > 1) {
            popTable("n/a")
            true
        }
        else {
            System.err.println("Fatal error during TreeBuilder tree construction. \n\tI just don't know what went wrong!")
            fatalError = true
            false
        }
    }
}



