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

import collection.mutable.ArrayStack
import collection.mutable.HashMap
import scala.actors.Actor
import scala.xml._

import ornl.elision.gui._


/** A factory class used to contruct TreeSprites. */
class TreeBuilder extends Thread {
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
    
    /** A reference for the TreeBuilder's actor. All operations with the TreeBuilder should be done through this actor to ensure concurrency. */
    val tbActor = new TreeBuilderActor(this)
    
    /** A count of the nodes currently in the tree. */
    var nodeCount = 0
    
    /** Used by saveNodeCount and restoreNodeCount allow removal of subtrees while maintaining the correct nodeCount for this tree. */
    val savedNodeCount = new ArrayStack[Int]
    
    /** A flag that causes the treeBuilder to ignore most commands. */
    var ignoreCmds = false
    
    /** Clears the TreeBuilder's members. */
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
     * Clears the TreeBuilder and creates a new tree containing only a root node. 
     * A scope table is added to the stack with only "root" in it which maps to the root node. 
     * @param rootLabel     The label for the root node of the new tree. This node will automatically be a comment node.
     */
    def newTree(rootLabel : String) : Unit = {
//        System.err.println("\nMaking a new tree")
        
        clear
        root = new NodeSprite(rootLabel)
        root.properties = ""
        pushTable("root")
        curScope += ("root" -> root)
        curScope += ("subroot" -> root)
        setSubroot("root")
        nodeCount = 1
        
    }
    
    /**
     * Creates a TreeSprite from the TreeBuilder and then clears the TreeBuilder.
     * @return      A TreeSprite corresponding to the structure of NodeSprites in the TreeBuilder with root as its root NodeSprite.
     */
    def finishTree : TreeSprite = {
//        System.err.println("Finishing current tree")
        
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
     * Sets the current subroot of the TreeBuilder.
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
        if(this.isMaxDepth || fatalError || ignoreCmds) return
        
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
        if(this.isMaxDepth || fatalError || ignoreCmds) return

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
        if(this.isMaxDepth || fatalError || ignoreCmds) return

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
                case _ => System.err.println("TreeBuilder.addTo error: key \"" + parentID + "\" does not exist in current scope table.")
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
                case _ => System.err.println("TreeBuilder.remLastChild error: key \"" + parentID + "\" does not exist in current scope table.")
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
        val node = new NodeSprite(commentAtom, parent, true)
        parent.addChild(node)
        nodeCount += 1
        enforceNodeLimit
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
        nodeCount += 1
        enforceNodeLimit
        node
    }
    
    /** Helper method checks to if we've reached our depth limit for tree building. */
    private def isMaxDepth : Boolean = {
        if(treeMaxDepth < 0) false
        else (scopeStack.size >= treeMaxDepth)
    }
    
    /** A handy deubgging helper method that prefixes a number of spaces to a message equal to the current size of the scopeStack. */
    private def printIndent(str : String) : Unit = {
        for(i <- 0 until scopeStack.size) {
            System.err.print(" ")
        }
        System.err.print(scopeStack.size + "")
        System.err.println(str)
    }
    
    /** A helper method that was used to try recover the TreeBuilder's scope if for some reason a popTable command was forgotten somewhere. 
    Now it just halts further tree construction until Elision is done processing its current input. */
    private def attemptStackRecovery : Boolean = {
        if(false && scopeStack.size > 1) {
            popTable("n/a")
            true
        }
        else {
            addTo("root", "", "Fatal error during TreeBuilder tree construction. \n\tI just don't know what went wrong!")
            System.err.println("Fatal error during TreeBuilder tree construction. \n\tI just don't know what went wrong!")
            fatalError = true
            false
        }
    }
    
    
    /** Enforces the node limit by causing a fatal error when we've reached our node limit. */
    private def enforceNodeLimit : Boolean = {
        if(nodeCount >= mainGUI.config.nodeLimit && mainGUI.config.nodeLimit > 1) {
            val node = new NodeSprite("Eva tree node limit " + mainGUI.config.nodeLimit + " has been reached! Halting further tree construction. " , root, true)
            System.err.println("Fatal error during TreeBuilder tree construction. \n\tEva tree node limit " + mainGUI.config.nodeLimit + " has been reached!")
            root.addChild(node)
            val node2 = new NodeSprite("Eva tree node limit " + mainGUI.config.nodeLimit + " has been reached! Halting further tree construction. " , subroot, true)
            subroot.addChild(node2)
            fatalError = true
            true
        }
        false
    }
    
    
    
    /** Loads a TreeSprite from a treexml file. */
    def loadFromFile(file : java.io.File) : TreeSprite = {
        
        /** Extracts a named attribute from an xml node. If the attribute doesn't exist, "", the empty string, is returned. */
        def extrAtt(myXML : Node, tttrait : String) : String = {
            try {
                val ttrait = "@" + tttrait
                (myXML \ ttrait).text
            }
            catch {
                case _ => ""
            }
        }
        
        def unreplaceNLs(str : String) : String = {
            val toks = str.split("""&#13;""")
            var result = ""
            
            for(tok <- toks) {
                result += tok + "\n"
            }
            result
        }
        
        try {
            val topXML = XML.loadFile(file)
            
            // load the label map
            val labels = (topXML \ "labelMap") \ "label"
            val labelMap = new HashMap[Int, String]
            for(labelXML <- labels) {
                val key = extrAtt(labelXML, "key").toInt
                val value = extrAtt(labelXML, "value")
                
                labelMap += (key -> value)
            }
            
            // load the properties map
            val props = (topXML \ "propsMap") \ "props"
            val propsMap = new HashMap[Int, String]
            for(propsXML <- props) {
                val key = extrAtt(propsXML, "key").toInt
                val value = extrAtt(propsXML, "value")
                
                propsMap += (key -> value)
            }
            
            // load the root NodeSprite
            val rootXML = (topXML \ "root")(0)
            
            val rLabelKey = extrAtt(rootXML, "label").toInt
            val rPropsKey = extrAtt(rootXML, "props").toInt
            
            val rootNode = new NodeSprite(labelMap(rLabelKey))
            rootNode.properties = unreplaceNLs(propsMap(rPropsKey))
            
            // recursively load the rest of the tree.
            def recLoadTree(subroot : NodeSprite, subrootXML : Node) : Unit = {
                val nodes = subrootXML \ "node"
                for(nodeXML <- nodes) {
                    // load the node's data
                    val lKey = extrAtt(nodeXML, "label").toInt
                    val nodeLabel = labelMap(lKey)
                    
                    val pKey = extrAtt(nodeXML, "props").toInt
                    val nodeProps = unreplaceNLs(propsMap(pKey))
                    
                    val nodeIsComment = extrAtt(nodeXML, "com").toBoolean
                    
                    // create the node and add it to subroot's list of children.
                    val node = new NodeSprite(nodeLabel, subroot, nodeIsComment)
                    node.properties = nodeProps
                    subroot.addChild(node)
                    
                    // recursive call
                    recLoadTree(node, nodeXML)
                }
            }
            recLoadTree(rootNode, rootXML)
            
            // return the loaded tree.
            new TreeSprite(0, 0, rootNode)
        }
        catch {
            case ex : Throwable => 
                System.out.println("" + ex)
                null
        }
    }
    
    /** Saves a TreeSprite to a file. */
    def saveToFile(treeSprite : TreeSprite, path : String) : Boolean = {
        
        
        val labelMap = new HashMap[String, Int]
        var labelsNext = 0
        val propsMap = new HashMap[String, Int]
        var propsNext = 0
        
        def recNodeSpriteXML(subroot : NodeSprite) : Seq[Elem] = {
            for(child <- subroot.children) yield {
                val labelKey = if(labelMap.contains(child.term)) labelMap(child.term)
                    else {
                        labelMap += (child.term -> labelsNext)
                        val res = labelsNext
                        labelsNext += 1
                        res
                    }

                val propsKey = if(propsMap.contains(child.properties)) propsMap(child.properties)
                    else {
                        propsMap += (child.properties -> propsNext)
                        val res = propsNext
                        propsNext += 1
                        res
                    }
                
                <node label={labelKey.toString} props={propsKey.toString} com={child.isComment.toString}>{recNodeSpriteXML(child)}</node>
            }
        }
        
        def replaceNLs(str : String) : String = {
            val toks = str.split("\n")
            var result = ""
            
            for(tok <- toks) {
                result += tok + """&#13;"""
            }
            result
        }
        
        try {
            labelMap += (treeSprite.root.term -> 0)
            labelsNext = 1
            propsMap += (treeSprite.root.properties -> 0)
            propsNext = 1
            
            val rootXML = <root label={"0"} props={"0"}>{
                recNodeSpriteXML(treeSprite.root)
            }</root>
            
            
            
            val labelMapXML = <labelMap>{
                for( (value, key) <- labelMap) yield <label key={key.toString} value={value}/>
            }</labelMap>
            
            val propsMapXML = <propsMap>{
                for( (value, key) <- propsMap) yield <props key={key.toString} value={replaceNLs(value)}/>
            }</propsMap>
            
            
            
            val all = <treexml>
                    {labelMapXML}
                    {propsMapXML}
                    {rootXML}
                    </treexml>
        	XML.save(path, all)
            true
        }
        catch {
            case _ => false
        }
    }

    
    /** Starts a new thread in which the TreeBuilder's actor will run. */
	override def run : Unit = {
		tbActor.start
	}
    
}



/** An actor object for doing concurrent operations with a TreeBuilder. */
class TreeBuilderActor(val treeBuilder : TreeBuilder) extends Actor {
    var ignoreNextTree = false
    
    def act() = {
		loop {
			receive {
                case ("Eva", cmd : String, args : Any) => 
                    // process a TreeBuilder command received from the Elision.
                    processTreeBuilderCommands(cmd, args)
                case ("OpenTree", file : java.io.File) =>
                    System.out.println("\nLoading tree from: " + file.getPath + "\n")
                    
                    // get a reference to the tree visualization panel
                    val treeVisPanel : TreeVisPanel = mainGUI.visPanel match {
                        case tvp : TreeVisPanel =>
                            tvp
                        case _ =>
                            null
                    }
                    if(treeVisPanel != null) {
                        GUIActor ! ("loading", true)
                        
                        val treeSprite = treeBuilder.loadFromFile(file)
                        treeVisPanel.treeSprite = treeSprite
                        
                        // once the tree visualization is built, select its root node and center the camera on it.
                        treeVisPanel.selectNode(treeVisPanel.treeSprite.root)
                        treeVisPanel.camera.reset
                        
                        GUIActor ! ("loading", false)
                    }
                    GUIActor ! "newPrompt"
                case ("SaveTree", file : java.io.File) =>
                    // make the correct treexml file path to save the tree to.
                    var filePath = file.getPath
                    if(!filePath.endsWith(".treexml")) filePath += ".treexml"
                    
                    // get a reference to the tree visualization panel
                    val treeVisPanel : TreeVisPanel = mainGUI.visPanel match {
                        case tvp : TreeVisPanel =>
                            tvp
                        case _ =>
                            null
                    }
                    if(treeVisPanel != null) {
                        GUIActor ! ("loading", true)
                        val success = treeBuilder.saveToFile(treeVisPanel.treeSprite, filePath)
                        GUIActor ! ("loading", false)
                        if(success) System.out.println("\nSaved the current tree to: " + filePath + "\n")
                        else System.out.println("Failed to save the current tree.\n")
                    }
                    GUIActor ! "newPrompt"
                case "IgnoreNextTree" =>
                    ignoreNextTree = true
                case cmd => System.err.println("Bad tree builder command: " + cmd)
            }
        }
    }
    
    /** Called by act when the actor receives a valid TreeBuilder command. Here we actually invoke the methods of the TreeBuilder corresponding to the commands that the actor receives. */
    def processTreeBuilderCommands(cmd :String, args : Any) : Unit = {
        cmd match {
            case "newTree" =>
                args match {
                    case label : String =>
                        treeBuilder.newTree(label)
                    case _ => System.err.println("TreeBuilder.newTree received incorrect arguments: " + args)
                }
            case "finishTree" => // FINISH HIM. FATALITY. KO!
                // get a reference to the tree visualization panel
                val treeVisPanel : TreeVisPanel = mainGUI.visPanel match {
                    case tvp : TreeVisPanel =>
                        tvp
                    case _ =>
                        null
                }
                
                GUIActor ! ("loading", true)
                if(treeVisPanel != null && !ignoreNextTree) {
                    
                    treeVisPanel.treeSprite = treeBuilder.finishTree
                    
                    // once the tree visualization is built, select its root node and center the camera on it.
                    treeVisPanel.selectNode(treeVisPanel.treeSprite.root)
                    treeVisPanel.camera.reset
                    
                }
                
                ignoreNextTree = false
                GUIActor ! ("loading", false)
                
            case "pushTable" => 
                treeBuilder.pushTable(args)
            case "popTable" => 
                treeBuilder.popTable(args)
            case "setSubroot" =>
                args match {
                    case id : String =>
                        treeBuilder.setSubroot(id)
                    case _ => System.err.println("TreeBuilder.setSubroot received incorrect arguments: " + args)
                }
            case "addToSubroot" =>
                args match {
                    case (id : String, comment : String, atom : ornl.elision.core.BasicAtom) =>
                        treeBuilder.addToSubroot(id, comment, atom)
                    case (id : String, commentAtom : String) =>
                        treeBuilder.addToSubroot(id, commentAtom)
                    case (id : String, atom : ornl.elision.core.BasicAtom) =>
                        treeBuilder.addToSubroot(id, atom)
                    case _ => System.err.println("TreeBuilder.addToSubroot received incorrect arguments: " + args)
                }
            case "addTo" =>
                args match {
                    case (parentID : String, id : String, comment : String, atom : ornl.elision.core.BasicAtom) =>
                        treeBuilder.addTo(parentID, id, comment, atom)
                    case (parentID : String, id : String, commentAtom : String) =>
                        treeBuilder.addTo(parentID, id, commentAtom)
                    case (parentID : String, id : String, atom : ornl.elision.core.BasicAtom) =>
                        treeBuilder.addTo(parentID, id, atom)
                    case _ => System.err.println("TreeBuilder.addTo received incorrect arguments: " + args)
                }
            case "remLastChild" =>
                args match {
                    case parentID : String =>
                        treeBuilder.remLastChild(parentID)
                    case _ => System.err.println("TreeBuilder.remLastChild received incorrect arguments: " + args)
                }
            case "saveNodeCount" => 
                treeBuilder.saveNodeCount
            case "restoreNodeCount" =>
                args match {
                    case flag : Boolean =>
                        treeBuilder.restoreNodeCount(flag)
                    case _ => System.err.println("TreeBuilder.restoreNodeCount received incorrect arguments: " + args)
                }
            case "toggleIgnore" =>
                args match {
                    case flag : Boolean =>
                        treeBuilder.toggleIgnore(flag)
                    case _ => System.err.println("TreeBuilder.toggleIgnore received incorrect arguments: " + args)
                }
            case _ => System.err.println("GUIActor received bad TreeBuilder command: " + cmd)
        }
    }
}

