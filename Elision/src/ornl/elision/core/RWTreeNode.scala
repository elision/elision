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
package ornl.elision.core


/**
 *	RWTreeNode class
 *	Used to represent a rewritten Elision term in a tree structure representing the rewrite process. 
 *	Objects of this class are sent to the GUI in order to construct a visualization of the rewriting process. 
 */
 
class RWTreeNode(val term : String) {
	
	import scala.collection.mutable.ArrayBuffer
	
	/** This node's collection of children */
	val children = new ArrayBuffer[RWTreeNode]
	
	/** String used to display the properties of the atom this node represents. */
	var properties : String = term
	
	/** A Node can be used to represent a comment instead of a type of BasicAtom. This helps to self-document the tree. */
	var isComment = true
	
	/** 
	 * Auxillary constructor accepting a BasicAtom. 
	 * It also sets properties to the field values of the BasicAtom.
	 * @param atom		The BasicAtom this node represents
	 */
	
	def this(atom : BasicAtom) = {
		this(atom.toParseString)
		
		properties = "Parse String: " + term + "\n\n"
		properties += "Class: " + atom.getClass + "\n"
		properties += "Type: " + atom.theType + "\n"
		properties += "De Bruijn index: " + atom.deBruijnIndex + "\n"
		properties += "Depth: " + atom.depth + "\n"
		
		properties += "Is bindable: " + atom.isBindable + "\n"
		properties += "Is false: " + atom.isFalse + "\n"
		properties += "Is true: " + atom.isTrue + "\n"
		properties += "Is De Bruijn index: " + atom.isDeBruijnIndex + "\n"
		properties += "Is constant: " + atom.isConstant + "\n"
		properties += "Is term: " + atom.isTerm + "\n"
		
		isComment = false
		
		try {
			properties += "constant pool: \n"
			for(i <- atom.constantPool.get) properties += "\t" + i + "\n"
		} catch {
			case _ => {}
		}
	}
	
	/**
	 * Attempts to add node as a child node to this node. node will not be added if its term matches its parent term because 
	 * we don't want to repeat a term in our tree if it wasn't rewritten.
	 * @param		node is the child we are adding to this node. 
	 * @return		node if successful. Otherwise returns this. 
	 */
	
	def addChild(node : RWTreeNode) : RWTreeNode = {
			if(true || this.term != node.term) {
			children += node
			node
		}
		else
			this
	}
	
	/**
	 * Attempts to add a node that is just a String label and doesn't actually contain an atom. 
	 * This mainly used for the rewrite tree to self-document itself.
	 * @param		label is the String the child node will have. 
	 * @return		a new label node if successful. Otherwise returns this. 
	 */
	
	def addChild(label : String) : RWTreeNode = {
			if(true || this.term != label) {
			val node = new RWTreeNode(label)
			children += node
			node
		}
		else
			this
	}
	
	/**
	 * Creates a new RWTreeNode from a BasicAtom and tries to add it as a child to this node.
	 * @param atom		The atom we are trying to add as a child to this node.
	 * @return			The node representing atom if successful. Otherwise returns this. 
	 */
	
	def addChild(atom : BasicAtom) : RWTreeNode = {
		val child = addChild(new RWTreeNode(atom))
		child
	}
}

/**
 * Utilities for aiding in the construction of a rewrite tree structure.
 */
object RWTree {
	
	/** 
	 * Current is used to store a reference to a RWTreeNode representing an atom currently being rewritten. 
	 * Before calling rewrite on an atom, you should first create a RWTreeNode for that atom and then set current
	 * to that new node. Then you can retrieve current for processing inside that atom's rewrite method.
	 * Maintaining this reference makes it so that the parameter lists for rewrite methods don't have to be rewritten 
	 * to accomodate passing down RWTreeNodes to construct the rewrite tree structure during the rewrite process.
	 */
	var current : RWTreeNode = new RWTreeNode("")
	
	/** factory method */
	def apply(atom : BasicAtom) : RWTreeNode = new RWTreeNode(atom)
	
	/** factory method */
	def apply(term : String) : RWTreeNode = new RWTreeNode(term)
	
}


