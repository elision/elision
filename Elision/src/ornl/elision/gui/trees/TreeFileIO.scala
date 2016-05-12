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
import collection.mutable.{OpenHashMap => HashMap}
import scala.actors.Actor
import scala.swing.Dialog
import scala.xml._

import ornl.elision.gui._

/** This singleton provides utilities for saving TreeSprites as XML and JSON. */
object TreeFileIO {
  /** 
   * Loads a TreeSprite from a treexml file. 
   * @deprecated  Eva no longer saves tree visualizations as XML files, but
   *              this method remains here to load Eva XML files for 
   *              backwards compatibility.
   */
  def loadFromFile(file : java.io.File) : TreeSprite = {
    
    /** Extracts a named attribute from an xml node. If the attribute doesn't exist, "", the empty string, is returned. */
    def extrAtt(myXML : Node, tttrait : String) : String = {
      val ttrait = "@" + tttrait
      (myXML \ ttrait).text
    }
    
    /** 
     * Replaces XML new-line literals, '&#13;' 
     * with their C equivalent, '\n'. 
     */
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
      
      val treeSprite = new elision.sprites.ElisionTreeSprite
      val rootNode = treeSprite.makeRoot(labelMap(rLabelKey)) 
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
          val node = subroot. makeChild(nodeLabel, nodeIsComment) 
          node.properties = nodeProps
          
          // recursive call
          recLoadTree(node, nodeXML)
        }
      }
      recLoadTree(rootNode, rootXML)
      
      // return the loaded tree.
      treeSprite
    }
    catch {
      case ex : Throwable => 
        // There was an error parsing the XML file, so return null.
        Dialog.showMessage(mainGUI.visPanel, "Could not read the saved tree file.", "Error", Dialog.Message.Error)
        null
    }
  }
  
  /** Loads a TreeSprite from a treejson file. */
  def loadFromFileJSON(file : java.io.File) : TreeSprite = {
      
      import scala.util.parsing.json._
      
      // replaces new lines with special characters
      def unreplaceNLs(str : String) : String = {
          val toks = str.split("""&#13;""")
          var result = ""
          
          for(tok <- toks) {
              result += tok + "\n"
          }
          result
      }
      
      def unReplaceQuotes(str : String) : String = {
          str.replaceAllLiterally("""&#14;""", "\"")
      }
      
      
      def toMap(obj : Any) : Map[String, Any] = {
          obj match {
              case toMap : Map[_,_] =>
                  toMap.asInstanceOf[Map[String,Any]]
              case _ =>
                  null
          }
      }
      
      def anyToInt(obj : Any) : Int = {
          obj match {
              case i : Double =>
                  i.toInt
              case _ =>
                  -1
          }
      }
      
      def anyToBoolean(obj : Any) : Boolean = {
          obj match {
              case i : Boolean =>
                  i
              case _ =>
                  false
          }
      }
      
      def toList(obj : Any) : List[Any] = {
          obj match {
              case toList : List[_] =>
                  toList
              case _ => 
                  null
          }
      }
      
      // Creates our tree from the top-level JSON representing this tree.
      def readTop(top : Map[String,Any]) : TreeSprite = {
          // load the label map
          val labels : Map[String, Any] = toMap(top("labelMap"))
          val labelMap = new HashMap[Int, String]
          for((key, value) <- labels) {
              labelMap += (key.toInt -> unReplaceQuotes(unreplaceNLs(value.toString)))
          }
          
          // load the properties map
          val props : Map[String, Any] =  toMap(top("propsMap"))
          val propsMap = new HashMap[Int, String]
          for((key, value) <- props) {
              propsMap += (key.toInt -> unReplaceQuotes(unreplaceNLs(value.toString)))
          }
          
          // Get the root node's data
          val rootMap : Map[String, Any] =  toMap(top("tree"))
          val rLabelKey = anyToInt(rootMap("label"))
          val rPropsKey = anyToInt(rootMap("props"))
          
          // construct the root node.
          val treeSprite = new elision.sprites.ElisionTreeSprite
          val rootNode = treeSprite.makeRoot(labelMap(rLabelKey))
          rootNode.properties = propsMap(rPropsKey)
          
          
          // recursively parses the json to construct the tree.
          def recLoadTree(subroot : NodeSprite, subrootMap : Map[String,Any]) : Unit = {
              val nodes = toList(subrootMap("tree"))
              if(nodes == null)
                  return
              
              for(nodeAny <- nodes) {
                  val nodeMap = toMap(nodeAny)
                  if(nodeMap == null)
                      return
                  
                  // get this node's data
                  val labelKey = anyToInt(nodeMap("label"))
                  val nodeLabel = labelMap(labelKey)
                  
                  val propsKey = anyToInt(nodeMap("props"))
                  val nodeProps = propsMap(propsKey)
                  
                  val nodeIsComment = anyToBoolean(nodeMap("com"))
                  
                  // construct this node and add it as a child to its parent node
                  val node = subroot.makeChild(nodeLabel, nodeIsComment)
                  node.properties = nodeProps
                  
                  // recursive call
                  recLoadTree(node, nodeMap)
              }
          }
          
          // Recursively construct the rest of the tree
          recLoadTree(rootNode, rootMap)
          
          treeSprite
      }
      
      
      try {
          // construct our JSON string from the file.
          val bf = new java.io.BufferedReader(new java.io.FileReader(file))
          var jsonStr = ""
          var line = bf.readLine
          while(line != null) {
              jsonStr += line
              line = bf.readLine
          }
          
          // Parse the JSON string
          val topOp = JSON.parseFull(jsonStr)
          
          topOp match {
              case Some(map : Map[_,_]) =>
                  val smap = map.asInstanceOf[Map[String,Any]]
                  val topAny = smap("treejson")
                  val topMap = toMap(topAny)
                  if(topMap != null) {
                      readTop(topMap)
                  }
                  else {
                      null
                  }
              case _ =>
                  null
          }
      }
      catch {
          case ex : Throwable => 
              Dialog.showMessage(mainGUI.visPanel, "Could not read the saved tree file.", "Error", Dialog.Message.Error)
              null
      }
  }
  
  /** Saves a TreeSprite to a file as JSON.*/
  def saveToFileJSON(treeSprite : TreeSprite, path : String) : Boolean = {
      val labelMap = new HashMap[String, Int]
      var labelsNext = 0
      val propsMap = new HashMap[String, Int]
      var propsNext = 0
      
      def recNodeSpriteJSON(subroot : NodeSprite) : String = {
          if(subroot.children.isEmpty)
              return "\"Empty\""
          var result = ""
          var isFirst = true
          for(child <- subroot.children) {
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
                  
              if(!isFirst)
                  result += ","
              isFirst = false
              
              result += "{\"label\":" + labelKey + ", \"props\":" + propsKey + ", \"com\":" + child.isComment + ", \"tree\":[" + recNodeSpriteJSON(child) + "]}"
          }
          result
      }
      
      def replaceNLs(str : String) : String = {
          val toks = str.split("\n")
          var result = ""
          
          for(tok <- toks) {
              result += tok + """&#13;"""
          }
          result
      }
      
      def replaceQuotes(str : String) : String = {
          str.replaceAllLiterally("\"", """&#14;""")
      }
      
      try {
          labelMap += (treeSprite.root.term -> 0)
          labelsNext = 1
          propsMap += (treeSprite.root.properties -> 0)
          propsNext = 1
          
          val rootJSON = "\"tree\" : {\"label\":0, \"props\":0, \"tree\":[" + recNodeSpriteJSON(treeSprite.root) + "]}"
          
          var labelMapJSON = "\"labelMap\":{"
          var isFirst = true
          for( (value, key) <- labelMap) {
              if(!isFirst)
                  labelMapJSON += ","
              isFirst = false
              
              labelMapJSON += "\"" + key + "\":\"" + replaceQuotes(replaceNLs(value)) + "\""
          }
          labelMapJSON += "}"
          
          var propsMapJSON = "\"propsMap\":{"
          isFirst = true
          for( (value, key) <- propsMap) {
              if(!isFirst)
                  propsMapJSON += ","
              isFirst = false
              
              propsMapJSON += "\"" + key + "\":\"" + replaceQuotes(replaceNLs(value)) + "\""
          }
          propsMapJSON += "}"

          val all = "{\"treejson\":{" + labelMapJSON + "," + propsMapJSON + "," + rootJSON + "}}"
          
          val writer = new java.io.PrintWriter(new java.io.File(path))
          writer.write(all)
          writer.close
          
          true
      }
      catch {
          case _: Throwable => 
            Dialog.showMessage(mainGUI.visPanel, "Could not save the tree.", "Error", Dialog.Message.Error)
            false
      }
  }
  
  /** Saves a TreeSprite to a file as XML. */
  def saveToFileXML(treeSprite : TreeSprite, path : String) : Boolean = {
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
          case _: Throwable => 
            Dialog.showMessage(mainGUI.visPanel, "Could not save the tree.", "Error", Dialog.Message.Error)
            false
      }
  }
}