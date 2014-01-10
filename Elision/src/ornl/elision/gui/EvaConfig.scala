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

import java.io._
import java.util.HashMap
import java.util.Map
import scala.xml._

/** Stores and saves configuration settings for Eva. */
object EvaConfig extends Serializable {
  
	/** The current decompression depth for the visualization trees. */
	var decompDepth = 0
	
	/** The maximum number of lines we want to have in the REPL panel at any one time. */
	var replMaxLines = 0
	
	/** The last directory viewed with the File->Open dialog. */
	var lastOpenPath = ""
    
  /** Maximum RWTree depth. If this is < 0, then there is assumed to be no maximum depth. */
  var maxTreeDepth = 0

  /** Flag for temporarilly disabling Eva tree construction in Elision */
  var disableTree = false
  
  /** Flag for disabling syntax coloring in NodeSprites. */
  var disableNodeSyntaxColoring = false
  
  /** The maximum nodes that Eva will include in a tree visualization. */
  var nodeLimit = 0
  
  /** Mode for Eva to start up in next time on boot-up. */
  var bootMode = ""
	
  
  /** 
   * Initialize the configurations object. Read it from the config file or
   * generate the default configurations.
   */
  def init : Unit = {
    restoreDefaults
    
    val configFile = new File("EvaConfig.xml")
    if(configFile.exists()) {
      XML.loadFile(configFile) match {
        case config : Elem => 
            parseConfigXMLProperties(config)
      }
    }
  }
  
  /**
   * Parses the XML config properties for EVA. Any properties not present in 
   * the XML are skipped and retain their default values.
   */
  private def parseConfigXMLProperties(config : Elem) : Unit = {
    (config \ "decompDepth").text match {
      case "" => // Skip this property.
      case text : String =>
        decompDepth = text.toInt
    }
    
    (config \ "replMaxLines").text match {
      case "" => // Skip this property.
      case text : String =>
        replMaxLines = text.toInt
    }
    
    (config \ "lastOpenPath").text match {
      case "" => // Skip this property.
      case text : String =>
        lastOpenPath = text
    }
    
    (config \ "maxTreeDepth").text match {
      case "" => // Skip this property.
      case text : String =>
        maxTreeDepth = text.toInt
    }
    
    (config \ "disableTree").text match {
      case "" => // Skip this property.
      case text : String =>
        disableTree = text.toBoolean
    }
    
    (config \ "disableNodeSyntaxColoring").text match {
      case "" => // Skip this property.
      case text : String =>
        disableNodeSyntaxColoring = text.toBoolean
    }
    
    (config \ "nodeLimit").text match {
      case "" => // Skip this property.
      case text : String =>
        nodeLimit = text.toInt
    }
    
    (config \ "bootMode").text match {
      case "" => // Skip this property.
      case text : String =>
        bootMode = text
    }
  }
	
	/** Restores the default values for all configuration variables. */
	def restoreDefaults : Unit = {
		decompDepth = 2
		replMaxLines = 60
		lastOpenPath = "."
    maxTreeDepth = -1
    disableTree = false
    disableNodeSyntaxColoring = false
    nodeLimit = 10000
    bootMode = "Welcome"
	}
	
	/** Saves the configuration object to ".\EvaConfig.xml" */
	def save : Unit = {
		val xmlString = 
"""
<Eva>
    <decompDepth>""" + decompDepth + """</decompDepth>
    <replMaxLines>""" + replMaxLines + """</replMaxLines>
    <lastOpenPath>""" + lastOpenPath + """</lastOpenPath>
    <maxTreeDepth>""" + maxTreeDepth + """</maxTreeDepth>
    <disableTree>""" + disableTree + """</disableTree>
    <disableNodeSyntaxColoring>""" + disableNodeSyntaxColoring + """</disableNodeSyntaxColoring>
    <nodeLimit>""" + nodeLimit + """</nodeLimit>
    <bootMode>""" + bootMode + """</bootMode>
</Eva>
"""
    
		XML.save("EvaConfig.xml", XML.loadString(xmlString))
	}
     
  init
}





