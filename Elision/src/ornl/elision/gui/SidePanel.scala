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

import swing._
import scala.swing.BorderPanel.Position._
import swing.TabbedPane

/** Used to display information about the currently selected node. */
class SidePanel extends BoxPanel(Orientation.Vertical) {
	background = GUIColors.bgColor
	
	// the current tree visualization Publisher.
	var allButTheSong : Publisher = null
	
	/** Used for setting border spacings in this panel */
	val inset = SidePanel.inset
	border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
  preferredSize = new Dimension(SidePanel.preferredWidth, SidePanel.parsePanelHeight)

  // This panel organizes a much of other panels into tabs. The panels available from the tabs depends on Eva's current mode.
  val tabs = new TabbedPane
  contents += tabs

  // Elision pages: 
  // Parse String tab
  val parsePanel = new elision.EliParseStringPane
  val parsePage = new TabbedPane.Page("Atom Parse String", parsePanel)
  
  // Properties tab
  val propsPanel = new elision.EliAtomPropsPane
  val propsPage = new TabbedPane.Page("Atom Properties", propsPanel)

	/** Changes the tabs in the SidePanel to match a given Eva mode. This is called by mainGUI's changeMode method. */
	def changeMode(mode : String) {
    tabs.pages.clear
    
    mode match {
      case "Elision" =>
          tabs.pages += parsePage
          tabs.pages += propsPage
      case "Welcome" => // ignore messages.
      case _ =>
    }
  }
    
  override def paint(g : Graphics2D) : Unit = {
    super.paint(g)
  }
    
  reactions += {
    case nle : sage2D.event.NewLevelEvent => 
      // Our visualization panel changed levels, so change the Level Publisher we're listening to.
      val level = nle.level
      
      if(allButTheSong != null) {
        deafTo(allButTheSong) // I just made a Touhou reference. :)
      }
      
      listenTo(mainGUI.visPanel.curLevel)
      allButTheSong = mainGUI.visPanel.curLevel
      
    case nce : trees.NodeClickedEvent =>
       val clickedNode = nce.node
       propsPanel.textArea.text = clickedNode.properties
       parsePanel.parseStringFormat(clickedNode.term, clickedNode.isComment)
  }
}


/** Contains constants used by the SidePanel */
object SidePanel {
	val preferredWidth = 300
	val parsePanelHeight = 200
  val inset = 3
}

