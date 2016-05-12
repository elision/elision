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

package ornl.elision.gui.menus

import swing._
import scala.swing.BorderPanel.Position._
import scala.concurrent.ops._
import sys.process._
import java.io._
import java.awt.Graphics2D

import sage2D.images.ImageLoader

import ornl.elision.gui.mainGUI

/** A dialog window containing help docs for the Elision GUI */
class EliHelpDialog extends Dialog {
	title = "Elision Mode Help"
	
	background = new Color(0xBBBBff)
	
	contents = new ScrollPane {
		val inset = 3
		border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
		
		val helpContents = new TextArea(HelpDialog.helpText,40,60) {
			editable = false
			border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset + 10)
			font = new java.awt.Font("Lucida Console", java.awt.Font.PLAIN, 12 )
		}
		contents = helpContents
	}
	
	open
}

/** A dialog window containing version information for the Elision GUI */
class EliAboutDialog extends Dialog {
	title = "Elision Mode About"
	val inset = 3
  background = new Color(0xBBBBff)

  val evaIcon = ImageLoader.loadPath("EvaIcon.png")
  
  val infoPaneContents = 
"""<b><u>Elision Visualization Assistant</u></b><br/>
Copyright (c) 2012 by UT-Battelle, LLC. <br/>
All rights reserved. <br/>
Homepage: <a href='""" + ornl.elision.util.Version.web + """'>""" + ornl.elision.util.Version.web + """</a>
"""
    
  contents = new BoxPanel(Orientation.Vertical) {
    /** Panel containing the EVA icon and version/build info. */
    val infoPane = new BoxPanel(Orientation.Horizontal) {
      border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
      
      /** The panel containing the Eva graphical icon. */
      contents += new FlowPanel {
        // Wait for the icon's image to finish loading before setting this panel's size.
        val imageLoader = new ImageLoader(this.peer)
        imageLoader.addImage(evaIcon)
        imageLoader.waitForAll

        preferredSize = new Dimension(evaIcon.getWidth(null), evaIcon.getHeight(null))
        override def paint(g : java.awt.Graphics2D) : Unit = {
          super.paint(g)
          g.drawImage(evaIcon,0,0,null)
        }
      }
      
      /** Panel containing the version/build information and things. */
      contents += new EditorPane("text/html", infoPaneContents) {
        border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
        import javax.swing.event.HyperlinkListener
        import javax.swing.event.HyperlinkEvent
        import java.awt.Desktop
        class hyperlinkOpener extends HyperlinkListener {
          def hyperlinkUpdate(event : HyperlinkEvent) : Unit = {
            if(event.getEventType == HyperlinkEvent.EventType.ACTIVATED && Desktop.isDesktopSupported) {
              try {
                Desktop.getDesktop.browse(new java.net.URI(ornl.elision.util.Version.web))
              } 
              catch { 
                case _: Throwable => 
                  Dialog.showMessage(mainGUI.visPanel, "Could not open Elision website.", "Error", Dialog.Message.Error)
              }
            }
          }
        }
        
        editable = false
        opaque = false
        peer.addHyperlinkListener(new hyperlinkOpener)
      }
    }
    
    val licensePane = new ScrollPane {
        horizontalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Never
        verticalScrollBarPolicy = scala.swing.ScrollPane.BarPolicy.Always
        border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
        
        val licenseContents = new TextArea(ornl.elision.HelpText.about, 20, 80) { // HelpDialog.licenseText, 20, 80) {
            editable = false
            border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset + 10)
            font = new java.awt.Font("Lucida Console", java.awt.Font.PLAIN, 10 )
        }
        contents = licenseContents
    }

    contents += infoPane
    contents += licensePane
	}
    
	open
}

/** Contains static text data used by EliHelpDialog */
object HelpDialog {
	val licenseText = 
"""Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

Collection of administrative costs for redistribution of the source code or
binary form is allowed. However, collection of a royalty or other fee in excess
of good faith amount for cost recovery for such redistribution is prohibited.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER, THE DOE, OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;
OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
ADVISED OF THE POSSIBILITY OF SUCH DAMAGE."""
    
    val helpText = 
"""
      _ _     _
  ___| (_)___(_) ___  _ __
 / _ \ | / __| |/ _ \| '_ \
|  __/ | \__ \ | (_) | | | |
 \___|_|_|___/_|\___/|_| |_|
The Elision Term Rewriter

Copyright (c) 2012 by UT-Battelle, LLC.
All rights reserved.

""" + licenseText + """

Contents
========
Menu options
GUI controls
License for Sage2D


README
======
This is the rewrite tree visualization GUI for the Elision term rewriter. This short file
describes how to use the GUI.

Visit the Elision web site at:
http://stacyprowell.com/wiki/doku.php?id=elision


Menu options
======
The File menu contains the following items:

* Open : Opens an open file dialog to open a file containing Elision term data. When the file 
	is opened, it is immediately processed by the REPL as input and a visualization tree is 
	constructed for the entire file. 

* Quit : Exits the application.

The View menu contains the following items:

* Reset camera : Resets the camera's position and zoom to its original state.

* Set decompression depth : Opens a dialog for the user to specify the depth out to which nodes 
	in the tree visualization are decompressed.

The Help menu contains the following items:

* Help : Opens this helpful help dialog.

* About : Opens a dialog containing product information about the Elision Visualization Assistant


GUI controls
======
Panning the camera : 
	Clicking and dragging the mouse anywhere in the visualization area will allow the user to 
	move the camera's position.
	
Zooming the camera :
	Scrolling up with your mouse's scroll wheel will cause the camera to zoom in towards the mouse. 
	Similarly, scrolling down with your mouse's scroll wheel will cause the camera to zoom away 
	from the mouse.

Selecting a term node :
	Clicking on a term node in the tree visualization will highlight that node and cause the tree to 
	become decompressed out to the current decompression depth relative to that node. That term node's
	properties will be displayed in the Atom Properties area.
	


"""
}


