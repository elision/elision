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

import ornl.elision.gui._
import ornl.elision.gui.menus.filefilters._

import swing._
import swing.BorderPanel.Position._

/** The menu for Elision-mode-specific things. */
object EliMenu {
    // Elision menu	
		
	val eliMenu = new Menu("Elision")
	eliMenu.mnemonic = event.Key.C
        
        // Create Rule from Node : Create a new rule from an Elision atom in an Eva tree.
		
		val makeRuleItem = new MenuItem(new Action("Create Rule from Node") {
        import javax.swing.KeyStroke
        import java.awt.event._
        accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_R, ActionEvent.CTRL_MASK))
        
        def apply = {
          GUIActor ! ("enableRuleMaker", true)
        }
		} )
		makeRuleItem.mnemonic = event.Key.R
        eliMenu.contents += makeRuleItem

}


class RulePredDialog(val lhs : String) extends Dialog {
  this.title = "Create Rule from Node"
  val inset = 3
  
  val linesInput = new TextField(10) { 
    listenTo(keys) 
    reactions += { case e : swing.event.KeyTyped => if(e.char == '\n') enterInput(text) }
    text = ""
  }
  val okBtn = new Button(Action("OK") {enterInput(linesInput.text)})
  val cancelBtn = new Button(Action("Cancel") { close } )
  
  contents = new BorderPanel {
    border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
    layout( new GridPanel(2,1) { 
                contents += new Label("Enter new rule right-hand-side:")
                contents += new Label(lhs + " -> ")
            } ) = North
    layout(linesInput) = Center
    layout(new FlowPanel {
      contents += okBtn
      contents += cancelBtn
    } ) = South
  }
    
    
    /** 
     * processes the input for the dialog when the user clicks OK or presses Enter 
     * @param input		The input string for the rule's RHS.
     */
    private def enterInput(input : String) : Unit = {
      // if the input is an integer > 0, proceed to set the decompression depth to the input. 
      // Otherwise, just close the dialog.
      val rhs = input
      
      try {
        val openDirectory = EvaConfig.lastOpenPath
        val fc = new FileChooser(new java.io.File(openDirectory))
        fc.fileFilter = new EliFileFilter
        val result = fc.showSaveDialog(null)
        val selFile = fc.selectedFile
        if(selFile != null && result == FileChooser.Result.Approve) {
          EvaConfig.lastOpenPath = selFile.getParent
          EvaConfig.save
    
          var filePath = selFile.getPath
          if(!filePath.endsWith(".eli")) {
            filePath += ".eli"
          }
          
          // create the parse string for the new rule declaration.
          var ruleDecl = """decl.{rule
    """ + lhs + """
    -> 
    """ + rhs
          
          // Append the default ruleset if no ruleset was provided.
          if(!rhs.contains("#rulesets")) {
            ruleDecl += """
    
    #rulesets DEFAULT"""
                ruleDecl += """
}
"""
          }
          
          // save the rule to the file.
          val fw = new java.io.FileWriter(filePath)
          fw.write(ruleDecl)
          fw.close
          
          // declare the rule in our context.
          GUIActor ! "IgnoreNextTree"
          GUIActor ! ("ReplInput", ruleDecl)
        }
        // close the dialog when we finish processing input
        close
      } catch {
        case _: Throwable =>
          Dialog.showMessage(mainGUI.visPanel, "Failed to create the rule.", "Error", Dialog.Message.Error)
      }
    }
    
    // open the dialog when it is finished setting up
    open
}







