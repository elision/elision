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

import ornl.elision.gui.EvaConfig
import ornl.elision.gui.mainGUI
import ornl.elision.gui.console.ConsolePanel

import swing._
import swing.BorderPanel.Position._

/** The console menu for Eva's menu bar. */
object ConsoleMenu {
  // Console menu	
	val consoleMenu = new Menu("Console")
	consoleMenu.mnemonic = event.Key.C
        
    // Set Maximum Lines : Opens dialog to change the maximum lines in the onboard console
		val setMaxLinesItem = new MenuItem(Action("Set Maximum Lines") {
			val maxLinesDia = new MaxLinesDialog
		} )
		setMaxLinesItem.mnemonic = event.Key.L
        
    /** Constructs a Console menu structure appropriate for the given mode. */
    def apply(mode : String) : Menu = {
      consoleMenu.contents.clear
      
      mode match {
        case "Elision" =>
            consoleMenu.contents += setMaxLinesItem 
        case _ =>
            consoleMenu.contents += setMaxLinesItem    
      }
      
      consoleMenu
    }
}


/** The dialog window for the "Console > Set Maximum Lines" menu item */
class MaxLinesDialog extends Dialog {
  this.title = "Set Maximum Lines"
  val inset = 3
  
  val linesInput = new TextField(10) { 
    listenTo(keys) 
    reactions += { case e : swing.event.KeyTyped => if(e.char == '\n') enterInput(text) }
    text = "" + EvaConfig.replMaxLines
  }
  val okBtn = new Button(Action("OK") {enterInput(linesInput.text)})
  val cancelBtn = new Button(Action("Cancel") { close } )
  
  contents = new BorderPanel {
    border = new javax.swing.border.EmptyBorder(inset,inset,inset,inset)
    val minLines = ConsolePanel.infiniteMaxLines
    layout( new GridPanel(2,1) { 
              contents += new Label("Enter max lines: (integer >= " + minLines + ")")
              contents += new Label("(<" + minLines + " will make there be no maximum)") 
          } ) = North
    layout(linesInput) = Center
    layout(new FlowPanel {
      contents += okBtn
      contents += cancelBtn
    } ) = South
  }
  
  
  /** 
   * processes the input for the dialog when the user clicks OK or presses Enter 
   * @param input		The input string being evaluated as the new value for the Console's maximum lines.
   */
  private def enterInput(input : String) : Unit = {
    // if the input is an integer > 0, proceed to set the decompression depth to the input. 
    // Otherwise, just close the dialog.
    
    try {
      val fieldInt = input.toInt
      EvaConfig.replMaxLines = fieldInt
      EvaConfig.save
      close
    } catch {
      case _ : Throwable =>
        Dialog.showMessage(mainGUI.visPanel, input + " is not an integer.", "Input Error", Dialog.Message.Error)
    }
  }
  
  // open the dialog when it is finished setting up
  open
}








