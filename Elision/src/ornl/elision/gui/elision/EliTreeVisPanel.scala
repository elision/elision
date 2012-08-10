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

import java.awt._
import ornl.elision.gui._
import ornl.elision.gui.trees._

/** An extension of TreeVisPanel that handles some Elision-specific functions. */
class EliTreeVisPanel extends TreeVisPanel {
    var selectingRuleLHS = false
    
    override def timerLoop : Unit = {
        super.timerLoop
        
        // keyboard input
/*        // the escape key will cancel out of whatever interactive tool is being used.
        if(keyboardIn.isPressed(swing.event.Key.Escape)) {
            selectingRuleLHS = false
        }
        if(keyboardIn.justPressed(swing.event.Key.Home)) {
            mainGUI.consolePanel.console.requestFocusInWindow
            mainGUI.consolePanel.tos ! ("bumpCaret", true)
        }
        // CTRL + O is a shortcut for File > Open
        if(keyboardIn.isPressed(swing.event.Key.Control) && keyboardIn.justPressed(swing.event.Key.O)) 
            mainGUI.guiMenuBar.openItem.doClick
        // CTRL + R is a shortcut for Elision > Create Rule from Node
        if(keyboardIn.isPressed(swing.event.Key.Control) && keyboardIn.justPressed(swing.event.Key.R)) 
            selectingRuleLHS = true
        // F1 is a shortcut for Help > Help
        if(keyboardIn.justPressed(swing.event.Key.F1)) 
            mainGUI.guiMenuBar.helpItem.doClick
            */
        
    }
    
    override def selectNode(clickedNode : NodeSprite) : Unit = {
        super.selectNode(clickedNode)
        
        // Interactive rule creation: User selects an atom node for the LHS and then 
        // inputs the RHS and saves it to an eli file.
        if(clickedNode != null && selectingRuleLHS && !clickedNode.isComment) {
            val ruleDia = new RulePredDialog(clickedNode.term)
            selectingRuleLHS = false
        }
    }
    
    override def mainPaint(g : Graphics2D) : Unit = {
        super.mainPaint(g)
        
        val helpPromptY = (this.size.getHeight-10).toInt
        g.setColor(new Color(0x000000))
        if(selectingRuleLHS) {
            g.drawString("Create Rule from Node: Click a node representing an atom to be the left-hand-side of the rule. Press Esc to cancel.", 10,helpPromptY)
        }
    }
}







