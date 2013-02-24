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

package ornl.elision.gui.elision.sprites

import ornl.elision.gui.elision._
import ornl.elision.gui.trees._

class ElisionTreeSprite extends TreeSprite(0,0) {
  /** Elision syntax formatter. */
  override val formatter = new ornl.elision.gui.syntax.SyntaxFormatter(ornl.elision.gui.elision.EliRegexes, true, true)
	
  /** If false, syntax coloring is disabled for the entire tree. */
  syntaxColoring = !ornl.elision.gui.mainGUI.config.disableNodeSyntaxColoring
}


/** 
 * Eva's welcome message tree for Elision mode.
 */
object ElisionWelcomeTree extends ElisionTreeSprite {
  makeRoot("root")
    root.makeChild("Welcome to the ") // addChild("Welcome to the ",realroot)
    root.makeChild("Elision Visualization Assistant (Eva)!") // addChild("Elision Visualization Assistant (Eva)!",realroot)
      root(1).makeChild("To create a rewrite tree visualization,") // addChild("To create a rewrite tree visualization,", root2)
      root(1).makeChild("simply do one of the following: ") // addChild("simply do one of the following: ",root2)
        root(1)(1).makeChild("Enter input into the ") // addChild("Enter input into the ", node2)
        root(1)(1).makeChild("onboard Elision REPL, below.") // addChild("onboard Elision REPL, below.", node2)
        root(1)(1).makeChild("OR") // addChild("OR",node2)
        root(1)(1).makeChild("Use File -> Open to open a file ") // addChild("Use File -> Open to open a file ",node2)
        root(1)(1).makeChild("containing Elision input.") // addChild("containing Elision input.", node2)
}

