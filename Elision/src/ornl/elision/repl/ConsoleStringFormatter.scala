/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com).
 * All rights reserved.  http://stacyprowell.com
 *
 * Redistribution and use in source and binary forms, with or without modification,
 * are permitted provided that the following conditions are met:
 *
 *  - Redistributions of source code must retain the above copyright notice, this
 *    list of conditions and the following disclaimer.
 *  - Redistributions in binary form must reproduce the above copyright notice,
 *    this list of conditions and the following disclaimer in the documentation
 *    and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 * CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR
 * TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
package ornl.elision.repl

import java.awt.Color

import ornl.elision.gui.elision.EliRegexes
import ornl.elision.gui.elision.EliWebColors
import ornl.elision.syntax._

object ConsoleStringFormatter {
  
  val defaultEsc = "\u001B[0m"
  val redEsc = "\u001B[31m"
  val greenEsc = "\u001B[32m"
  val goldEsc = "\u001B[33m"
  val blueEsc = "\u001B[34m"
  val pinkEsc = "\u001B[35m"
  val cyanEsc = "\u001B[36m"
  val greyEsc = "\u001B[37m"
  
  
  /** 
   * Converts a color used by EliRegexes to an appropriate escape string
   * for colored console output. 
   */
  def color2EscString(color : String) : String = {
    val colorStr = color // "#" + Integer.toHexString(color.getRGB).drop(2).toLowerCase()
    
    if(colorStr.equals(EliWebColors.comments)) {
      greenEsc
    }
    else if(colorStr.equals(EliWebColors.stringLits)) {
      greyEsc
    }
    else if(colorStr.equals(EliWebColors.vars)) {
      pinkEsc
    }
    else if(colorStr.equals(EliWebColors.keywords)) {
      goldEsc
    }
    else if(colorStr.equals(EliWebColors.constants)) {
      blueEsc
    }
    else if(colorStr.equals(EliWebColors.types)) {
      cyanEsc
    }
    else if(colorStr.equals(EliWebColors.properties)) {
      blueEsc
    }
    else {
      defaultEsc
    }
  }
  
  
  /**
   * Applies coloring escape sequences to an Elision parse string.
   * @param src   the source Elision parse string.
   * @param width the width of the console the string is being printed in.
   * @return      The parse string with coloring escape sequences.
   */
  def format(src : String, width : Int = 50) : String = {
    val formatter = new SyntaxFormatter(EliRegexes)
    val sfStr = formatter.format(src, width)
    
    var result = ""
      
    // Use the SyntaxFormattedString to construct our result string with color
    // escape sequences inserted in the correct places.
    for( line <- sfStr.lines) {
      for((i, substr) <- line.substrings) {
        val clrEsc = color2EscString(substr.color)
        result += clrEsc + substr.toString
      }
      result += "\n"
    }
    
    result + defaultEsc
  }
  
}
