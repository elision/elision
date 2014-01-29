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
package ornl.elision.syntax

import java.awt.Color
import scala.collection.mutable.ListBuffer
import scala.util.matching._

import ornl.elision.gui._
import ornl.elision.util.AbbrevException

/** 
 * An object for applying syntax formatting to a string. 
 * @param regexes       The regex information for the syntax used by this.
 * @param doesColoring  If true, this will apply formatting for syntax coloring.
 */
class SyntaxFormatter (val regexes : SyntaxRegexes = null, var doesColoring : Boolean = true, var doesIndent : Boolean = false) {
  
  if(regexes == null) {
    doesColoring = false
  }
  
  /** A string representing a tab in way that HTML can understand. */
  var htmlTab = """&nbsp;&nbsp;&nbsp;"""
  
  /** A string mimicing a tab. It's really just a bunch of '~'s. */
  var _fauxTab = """~~~"""
  
  /** A string of spaces mimicing a tab. */
  var spaceTab = "   "
  
  /** The number of characters making up a tab. Default 3. */
  def tabSize = _fauxTab.size
  
  var showColorData = false
  
  def tabSize_= (size : Int):Unit = {
    _fauxTab = ""
    htmlTab = ""
    spaceTab = ""
    for(i <- 1 to size) {
      _fauxTab += "~"
      htmlTab += SyntaxFormatter.htmlSpace
      spaceTab += " "
    }
  }
  
  
  /**
   * Consumes the parse string text to construct lists for correct start and end positions
   * for <font color=~~~~> </font> tags to be inserted into the parse string. 
   * @param txt      the parse string that is having highlighting applied to it.
   * @param prevChomped  a count of how many characters in txt have already been processed for highlighting.
   * @param starts    an empty list of insertion indices for <font> tags. This will be populated by this method.
   * @param colors    an empty list of web colors coresponding to the indices in starts. This will be populated by this method.
   * @param ends      an empty list of insertion indices for </font> tags. This will be populated by this method.
   */
  def _applyRegexes(txt: String, prevChomped : Int, starts : ListBuffer[Int], colors : ListBuffer[String], ends : ListBuffer[Int], depth : Int = 0) : Unit = {
    if(regexes == null) {
      return
    }
    
    // a consumable copy of the source string.
    var text = txt
    
    // keeps track of the number of processed characters.
    var chompedChars = prevChomped
    
    // populate our lists of font tag data by having our regular expressions 
    // consume the text until no more regular expressions can be applied to it.
    while(text != "") {
      var bestRegex : Regex = null
      var bestStart : Int = -1
      var bestEnd : Int = -1
      var bestRecStart : Int = -1
      var bestRecEnd : Int = -1
      
      // iterate over our list of regexes and find the one the can be applied the earliest.
      for(regex <- regexes.rList) {
        regex.findFirstMatchIn(text) match {
          case Some(myMatch : scala.util.matching.Regex.Match) =>
            // If this regex can be applied earliest, remember it. 
            if(bestStart == -1 || myMatch.start < bestStart) {
              bestRegex = regex
              bestStart = myMatch.start
              bestEnd = myMatch.end
              
              // if the regex requires recursive formatting, figure out between 
              // what indices to apply it. 
              // Otherwise discard old recursive formatting data.
              val recGroup = regexes.recursableMap(bestRegex)
              if(recGroup != -1) {
                bestRecStart = myMatch.start(recGroup)
                bestRecEnd = myMatch.end(recGroup)
              }
              else {
                bestRecStart = -1
                bestRecEnd = -1
              }
            } // endif
          case _ => 
            // this regex could not be applied.
        } // endmatch
      } // endfor
      
      if(bestRegex == null)  {
        // No more regexes could be applied to the remaining string. 
        // We are done.
        text = ""
      } 
      else {
        // We have determined the best regex that could be applied to the 
        // remaining string. 
        // Add its coloring/index information to our lists.
        val color = regexes.colorMap(bestRegex)
        starts += chompedChars + bestStart
        colors += color
        
        // Determine indices of recursive highlighting if needed.
        if(bestRecStart != -1) {
          ends += chompedChars + bestRecStart
          
          _applyRegexes(text.substring(bestRecStart,bestRecEnd), chompedChars + bestRecStart, 
                        starts, colors, ends, depth+1)
          
          starts += chompedChars + bestRecEnd
          colors += color
        }
        
        // Always chomp at least 1 character.
        if(bestEnd <= 0) {
          bestEnd = 1;
        }
        
        ends += chompedChars + bestEnd
        
        // consume the text we processed for applying the regex.
        text = text.drop(bestEnd)
        chompedChars += bestEnd
      } // endifelse
    } // endwhile
  }
  
  
  
  /** 
   * Applies linewrapping and indentation (if doesIndent is true) to a string.
   * @param txt       the parse string for which we are computing where to insert 
   *                  line breaks and indentations.
   * @param maxCols   the character width of the component the parse string is 
   *                  going to be displayed in. 
   * @return          txt with linewrapping and indentation applied.
   */
  def _lineWrap(txt : String, maxCols : Int) : String = { 
    if(maxCols <= 0) {
      return txt
    }
    
    // consumed text data
    var edibleTxt = txt
    var result = ""
    
    // indentation data
    var indents = 0
    val maxIndents = maxCols/tabSize - 1
    var tabsSize = 0
    
    // Apply line wrapping as long as the remaining text size is longer than 
    // the maximum allowed columns. 
    while(edibleTxt.size > maxCols - tabsSize) {
    
      // Apply word wrapping.
      val (breakPt, isNewLine) = _wordWrap(edibleTxt, maxCols - tabsSize)
      
      // chomp the characters before the break point. Om nom nom...
      var curLine = edibleTxt.take(breakPt)
      
      result += curLine
      if(!isNewLine)
        result += "\n"
      
      edibleTxt = edibleTxt.drop(breakPt)
      
      // Apply indention if our syntax uses it.
      if(doesIndent) {
        // Count the number of indents that will be needed for the next line.
        indents += curLine.count(ch => (ch == '{' || ch == '(' || ch == '[') )
        indents -= curLine.count(ch => (ch == '}' || ch == ')' || ch == ']') )
        indents = math.max(0,indents)
        
        // If line starts with some number of block end characters }, ), or ] 
        // subtract that from our number of indentations to apply.
        var j = 0
        while(j < maxCols - tabsSize) {
          if(j >= edibleTxt.size) 
            j = maxCols
          else {
            val ch = edibleTxt.charAt(j)
              
            if(ch == '}' || ch == ')' || ch == ']')
              indents -= 1
            else
              j = maxCols
          }
          j += 1
        } // endwhile
        
        // apply the number of indentations we counted.
        tabsSize = 0
        for(i <- 0 until indents % maxIndents) {
          //edibleTxt = spaceTab + edibleTxt
          result += spaceTab
          tabsSize += spaceTab.length
        }
      } // endif
    } // endwhile
    result += edibleTxt

    result
  }
  
  
  
  /** 
   * Finds where to insert a line break in a single line for correct word wrapping. 
   * @param txt       This is the line of text in which we are figuring out where 
   *                  to insert a line break that will satisfy word wrapping.
   * @param maxCols   The character width of the component the parse string is 
   *                  going to be displayed in. 
   * @return          A tuple containing the index in txt in which our wordwrap-friendly 
   *                  linebreak will be inserted and a boolean that is true iff
   *                  the linebreak resulted from encountering a \n character.
   */
  def _wordWrap(txt : String, maxCols : Int) : (Int, Boolean) = {
    var index = maxCols
    var foundIt = false
    
    // if a '\n' exists before maxCols in txt, then just return its index.
    val newLineIndex = txt.indexOf('\n')
    if(newLineIndex != -1 && newLineIndex < maxCols) {
      return (newLineIndex+1, true)
    }
    
    // search backwards from maxCols until we reach a nonalphanumeric character.
    while(!foundIt && index > 1) {
      index -= 1
      foundIt = !_isWrappable(txt.charAt(index))
    }
    
    if(!foundIt) 
      (maxCols, false)
    else 
      (index, false)
  }
  
  /** 
   * Checks if a character is an alphanumeric. 
   * @param c    the character we are testing.
   * @return    if c is an alphanumeric character, true. Otherwise false.
   */
  def _isWrappable(c : Char) : Boolean = {
    return (c > ' ' && c != '(' && c != ')' && c != '[' && c != ']' && c != '{' && c != '}')
  }
  
  
  
  /** 
   * Applies line/word wrapping, indentation (if doesIndent is true), and
   * syntax coloring (if doesColoring is true) to a String. 
   * @param text        the source string which we are formatting.
   * @param maxCols     The character width of the component the parse string is 
   *                    going to be displayed in. 
   * @return            A SyntaxFormattedString object containing the formatted
   *                    String and its colored components.
   */
  def format(text : String, maxCols : Int = 50) : SyntaxFormattedString = {
    var result = _lineWrap(text, maxCols)
    
    val starts = new ListBuffer[Int]
    val colors = new ListBuffer[String]
    val ends = new ListBuffer[Int]

    if(doesColoring) {
      _applyRegexes(result, 0, starts, colors, ends)
      
      if(showColorData) {
        System.err.println("----Color Data----")
        System.err.println("starts: " + starts)
        System.err.println("colors: " + colors)
        System.err.println("ends: " + ends)
      }
    }
    
    new SyntaxFormattedString(result, starts, colors, ends)
  }
  
  
  
  /** 
   * Applies HTML-style formatting to a parse string for use in an EditorPane. 
   * @param text        The parse string to which we are applying HTML tags 
   *                    for formatting.
   * @param maxCols     The character width of the component the parse string 
   *                    is going to be displayed in. 
   * @return            The parse string with HTML tags inserted for formatting.
   */
  def htmlFormat(text : String, maxCols : Int = 50) : String = {
    val formattedString = format(text, maxCols)
    
    var resultString = ""
    for(i <- 0 until formattedString.lines.size) {
      var resultLine = ""
      val line = formattedString.lines(i)
      for((_, csubstr) <- line.substrings) {
        var substr = csubstr.toString
        substr = substr.replaceAllLiterally("<", SyntaxFormatter.htmlLT)
        substr = substr.replaceAllLiterally(">", SyntaxFormatter.htmlGT)
        substr = substr.replaceAllLiterally(" ", SyntaxFormatter.htmlSpace)
        if(doesColoring)
          resultLine += "<font color=\"" + csubstr.color + "\">" + substr + "</font>"
        else
          resultLine += substr
      }
      
      if(i < formattedString.lines.size-1)
        resultLine += SyntaxFormatter.htmlLineBreak
        
      resultString += resultLine
    }
    resultString
  }
  
  def _colorToWebString(color : java.awt.Color) : String = {
    "#" + Integer.toHexString(color.getRGB).drop(2)
  }
  
  
  /** 
   * Only replaces < > with &lt; &gt; respectively. Also replaces lines breaks with <br/> tags. 
   * @param text    The parse string we are converting to be HTML-friendly.
   * @param maxCols  The character width of the component the parse string is going to be displayed in. 
   * @return       The parse string with < > characters replaced with &lt; &gt; respectively. 
   */
  def minHtmlFormat(text : String, maxCols : Int = 80) : String = {
    val tempDoesIndent = doesIndent
    doesIndent = false
    
    val tempDoesColoring = doesColoring
    doesColoring = false
    
    val resultString = htmlFormat(text, maxCols)
    
    doesIndent = tempDoesIndent
    doesColoring = tempDoesColoring
    
    resultString
  }
}


/** provides methods for doing syntax formatting for text using a SyntaxRegexes object. */
object SyntaxFormatter {

  /** A string representing an html space. */
  val htmlSpace = "&nbsp;"
  
  /** A string representing an html line break. */
  val htmlLineBreak = "<br/>"
  
  /** A string representing an html less than. */
  val htmlLT = "&lt;"
  
  /** A string representing an html greater than. */
  val htmlGT = "&gt;"
  
  /** Used to find <br/> tags in an HTML string. */
  val htmlNewLineRegex = new Regex("""(<br/>)""",
    "all")
    
  /** Used to find <font> start tags in an HTML string. */
  val htmlFontStartRegex = new Regex("""(<font.*?>)""",
    "all")
    
  /** Used to find </font> end tags in an HTML string. */
  val htmlFontEndRegex = new Regex("""(</font>)""",
    "all")
}



