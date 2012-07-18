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

import scala.collection.mutable.ListBuffer
import util.matching._



/** provides methods for doing syntax formatting for text using a SyntaxRegexes object. */
object SyntaxFormatter {
	
	/** A string representing a tab in way that HTML can understand. */
	val tab = """&nbsp;&nbsp;&nbsp;"""
	
	/** A string mimicing a tab. It's really just a bunch of '~'s. */
	val fauxTab = """~~~"""
	
	/** The number of characters making up a tab. */
	val tabSize = fauxTab.size
	
	/** Used to find <br/> tags in an HTML string. */
	val htmlNewLineRegex = new Regex("""(<br/>)""",
		"all")
		
	/** Used to find <font> start tags in an HTML string. */
	val htmlFontStartRegex = new Regex("""(<font.*?>)""",
		"all")
		
	/** Used to find </font> end tags in an HTML string. */
	val htmlFontEndRegex = new Regex("""(</font>)""",
		"all")
    
    /** The syntax regexes currently being used by the SyntaxFormatter for coloring. */
    var regexes : SyntaxRegexes = null
	
	/**
	 * Consumes the parse string text to construct lists for correct start and end positions
	 * for <font color=~~~~> </font> tags to be inserted into the parse string. 
	 * @param txt			the parse string that is having highlighting applied to it.
	 * @param prevChomped	a count of how many characters in txt have already been processed for highlighting.
	 * @param starts		an empty list of insertion indices for <font> tags. This will be populated by this method.
	 * @param colors		an empty list of web colors coresponding to the indices in starts. This will be populated by this method.
	 * @param ends			an empty list of insertion indices for </font> tags. This will be populated by this method.
	 */
	def applyRegexes(txt: String, prevChomped : Int, starts : ListBuffer[Int], colors : ListBuffer[String], ends : ListBuffer[Int], depth : Int = 0) : Unit = {
		var text = txt
		var chompedChars = prevChomped
		
		// populate our lists of font tag data by having our regular expressions consume the text until
		// no more regular expressions can consume any of the text.
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
						if(bestStart == -1 || myMatch.start < bestStart) {
							bestRegex = regex
							bestStart = myMatch.start
							bestEnd = myMatch.end
							
							// if the regex requires recursive formatting, figure out between what indices to apply the recursive formatting if it is chosen.
							val recGroup = regexes.recursableMap(bestRegex)
							if(recGroup != -1) {
								bestRecStart = myMatch.start(recGroup)
								bestRecEnd = myMatch.end(recGroup)
							}
							else {
								bestRecStart = -1
								bestRecEnd = -1
							}
						}
					case _ => // this regex could not be applied.
				}
			}
			if(bestRegex == null)	text = ""
			else {
				// now that we have figured out which regex to apply next, add its coloring/indexing information to our lists.
				val color = regexes.colorMap(bestRegex)
				starts += chompedChars + bestStart
				colors += color
				
				// apply recursive highlighting if needed.
				if(bestRecStart != -1) {
					ends += chompedChars + bestRecStart
					
					applyRegexes(text.substring(bestRecStart,bestRecEnd), chompedChars + bestRecStart, starts, colors, ends, depth+1)
					
					starts += chompedChars + bestRecEnd
					colors += color
				}
				
				ends += chompedChars + bestEnd
				
				text = text.drop(bestEnd)
				chompedChars += bestEnd
			}
		}
	}
	
	
	/** 
	 * Enforces wrapped lines in a parse string so that the editor pane doesn't grossly resize this panel.
	 * @param txt		the parse string for which we are computing where to insert line breaks and indentations.
	 * @param maxCols	the character width of the component the parse string is going to be displayed in. 
	 * @return 			two ListBuffers containing indices in txt to insert line breaks and tabs at, respectively.
	 */
	def enforceLineWrap(txt : String, maxCols : Int, replaceNewLines : Boolean = true) : (ListBuffer[Int], ListBuffer[Int]) = {
		var chompedChars = 0
		var edibleTxt = txt
		val breaks = new ListBuffer[Int]
		val tabs = new ListBuffer[Int]
		
		var indents = 0
		val maxIndents = maxCols/tabSize - 1
		
		while(edibleTxt.size > maxCols) {
			// enforce word wrapping while deciding where to insert a line break.
			val (breakPt, addToList) = enforceWordWrap(edibleTxt, maxCols, replaceNewLines)
			if(addToList) breaks += chompedChars + breakPt - tabSize*(indents % maxIndents) //maxCols
			chompedChars += breakPt - tabSize*(indents % maxIndents)
			
			// update the number of indents that will be needed for the next line.
			val curLine = edibleTxt.take(breakPt)

			indents += curLine.count(ch => (ch == '{' || ch == '(' || ch == '[') )
			indents -= curLine.count(ch => (ch == '}' || ch == ')' || ch == ']') )
			indents = math.max(0,indents)
			
			// chomp the characters before the break point. Om nom nom...
			edibleTxt = edibleTxt.drop(breakPt)
			
			// insert indentations
			var j = 0
			while(j < maxCols) {
				if(j >= edibleTxt.size) j = maxCols
				else {
					val ch = edibleTxt.charAt(j)
					if(!(ch == '}' || ch == ')' || ch == ']')) j = maxCols
					else indents -= 1
				}
				j += 1
			}
			for(i <- 0 until indents % maxIndents) {
				edibleTxt = fauxTab + edibleTxt
				tabs += chompedChars
			}
		}
		// check if there is one more '\n' character that needs to be accounted for in the leftover edibleTxt.
		val lastLineBreak = edibleTxt.indexOf('\n')
		if(lastLineBreak != -1 && replaceNewLines) breaks += chompedChars + lastLineBreak
		(breaks, tabs)
	}
	
	
	/**
     * Determines where to insert word/line-wrap friendly break points in a string without applying indentation.
     * @param txt               The string we are determining line breaks for.
     * @param maxCols	        The character width of the component the parse string is going to be displayed in. 
     * @param replaceNewLines   A flag that tells this method to mark indices where it finds '\n' characters. Default true.
     * @return                  A list of indices where new lines should be inserted to enforce word/line wrapping.
     */
	def enforceLineWrapWithNoTabs(txt : String, maxCols : Int, replaceNewLines : Boolean = true) : ListBuffer[Int] = {
		var chompedChars = 0
		var edibleTxt = txt
		val breaks = new ListBuffer[Int]
		
		while(edibleTxt.size > maxCols) {
			// enforce word wrapping while deciding where to insert a line break.
			val (breakPt, addToList) = enforceWordWrap(edibleTxt, maxCols, replaceNewLines)
			if(addToList) breaks += chompedChars + breakPt //maxCols
			chompedChars += breakPt
			
			// chomp the characters before the break point. Om nom nom...
			edibleTxt = edibleTxt.drop(breakPt)
		}
		// check if there is one more '\n' character that needs to be accounted for in the leftover edibleTxt.
		val lastLineBreak = edibleTxt.indexOf('\n')
		if(lastLineBreak != -1 && replaceNewLines) breaks += chompedChars + lastLineBreak
		breaks
	}
	
	
	
	/** 
	 * Finds where to insert a line break in a single line for correct word wrapping. 
	 * @param txt 		This is the line of text in which we are figuring out where to insert a line break that will satisfy word wrapping.
	 * @param maxCols	The character width of the component the parse string is going to be displayed in. 
	 * @return			the index in txt in which our wordwrap-friendly linebreak will be inserted.
	 */
	def enforceWordWrap(txt : String, maxCols : Int, replaceNewLines : Boolean = true) : (Int, Boolean) = {
		var index = maxCols
		var foundIt = false
		
		// if a '\n' exists before maxCols in txt, then just return its index.
		val newLineIndex = txt.indexOf( '\n' )
		if(newLineIndex != -1 && newLineIndex < maxCols) return (newLineIndex+1, replaceNewLines)
		
		while(!foundIt && index > 1) {
			index -= 1
			foundIt = !isWordChar(txt.charAt(index))
		}
		
		if(!foundIt) (maxCols, true)
		else (index, true)
	}
	
	/** 
	 * Checks if a character is an alphanumeric. 
	 * @param c		the character we are testing.
	 * @return		if c is an alphanumeric character, true. Otherwise false.
	 */
	def isWordChar(c : Char) : Boolean = {
		return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')
	}
	
	
	/** 
	 * Finds the locations of all angle brackets (< and >) in txt. 
	 * @param txt		The string we are finding the position of <> characters in.
	 * @return			Two lists containing the indices of the txt's < and > characters, respectively.
	 */
	def enforceAngleBracketReplacement(txt: String) : (ListBuffer[Int], ListBuffer[Int]) = {
		val ltMatches = new Regex("""([<])""",
		"all").findAllIn(txt).matchData
		val gtMatches = new Regex("""([>])""",
		"all").findAllIn(txt).matchData
		
		val ltList = new ListBuffer[Int]
		val gtList = new ListBuffer[Int]
		
		for(myMatch <- ltMatches) {
			ltList += myMatch.start
		}
		
		for(myMatch <- gtMatches) {
			gtList += myMatch.start
		}
		
		(ltList,gtList)
	}
	
	
	/** 
	 * Applies HTML-style formatting to a parse string for use in an EditorPane. 
	 * @param text					The parse string to which we are applying HTML tags for formatting.
	 * @param disableHighlight		Disables coloring tags if true.
	 * @param maxCols				The character width of the component the parse string is going to be displayed in. 
     * @param applyTabs             A flag for inserting indentations. Some REPL output is already indented. This should be false for that kind of text.
	 * @return						The parse string with HTML tags inserted for formatting.
	 */
	def applyHTMLHighlight(text : String, ddisableHighlight : Boolean = true, maxCols : Int = 50, applyTabs : Boolean = true) : String = {
		var result = text
        
        var disableHighlight = ddisableHighlight
        if(regexes == null) disableHighlight = true
		
		// inserting HTML tags alters the location of text in our string afterwards, so we need to keep track of how many characters are inserted when we inject our HTML tags.
		var insertedChars = 0
		
		// obtain the locations of angle brackets so we can convert them to be not interpretted by the EditorPane's HTML kit.
		val (ltList,gtList) = enforceAngleBracketReplacement(text)
		
		if(!disableHighlight) { // skip this whole block if we're disabling highlighting.
			// obtain the lists of data for the font tags.
			val starts = new ListBuffer[Int]
			val colors = new ListBuffer[String]
			val ends = new ListBuffer[Int]

			applyRegexes(text, 0, starts, colors, ends)
			
			// obtain the list of data for enforcing line breaks. (because HTMLEditorKit is dumb and doesn't enforce word-wrap)
			val (breaks, tabs) = if(applyTabs) enforceLineWrap(text,maxCols) else (enforceLineWrapWithNoTabs(text,maxCols), new ListBuffer[Int])
			
			// insert all of our <font></font> and <br/> tags in their appropriate places.
			while(!starts.isEmpty || !ends.isEmpty || !breaks.isEmpty || !tabs.isEmpty || !ltList.isEmpty || !gtList.isEmpty) {
				var tag = ""
				var insertLoc = 0
				var trimmedChars = 0
				
				if(!starts.isEmpty && (ends.isEmpty || starts(0) < ends(0)) && (breaks.isEmpty || starts(0) <= breaks(0)) && (tabs.isEmpty || starts(0) <= tabs(0)) && (ltList.isEmpty || starts(0) <= ltList(0)) && (gtList.isEmpty || starts(0) <= gtList(0))) {
					// insert a <font color=> tag
					tag = """<font color=""" + "\"" + colors(0) + "\"" + """>"""
					insertLoc = starts(0)
					starts.trimStart(1)
					colors.trimStart(1)
				} else if(!ends.isEmpty && (breaks.isEmpty || ends(0) <= breaks(0)) && (tabs.isEmpty || ends(0) <= tabs(0)) && (ltList.isEmpty || ends(0) <= ltList(0)) && (gtList.isEmpty || ends(0) <= gtList(0))) {
					// insert a </font> tag
					tag = """</font>"""
					insertLoc = ends(0)
					ends.trimStart(1)
				} else if(!breaks.isEmpty && (tabs.isEmpty || breaks(0) <= tabs(0)) && (ltList.isEmpty || breaks(0) <= ltList(0)) && (gtList.isEmpty || breaks(0) <= gtList(0))) {
					// insert a <br/> tag to enforce a line break
					tag = """<br/>"""
					insertLoc = breaks(0)
					breaks.trimStart(1)
				} else if(!tabs.isEmpty && (ltList.isEmpty || tabs(0) <= ltList(0)) && (gtList.isEmpty || tabs(0) <= gtList(0))) {
					// insert a tab
					tag = tab
					insertLoc = tabs(0)
					tabs.trimStart(1)
				} else if(!ltList.isEmpty && (gtList.isEmpty || ltList(0) < gtList(0))) { 
					tag = """&lt;"""
					insertLoc = ltList(0)
					ltList.trimStart(1)
					// trim out the '<'. 
					val (str1,str2) = result.splitAt(insertLoc + insertedChars)
					result = str1 + str2.drop(1)
					trimmedChars = 1
				} else {
					tag = """&gt;"""
					insertLoc = gtList(0)
					gtList.trimStart(1)
					// trim out the '>'. 
					val (str1,str2) = result.splitAt(insertLoc + insertedChars)
					result = str1 + str2.drop(1)
					trimmedChars = 1
				}
				
				val (str1, str2) = result.splitAt(insertLoc + insertedChars)
				result = str1 + tag + str2
				insertedChars += tag.size - trimmedChars
			}
		} else { // we still need to enforce line breaks even if we're not highlighting anything.
			// obtain the list of data for enforcing line breaks. (because HTMLEditorKit is dumb and doesn't enforce word-wrap)
			val (breaks, tabs) = if(applyTabs) enforceLineWrap(text,maxCols) else (enforceLineWrapWithNoTabs(text,maxCols), new ListBuffer[Int])
			
			// insert the line breaks.
			while(!breaks.isEmpty || !tabs.isEmpty || !ltList.isEmpty || !gtList.isEmpty) {
				var tag = ""
				var insertLoc = 0
				var trimmedChars = 0
				
				if(!breaks.isEmpty && (tabs.isEmpty || breaks(0) <= tabs(0)) && (ltList.isEmpty || breaks(0) <= ltList(0)) && (gtList.isEmpty || breaks(0) <= gtList(0))) {
					// insert a <br/> tag to enforce a line break
					tag = """<br/>"""
					insertLoc = breaks(0)
					breaks.trimStart(1)
				} else if(!tabs.isEmpty && (ltList.isEmpty || tabs(0) <= ltList(0)) && (gtList.isEmpty || tabs(0) <= gtList(0))) {
					// insert a tab
					tag = tab
					insertLoc = tabs(0)
					tabs.trimStart(1)
				} else if(!ltList.isEmpty && (gtList.isEmpty || ltList(0) < gtList(0))) { 
					tag = """&lt;"""
					insertLoc = ltList(0)
					ltList.trimStart(1)
					// trim out the '<'. 
					val (str1,str2) = result.splitAt(insertLoc + insertedChars)
					result = str1 + str2.drop(1)
					trimmedChars = 1
				} else {
					tag = """&gt;"""
					insertLoc = gtList(0)
					gtList.trimStart(1)
					// trim out the '>'. 
					val (str1,str2) = result.splitAt(insertLoc + insertedChars)
					result = str1 + str2.drop(1)
					trimmedChars = 1
				}
					
				val (str1, str2) = result.splitAt(insertLoc + insertedChars)
				result = str1 + tag + str2
				insertedChars += tag.size - trimmedChars
			}
		}
		// wrap the whole text in a font tag to give it the font face "Lucida Console" then return our final result.
		//result = """<div style="font-family:Lucida Console;font-size:12pt">""" + result + """</div>"""
		result
	}
	
	
	/** 
	 * Only replaces < > with &lt; &gt; respectively. Also does lines breaks. 
	 * @param text		The parse string we are converting to be HTML-friendly.
     * @param maxCols	The character width of the component the parse string is going to be displayed in. 
	 * @return 			The parse string with < > characters replaced with &lt; &gt; respectively. 
	 */
	def applyMinHTML(text : String, maxCols : Int = 80) : String = {
		var result = text
		
		// inserting HTML tags alters the location of text in our string afterwards, so we need to keep track of how many characters are inserted when we inject our HTML tags.
		var insertedChars = 0
		
		// obtain the locations of angle brackets so we can convert them to be not interpretted by the EditorPane's HTML kit.
		val (ltList,gtList) = enforceAngleBracketReplacement(text)
		val breaks = enforceLineWrapWithNoTabs(text,maxCols)
		
		while(!breaks.isEmpty || !ltList.isEmpty || !gtList.isEmpty) {
			var tag = ""
			var insertLoc = 0
			var trimmedChars = 0
			
			if(!breaks.isEmpty && (ltList.isEmpty || breaks(0) <= ltList(0)) && (gtList.isEmpty || breaks(0) <= gtList(0))) {
				// insert a <br/> tag to enforce a line break
				tag = """<br/>"""
				insertLoc = breaks(0)
				breaks.trimStart(1)
			} else if(!ltList.isEmpty && (gtList.isEmpty || ltList(0) < gtList(0))) { 
				tag = """&lt;"""
				insertLoc = ltList(0)
				ltList.trimStart(1)
				// trim out the '<'. 
				val (str1,str2) = result.splitAt(insertLoc + insertedChars)
				result = str1 + str2.drop(1)
				trimmedChars = 1
			} else {
				tag = """&gt;"""
				insertLoc = gtList(0)
				gtList.trimStart(1)
				// trim out the '>'. 
				val (str1,str2) = result.splitAt(insertLoc + insertedChars)
				result = str1 + str2.drop(1)
				trimmedChars = 1
			}
				
			val (str1, str2) = result.splitAt(insertLoc + insertedChars)
			result = str1 + tag + str2
			insertedChars += tag.size - trimmedChars
		}
		
		result
	}
	
	
	
	/** 
	 * Applies C-style formatting to a parse string for use in NodeSprites.
	 * @param text			the parse string in which we are applying C-style formatting.
	 * @param maxCols		The character width of the component the parse string is going to be displayed in. 
	 * @return 				The parse string with new line and mimic-tab (3 spaces) characters inserted in where appropriate for formatting, A list of start indices for syntax coloring, A list of colors corresponding to the indices in the previous list, and a list of end indices for syntax coloring.
	 */
	def applyCFormatting(text : String, ddisableHighlight : Boolean = true, maxCols : Int = 50) : (String, ListBuffer[Int], ListBuffer[java.awt.Color], ListBuffer[Int]) = {
		var result = text
        
        var disableHighlight = ddisableHighlight
        if(regexes == null) disableHighlight = true
		
		// inserting characters alters the location of text in our string afterwards, so we need to keep track of how many characters are inserted when we inject our HTML tags.
		var insertedChars = 0

		// obtain the lists of data for the coloring.
		val starts = new ListBuffer[Int]
		val colors = new ListBuffer[String]
		val ends = new ListBuffer[Int]
		
        try {
            if(!disableHighlight) applyRegexes(text, 0, starts, colors, ends)
		} catch {
            case _ =>
                // catch an errors or exception just in case something goes horribly wrong while applying the regexes.
                System.err.println("I just don't know what went wrong! :\n" + text) 
        }
		val resultStarts = new ListBuffer[Int]
		val resultColors = new ListBuffer[java.awt.Color]
		val resultEnds = new ListBuffer[Int]
		
		// obtain the list of data for enforcing line breaks. (because HTMLEditorKit is dumb and doesn't enforce word-wrap)
		val (breaks, tabs) = enforceLineWrap(text,maxCols, false)
		
		// insert the line breaks.
		while(!starts.isEmpty || !ends.isEmpty || !breaks.isEmpty || !tabs.isEmpty) {
			var tag = ""
			var insertLoc = 0
			
			if(!starts.isEmpty && (ends.isEmpty || starts(0) < ends(0)) && (breaks.isEmpty || starts(0) <= breaks(0)) && (tabs.isEmpty || starts(0) <= tabs(0))) {
				// there aren't any color tags. Instead, we adjust the index at which the coloring is applied to account for previous formatting.
				resultStarts += starts(0) + insertedChars
				resultColors += new java.awt.Color(Integer.parseInt(colors(0).drop(1), 16) )
				starts.trimStart(1)
				colors.trimStart(1)
			} else if(!ends.isEmpty && (breaks.isEmpty || ends(0) <= breaks(0)) && (tabs.isEmpty || ends(0) <= tabs(0))) {
				// there aren't any color tags. Instead, we adjust the index at which the coloring is applied to account for previous formatting.
				resultEnds += ends(0) + insertedChars
				ends.trimStart(1)
			} else if(!breaks.isEmpty && (tabs.isEmpty || breaks(0) <= tabs(0))) {
				// insert a '\n' character
				tag = "\n"
				insertLoc = breaks(0)
				breaks.trimStart(1)
			} else {
				// insert a tab
				tag = "   "
				insertLoc = tabs(0)
				tabs.trimStart(1)
			}
            
            
            // insert the new line or tab spaces into our result string
			val (str1, str2) = result.splitAt(insertLoc + insertedChars)
			result = str1 + tag + str2
			insertedChars += tag.size
		}
		
		(result, resultStarts, resultColors, resultEnds)
	}
}



