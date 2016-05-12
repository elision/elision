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

import scala.collection.mutable.ListBuffer
import scala.util.matching._

import ornl.elision.gui._
import ornl.elision.syntax


/** Provides regexes and some other useful data for performing Elision syntax highlighting in Eva. */
object EliRegexes extends syntax.SyntaxRegexes {
  val multilineComment = new Regex("""(/\*(\n|.)+?\*/)""",
    "all")
  val singlelineComment = new Regex("""(//.*(\n|))""",
    "all")
  val verbatim = new Regex("""((\"\"\"(\n|.)*?\"\"\"))""",        
    "all")
  val stringLit = new Regex("""(\"(\n|\\\"|.)*?\")""",              
    "all")
  
  val lambdaBackTick = new Regex("""((\\\$\$?)(`(\n|\\`|.)*?`))""",    
    "all")
  val lambdaNormal = new Regex("""((\\\$\$?)([_a-zA-Z][_a-zA-Z0-9]*))""",  
    "all")
  
  val varBackTick = new Regex("""((\$\$?)(`(\n|\\`|.)*?`))""",      
    "all")
  val varNormal = new Regex("""((\$\$?)([_a-zA-Z][_a-zA-Z0-9]*))""",    
    "all")
  
  val termSFBackTick = new Regex("""((##?)(`(\n|\\`|.)*?`))""",      
    "all")
  val termSFNormal = new Regex("""((##?)([_a-zA-Z][_a-zA-Z0-9]*))""",    
    "all")
  
  val typeRoot = new Regex("""(ANY|BINDING|BOOLEAN|FLOAT|INTEGER|OPTYPE|STRATEGY|STRING|SYMBOL|NONE|\\^TYPE)""",
    "all")
    
  val boolConstants = new Regex("""(true|false|Nothing)""",
    "all")
  val keywords = new Regex("""(operator|is|cases|case|->|@|=)""",
    "all")
  val algProps = new Regex("""(associative|commutative|idempotent|identity|absorber|not)""",
    "all")
  
  val specialForm = new Regex("""((\{)(:)((\n|.)*?)(:)(\}))""",
    "all","startCurly","startColon","body","endColon","endCurly")
  val opDefShortcut = new Regex("""((\{)(!)((\n|.)*?)(\}))""",
    "all","startCurly","startBang","body","endCurly")
  val sequence = new Regex("""((%)(([!]?[ACIBD](\[.+?\\])?)*))""",
    "all","startMod","algProps")
  
  val brackets = new Regex("""((\{|\(|\[|\}|\)|\]))""",
    "all")
  
  val typeBackTick = new Regex("""(: *`(\n|\\`|.)*?`)""",
    "all")
  val typeNormal = new Regex("""(: *[_a-zA-Z][_a-zA-Z0-9]*)""",
    "all")
  
  val symbolBackTicks = new Regex("""(`(\n|\\`|.)*?`)""",          
    "all")
  val symbolNormal = new Regex("""([_a-zA-Z][_a-zA-Z0-9]*)""",      
    "all")
  
  val numHex = new Regex("""(-?0(x|X)[0-9a-fA-F]*([0-9a-fA-F]+[.])?[0-9a-fA-F]+p?)""",
    "all")
  val numOct = new Regex("""(-?0(o|O)[0-7]*([0-7]+[.])?[0-7]+(e|p)?)""",
    "all")
  val numBin = new Regex("""(-?0(b|B)[0-1]*([0-1]+[.])?[0-1]+(e|p)?)""",
    "all")
  val numDec = new Regex("""(-?[0-9]*([0-9]+[.])?[0-9]+(e|p)?)""",
    "all")
  
  /** A list of the regexes in the correct precedence that they should be applied. */
  val rList = List(  multilineComment,
    singlelineComment,
    verbatim,
    stringLit,
    lambdaBackTick,
    lambdaNormal,
    varBackTick,
    varNormal,
    termSFBackTick,
    termSFNormal,
    typeRoot,
    boolConstants,
    keywords,
    algProps,
    specialForm,
    opDefShortcut,
    sequence,
    brackets,
    typeBackTick,
    typeNormal,
    symbolBackTicks,
    symbolNormal,
    numHex,
    numOct,
    numBin,
    numDec
  )
  
  /** A map between regexes and their appropriate web colors used for HTML formatting. */
  val colorMap = Map[Regex,String](
    multilineComment -> EliWebColors.comments,
    singlelineComment -> EliWebColors.comments,
    verbatim -> EliWebColors.stringLits,
    stringLit -> EliWebColors.stringLits,
    lambdaBackTick -> EliWebColors.keywords,
    lambdaNormal -> EliWebColors.keywords,
    varBackTick -> EliWebColors.vars,
    varNormal -> EliWebColors.vars,
    termSFBackTick -> EliWebColors.vars,
    termSFNormal -> EliWebColors.vars,
    typeRoot -> EliWebColors.types,
    boolConstants -> EliWebColors.constants,
    keywords -> EliWebColors.keywords,
    algProps -> EliWebColors.properties,
    specialForm -> EliWebColors.keywords,
    opDefShortcut -> EliWebColors.keywords,
    sequence -> EliWebColors.properties,
    brackets -> EliWebColors.stringLits,
    typeBackTick -> EliWebColors.types,
    typeNormal -> EliWebColors.types,
    symbolBackTicks -> EliWebColors.vars,
    symbolNormal -> EliWebColors.vars,
    numHex -> EliWebColors.constants,
    numOct -> EliWebColors.constants,
    numBin -> EliWebColors.constants,
    numDec -> EliWebColors.constants
  )
  
  /** A map indicating what regexes need recursive parsing. -1 indicates that the regex does not need recursive parsing. Any other integer indicates the regex group in which recursive parsing will be performed. */
  val recursableMap = Map[Regex, Int](
    multilineComment -> -1,
    singlelineComment -> -1,
    verbatim -> -1,
    stringLit -> -1,
    lambdaBackTick -> -1,
    lambdaNormal -> -1,
    varBackTick -> -1,
    varNormal -> -1,
    termSFBackTick -> -1,
    termSFNormal -> -1,
    typeRoot -> -1,
    boolConstants -> -1,
    keywords -> -1,
    algProps -> -1,
    specialForm -> 4,
    opDefShortcut -> 4,
    sequence -> -1,
    brackets -> -1,
    typeBackTick -> -1,
    typeNormal -> -1,
    symbolBackTicks -> -1,
    symbolNormal -> -1,
    numHex -> -1,
    numOct -> -1,
    numBin -> -1,
    numDec -> -1
  )
}

/** 
 * Web color constants for Elision syntax highlighting in Eva. 
 * These are Strings of the form "#[some hex value representing a color]".
 */
object EliWebColors {
  /** Black, just black. */
  val default = "#000000"
  
  /** Everfree green */
  val comments = "#007700"
  
  /** Derp gray */
  val stringLits = "#888888"
  
  /** Pie magenta */
  val vars = "#dd00dd"
  
  /** Apple orange */
  val keywords = "#ee9944"
  
  /** Blue, just blue. */
  val constants = "#0000ff"
  
  /** Dash cyan */
  val types = "#77a9dd"
  
  /** Twilight lavender */
  val properties = "#9977cc" 
}


