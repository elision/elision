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
package ElisionGUI

import util.matching._

object EliRegexes {
	val multilineComment = new Regex("""(/\*(\n|.)+?\*/)""",
		"all")
	val singlelineComment = new Regex("""(//.*\n)""",
		"all")
	val verbatim = new Regex("""((\"\"\"(\n|.)*?\"\"\"))""",				
		"all")
	val stringLit = new Regex("""(\"(\n|.)*?\")""",							
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
	val keywords = new Regex("""(operator|is|case|->|@|=)""",
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
	val numBin = new Regex("""(-?0(b|B)[0-1]*([0-1]+[.])?[0-1]+(e|p)?)""",
		"all")
	val numDec = new Regex("""(-?[0-9]*([0-9]+[.])?[0-9]+(e|p)?)""",
		"all")
}





