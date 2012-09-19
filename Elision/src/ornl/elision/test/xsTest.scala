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
package ornl.elision.test

import ornl.elision.core._
import ornl.elision.repl._
import com.thoughtworks.xstream.XStream

/**
 * @author l5o
 *
 */
object xsTest extends App {
  val k = scala.collection.mutable.HashMap[Int,Int](1->2,2->3)
  println(k)
  val xstream = new XStream
  
  println("working")
  val xml = xstream.toXML(k)
  println("got xml, dumping to file")
  import sys.process._
  import java.io.ByteArrayInputStream

  val is = new ByteArrayInputStream(xml.toString.getBytes("UTF-8"))
  new java.io.File("dmp.bin") #< is !
  
  println("Reading from dump...")
  val lines = scala.io.Source.fromFile("dmp.bin").mkString
  println("Building object")
  val in = xstream.fromXML(lines).asInstanceOf[scala.collection.mutable.HashMap[Int,Int]]
  println(in)
  
  println("done.")
  System.exit(0)
  
  
//  
//  val repl = new ERepl
//  repl.banner()
//  println(repl.context.ruleLibrary.toParseString)
////  println("="*70)
//  repl.bootstrap()
//  
//val text =
<execute>
<![CDATA[
decl.({! perry($x:BOOLEAN,$y:BOOLEAN):BOOLEAN is %AC })
decl.({! ferb($x:BOOLEAN):BOOLEAN })
 
decl.{rule
  perry($x, ferb($x)) -> false
 
  #rulesets DEFAULT
}
 
decl.{rule
  perry($x, ferb($x), $others) -> false
 
  #rulesets DEFAULT
}
 
// Rewrites properly to false.
perry($a, ferb($a))
 
// Rewrites properly to false.
perry($a, ferb($a), $b)
 
// Does not rewrite.
perry($a, ferb($a), $b, $c)
]]>
</execute>
//  
//  repl.execute(text.text)
//  
//
//  println(repl.context.ruleLibrary.toParseString)
////  println(repl.context.operatorLibrary.toParseString)
////  println("="*70)
//
//  val xstream = new XStream
//  
//  println("working")
//  val xml = xstream.toXML(repl.context.ruleLibrary)
//  println("got xml, dumping to file")
//  import sys.process._
//  import java.io.ByteArrayInputStream
//
//  val is = new ByteArrayInputStream(xml.toString.getBytes("UTF-8"))
//  new java.io.File("dmp.bin") #< is !
//  
//  println("Reading from dump...")
//  val lines = scala.io.Source.fromFile("dmp.bin").mkString
//  println("Building object")
//  val newRepl = xstream.fromXML(lines).asInstanceOf[RuleLibrary]
//  
//  println(newRepl.toParseString)
////  println(newRepl.context.operatorLibrary.toParseString)
////  println("="*70)
////  newRepl.bootstrap()
////  println(newRepl.context.operatorLibrary.toParseString)
////  println("="*70)
//  
//  println("done.")
//  System.exit(0)
//  
//  
  
}