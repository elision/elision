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

/**
 * @author l5o
 *
 */
object espresso {
  def `native$espresso`():Option[ApplyData => BasicAtom] = {

def _handler(_data: ApplyData): BasicAtom = {
import _data.{ exec => _, _ }
import ApplyData._
import console._

// FIXME: BEGIN NATIVE HANDLER
import scala.collection.mutable.{ HashMap => Map }
import scala.collection.mutable.MultiMap
import scala.collection.mutable.Queue
import scala.collection.mutable.{ Set => MSet }
import scala.collection.mutable.{ MutableList => MList }

val fxDir = System.getenv("FX_DIR")
val espressoCmd = if(fxDir != null) fxDir+"/bin/espresso" else "espresso"
val t0 = System.nanoTime

class BoolIterator(len:Int) extends Iterator[List[Boolean]] {
  import scala.collection.mutable.{ MutableList => List }
  
  var state:List[Boolean] = List((for(i <- 1 to len) yield false): _*)
  var i: Int = 0
  
  
  def next() = {
    i = i+1
    val oldState = List(state: _*)
    
    state.zipWithIndex.foldRight(true){ case ((e,i),v) =>
      state(i) = v^e
      if(v&e) true else false
    }
    
    oldState.toList
  }
  
  def hasNext(): Boolean = i < (1 << len)
}

abstract class Node(val opName: String, val atom: BasicAtom, val children: List[Node] = Nil) {
  var bad = false
  var known = false
  
  def eval(): BasicAtom

  override def toString(): String = "Node("+ atom.toParseString +")"
}

class AndNode(atom: BasicAtom, children: List[Node] = Nil) extends Node("and", atom, children) {
  def eval() = children.foldLeft(true)(_&_.eval().asInstanceOf[Boolean])
}
class NotNode(atom: BasicAtom, children: List[Node] = Nil) extends Node("not", atom, children) {
  def eval() = !children(0).eval.asInstanceOf[Boolean]
}
class OrNode(atom: BasicAtom, children: List[Node] = Nil) extends Node("or", atom, children) {
  def eval() = children.foldLeft(false)(_|_.eval().asInstanceOf[Boolean])
}
class XorNode(atom: BasicAtom, children: List[Node] = Nil) extends Node("xor", atom, children) {
  def eval() = children.foldLeft(false)(_^_.eval().asInstanceOf[Boolean])
}
class OpNode(opName: String, atom: BasicAtom, val context: Context, children: List[Node] = Nil) extends Node(opName, atom, children) {
  def eval() = context.operatorLibrary(opName)(children.map(_.atom).toSeq:_*)
}
class VarNode(atom: BasicAtom) extends Node("_var", atom) {
  def eval() = atom
}
class LitNode(atom: BasicAtom) extends Node("_lit", atom) {
  def eval() = atom
}


// Globals
val debug = false
val opNames = List("and", "or", "xor", "not")
val queue = Queue[BasicAtom]()
// goodRets contains variables which we know the return type of and OPREFs
// for operators that have KNOWN return types, not necessarily boolean return
// type
val goodRets = MSet[BasicAtom]()

// holds vars which need iteration
var iterVars = scala.collection.mutable.MutableList[BasicAtom]()

// the table is composed of the function name -> boolean for whether this is 'good or not', and the argument list
val table = new Map[String, MSet[(Node,MList[Node])]] with MultiMap[String, (Node,MList[Node])]

def makeTree(a: BasicAtom): Node = {
  a match {
    case Apply(op0: OperatorRef, args0: AtomSeq) => {
      if(debug) println("Operator: "+ op0.name)
      // if we have an and, or, xor, not then add whatever the operands are to the goodRets list
      // if an operand is an operator application, we just add the OPREF. Otherwise, equality tests
      // would only succeed if every operator application had the same parameters
      if(opNames.contains(op0.name)) {
        args0.filter(!goodRets.contains(_)) foreach(_ match {
          case Apply(op1: OperatorRef, args1: AtomSeq) => goodRets += op1
          case x => goodRets += x
        })
        op0.name match {
          //case "and" => new AndNode(a, args0.map(e => makeTree(e)).toList)
          //case "or" => new OrNode(a, args0.map(e => makeTree(e)).toList)
          //case "xor" => new XorNode(a, args0.map(e => makeTree(e)).toList)
          //case "not" => new NotNode(a, args0.map(e => makeTree(e)).toList)
          case "and" => new OpNode(op0.name, a, context, args0.map(e => makeTree(e)).toList)
          case "or" =>  new OpNode(op0.name, a, context, args0.map(e => makeTree(e)).toList)
          case "xor" => new OpNode(op0.name, a, context, args0.map(e => makeTree(e)).toList)
          case "not" => new OpNode(op0.name, a, context, args0.map(e => makeTree(e)).toList)
        }
      } else {
        // if we're here then we need to add the prototype for this function to the table
        val thisNode = new OpNode(op0.name, a, context, args0.map(e => makeTree(e)).toList)

        table.addBinding(op0.name, (thisNode -> MList(thisNode.children:_*)))

        thisNode
      }
    }
    case v: Variable => if(debug) println("Variable: "+ v); new VarNode(v)
    case l: Literal[_] => if(debug) println("Literal: "+ l); new LitNode(l)
    case _ => println("ERROR Something else"); new LitNode("ERROR")
  }
}
 
def postProcess(root: Node): Unit = {
  // OK here is the algorithm used for figuring out what needs to be
  // enumerated

  // first, mark all nodes which are in goodRets as known
  // look at every parameter in every parameterlist. If a parameter is
  // an and,or,not,xor then we know return value. Mark it.
  table.valuesIterator.foreach(  operator =>
              operator.foreach{  case (opNode,paramList) => 
              paramList.foreach{ paramNode =>
    paramNode.atom match {
      case Apply(op: OperatorRef, args: AtomSeq) => {
        if(opNames.contains(op.name)) paramNode.known = true
        else if(goodRets.contains(op)) paramNode.known = true
      }
      case v: Variable => if(goodRets.contains(v)) paramNode.known = true
      case l: Literal[_] => paramNode.known = true
      case _ => println("ERROR SOMETHING ELSE 1")
    }
  }})

  // second, we look in every paramList and for every param that 
  // is marked as known, in every other paramList we mark that position
  // as known. So, if we have foo(G,x,z) and foo(w,y,t) where G represents
  // a node marked as known, we know z=G, z is Boolean type
  val knownMap = new Map[String, MSet[Int]] with MultiMap[String, Int]


  var changed = false
  do {
    changed = false

    table.valuesIterator.foreach(  operator =>
                operator.foreach{  case (opNode,paramList) => 
                paramList.indices.foreach{ i =>
      if(paramList(i).known) {
        knownMap.addBinding(opNode.opName, i)
      }
    }})

    if(debug) knownMap foreach println
    if(debug) knownMap.keys foreach println

    table.valuesIterator.foreach(  operator =>
                operator.foreach{  case (opNode,paramList) => 
                paramList.indices.foreach{ i =>
      if(knownMap.contains(opNode.opName) && 
         !knownMap.get(opNode.opName).isEmpty &&
         knownMap.get(opNode.opName).get.contains(i) &&
         !paramList(i).known) {

          if(debug) println("CHANGING1: "+ paramList(i).atom.toParseString)
          paramList(i).known = true
          paramList(i).atom match {
            case Apply(op1: OperatorRef, args1: AtomSeq) => goodRets += op1
            case x => goodRets += x
          }

          changed = true
      }
    }})

    // repeat of the first step, we may have picked up new things
    // for goodRets
    table.valuesIterator.foreach(  operator =>
                operator.foreach{  case (opNode,paramList) => 
                paramList.foreach{ paramNode =>
      paramNode.atom match {
        case Apply(op: OperatorRef, args: AtomSeq) => {
          if(opNames.contains(op.name) && paramNode.known == false) {
            paramNode.known = true
            if(debug) println("CHANGING2: "+ paramNode.atom.toParseString)
            changed = true
          } else if(goodRets.contains(op) && paramNode.known == false) {
            paramNode.known = true
            if(debug) println("CHANGING3: "+ paramNode.atom.toParseString)
            changed = true
          }
        }
        case v: Variable => if(goodRets.contains(v) && paramNode.known == false) {
          paramNode.known = true
          if(debug) println("CHANGING4: "+ paramNode.atom.toParseString)
          changed = true
        }
        case l: Literal[_] => paramNode.known = true
        case _ => if(debug) println("SOMETHING ELSE 1"); ()
      }
    }})
    
  } while(changed)


  // that was a doozy. Now, we bubble up badness so when we look at leaf
  // nodes we get things that need iteration
  bubbleBad(root)

  // getvars
  if(debug) println("-----")
  getVars(root)
  if(debug) println("-----")
}

def bubbleBad(root: Node): Unit = {
  // if root is a leaf and unknown, mark for
  // enum, otherwise leave as known
  if(root.children.isEmpty) {
    val a: BasicAtom = root.atom match {
      case Apply(op1: OperatorRef, args1: AtomSeq) => op1
      case x => x
    }

    if(!goodRets.contains(a)) {
      root.bad = true
    }
  } else {
    root.children.foreach(e => bubbleBad(e))

    // if not a leaf node, then we're bad if
    // one of our children is bad and unknown
    root.children.foreach(e => {
      val a: BasicAtom = e.atom match {
        case Apply(op1: OperatorRef, args1: AtomSeq) => op1
        case x => x
      }
      if(e.bad && !goodRets.contains(a)) root.bad = true
      //if(e.bad && !root.known && !goodRets.contains(e)) root.bad = true
    }) 
  }
}

def getVars(root: Node): Unit = {
  if(root.children.isEmpty || root.bad) {
    if(debug) println(root.atom.toParseString)
    iterVars += root.atom
  } else {
    root.children.foreach(e => getVars(e)) 
  }
}

// TODO: Area of interest for parallelization
def enum(root: Node): List[Int] = {
  println("enum begin: "+ iterVars.distinct.length)
  val boolIter = new BoolIterator(iterVars.distinct.length)
  var l = new MList[Int]()

  if(debug) println("!!!")
  // val l = boolIter.next
  // val a = eval(root, l)  
  // println(a.toParseString)
  boolIter.zipWithIndex.foreach { case (v,i) =>
    // TODO: For each combination, DO EVAL
    val a = eval(root, v)  
    if(debug) println(v.foldLeft("")((v,e) => if(e==true) v ++ "1" else v ++ "0") +": "+ a.toParseString)
    a match {
      case BooleanLiteral(_, v0) => if(v0) l += i
      case _ => return List(-1)
    }
  }  

  println("enum done.")
  l.toList
}

def eval(root: Node, list: List[Boolean]): BasicAtom = {
  // if there is should be a binding for this node return it
  if(root.children.isEmpty || root.bad) {
    if(debug) println("Getting binding... "+ root.atom)
    if(debug) println(iterVars.distinct.indexOf(root.atom))
    list(iterVars.distinct.indexOf(root.atom))
  } else {
    context.operatorLibrary(root.opName)(root.children.map(e => eval(e, list)):_*)
  }
}
def espresso(numInputs: Int, list: List[Int]): String = {
  import sys.process._

  val sb = new StringBuilder()
  sb.append(".i "+ numInputs +"\n")
  sb.append(".o 1\n")
  list.foreach(e => { var s = e.toBinaryString; while (s.length < numInputs) { s = "0" ++ s }; sb.append(s + "\t1\n") })
  
  // Write espresso input to unique tmp file.
  println("** Doing espresso...") 
  val cmd = Seq(espressoCmd, "-Dexact")
  var ret = ""

  val pio = new ProcessIO(
    stdin => { 
      stdin.write(sb.toString.getBytes("UTF-8"))
      stdin.close
    },
    stdout => 
      ret = scala.io.Source.fromInputStream(stdout).getLines.mkString("\n"),
    _ => ()
  )

  val p = cmd.run(pio)
  p.exitValue

  val t1 = System.nanoTime
  println("** espresso() time = " + (t1.toDouble-t0.toDouble)/1000000000)

  ret
}

def makeNot(a: BasicAtom): BasicAtom = {
  context.operatorLibrary("not")(a) 
}
def makeAnd(l: BasicAtom*): BasicAtom = {
  context.operatorLibrary("and")(l: _*)
}
def makeOr(l: BasicAtom*): BasicAtom = {
  context.operatorLibrary("or")(l: _*)
}

def parseEspresso(retString: String, iterVars: List[BasicAtom]): BasicAtom = {
  import scala.collection.immutable.StringOps

  if(debug) println(retString) 

  val so = new StringOps(retString)
  var piList = List[BasicAtom]()

  val Regex = "([01-]+) ([01])".r
  so.lines.foreach { line =>
    line match {
      case Regex(vars, _) => {
        // vList contains variables which need to be kept around to be 
        // anded into this prime implicant
        var vList = List[BasicAtom]()

        vars.zipWithIndex.foreach { case (c,i) => 
          if(debug) println(i +" "+c)
          c match {
              case '0' => vList = makeNot(iterVars(i)) :: vList 
              case '1' => vList = iterVars(i) :: vList 
              case '-' => ()
              case _   => println("ERROR asdfb")
          }
        }

        // ok, we now have all of the variables involved in this PI
        // if we only have 1 var then we don't need to and anything
        piList = makeAnd(vList: _*) :: piList
      }
      case _ => ()
    }
  }

  makeOr(piList: _*)
}

args match {
  case Args(Apply(op0: OperatorRef, args0: AtomSeq)) => 
    if(opNames.contains(op0.name)) {
      val root = makeTree(args(0))
      postProcess(root)

      if(debug) {
        println("")
        goodRets foreach println
        println("")
        iterVars.distinct foreach println
      }

      val vars = iterVars.distinct.toList
// FIXME: This is not commented out in the eli file
//  if(vars.length > 9) {
//    val t1 = System.nanoTime
//    println("** espresso() time fail = " + (t1.toDouble-t0.toDouble)/1000000000)
//    return args(0)
//  }

      val l = enum(root)

      // if l == Nil then there are no cases where this
      // evaluates to true, so return false
      if(l == Nil) false
      else if(l.contains(-1)) {
        if(debug) println("FAIL")
        println("FAIL: espresso failure in identifying what to enum")
    println(iterVars.distinct.length +" vars identified:")
        iterVars.distinct foreach(e => println(e.toParseString))
    println("FAIL: end here")

        args(0)
      }
      else {
        val ret = espresso(vars.length, l)
        parseEspresso(ret, vars.toList)
      }
    } else {
      args(0)
    }
    case _ => args(0)
    //case Args(BooleanLiteral(a1,a2)) => BooleanLiteral(a1,a2)
    //case _ => as_is
}

// FIXME: END NATIVE HANDLER
}
  Some(_handler _)
  }
}

object EspressoTest extends App {
  val exec = new ERepl
  val context = new Context
  
  exec.bootstrap()
  // add espresso op
  exec.context.operatorLibrary.add(TypedSymbolicOperator("espresso",ANY,AtomSeq(NoProps,Variable(ANY,"x",BooleanLiteral(BOOLEAN,true),Set())),"No description.","No detail.\n \nDeclared in: espresso.eli",false,"v",espresso.`native$espresso`).asInstanceOf[Operator])
  exec.context.operatorLibrary.add(TypedSymbolicOperator("and",BOOLEAN,AtomSeq(AlgProp(Some(BooleanLiteral(BOOLEAN,true)),Some(BooleanLiteral(BOOLEAN,true)),Some(BooleanLiteral(BOOLEAN,true)),Some(BooleanLiteral(BOOLEAN,false)),Some(BooleanLiteral(BOOLEAN,true))),Variable(BOOLEAN,"P",BooleanLiteral(BOOLEAN,true),Set()),Variable(BOOLEAN,"Q",BooleanLiteral(BOOLEAN,true),Set())),"Boolean conjunction.","True if all arguments are true, and false if any is false.\n \nDeclared in: logical.eli",false,"",None).asInstanceOf[Operator])
  exec.context.operatorLibrary.add(CaseOperator("not",BOOLEAN,AtomSeq(NoProps,MapPair(AtomSeq(NoProps,BooleanLiteral(BOOLEAN,true)),BooleanLiteral(BOOLEAN,false)),MapPair(AtomSeq(NoProps,BooleanLiteral(BOOLEAN,false)),BooleanLiteral(BOOLEAN,true)),MapPair(BooleanLiteral(BOOLEAN,true),BooleanLiteral(BOOLEAN,false)),MapPair(BooleanLiteral(BOOLEAN,false),BooleanLiteral(BOOLEAN,true)),MapPair(AtomSeq(NoProps,Variable(BOOLEAN,"x",BooleanLiteral(BOOLEAN,true),Set())),ANY),MapPair(Variable(BOOLEAN,"x",BooleanLiteral(BOOLEAN,true),Set()),ANY)),"Boolean negation.","Negate a provided Boolean value.\n \nDeclared in: logical.eli").asInstanceOf[Operator])
  exec.context.operatorLibrary.add(TypedSymbolicOperator("or",BOOLEAN,AtomSeq(AlgProp(Some(BooleanLiteral(BOOLEAN,true)),Some(BooleanLiteral(BOOLEAN,true)),Some(BooleanLiteral(BOOLEAN,true)),Some(BooleanLiteral(BOOLEAN,true)),Some(BooleanLiteral(BOOLEAN,false))),Variable(BOOLEAN,"P",BooleanLiteral(BOOLEAN,true),Set()),Variable(BOOLEAN,"Q",BooleanLiteral(BOOLEAN,true),Set())),"Boolean disjunction.","True if any argument is true, and false if all are false.\n \nDeclared in: logical.eli",false,"", None).asInstanceOf[Operator])

  import exec.ParseSuccess
  try {
    val r = exec.parse("espresso(and($a,$b,$c,$d,$e,$f,$g,$h,$i,$j,$k,$l,$m,$n))")
    r match {
      case ParseSuccess(l) => println(l(0).toParseString)
      case _ => println("FAIL")
    }
  } catch {
    case e => println(e)
  }
  System.exit(0)
}

