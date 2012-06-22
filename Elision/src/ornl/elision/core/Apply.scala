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
package ornl.elision.core

import scala.collection.mutable.ListBuffer
import ornl.elision.repl.ReplActor

/**
 * The common root for all application atoms.  This class represents the
 * "applicative dot."
 * 
 * == Purpose ==
 * An ''apply'' takes two atoms and applies the first (as an operator,
 * rewriter, etc.) to the second (as the argument).
 * 
 * In general this just forms a pair, but certain left-hand sides will
 * undergo specialized processing by the system.
 * 
 * == Use ==
 * Use this class via the companion object, so that the correct result is
 * returned.  The result may be any kind of atom.
 * 
 * @param op		The left-hand element of the apply (operator).
 * @param arg		The right-hand element of the apply (argument).
 */
abstract class Apply(val op: BasicAtom, val arg: BasicAtom) extends BasicAtom {
  val isConstant = op.isConstant && arg.isConstant
  val isTerm = op.isTerm && arg.isTerm
  val constantPool = Some(BasicAtom.buildConstantPool(2, op, arg))
  val depth = (op.depth max arg.depth) + 1
  val deBruijnIndex = op.deBruijnIndex max arg.deBruijnIndex
  
  /** The hash code for this apply. */
  override lazy val hashCode = op.hashCode * 31 + arg.hashCode
  
  override def equals(other: Any) = other match {
    case Apply(oop, oarg) => oop == op && oarg == arg
    case _ => false
  }
  /*
  def rewrite(binds: Bindings) = {
    val (nop, nof) = op.rewrite(binds)
    val (narg, naf) = arg.rewrite(binds)
    if (nof || naf) (Apply(nop, narg), true) else (this, false)
  }
  */
  //////////////////// GUI changes
  
  def rewrite(binds: Bindings) = {
    ReplActor ! ("Eva", "pushTable", "Apply rewrite")
	// top node of this subtree
	ReplActor ! ("Eva", "addToSubroot", ("rwNode", "Apply rewrite: ")) //val rwNode = RWTree.addToCurrent("Apply rewrite: ")

	ReplActor ! ("Eva", "addTo", ("rwNode", "op", "Operator: ")) // RWTree.current = RWTree.addTo(rwNode, "Operator: ", op)
    ReplActor ! ("Eva", "addTo", ("op", "op", op))
    ReplActor ! ("Eva", "setSubroot", "op")
    val (nop, nof) = op.rewrite(binds)
	
	ReplActor ! ("Eva", "addTo", ("rwNode", "arg", "Argument: ")) //RWTree.current = RWTree.addTo(rwNode, "Argument: ", arg)
    ReplActor ! ("Eva", "addTo", ("arg", "arg", arg))
    ReplActor ! ("Eva", "setSubroot", "arg")
    val (narg, naf) = arg.rewrite(binds)
	
    ReplActor ! ("Eva", "setSubroot", "rwNode") // RWTree.current = rwNode
    if (nof || naf) {
		val newApply = Apply(nop, narg)
		ReplActor ! ("Eva", "addTo", ("rwNode", "", newApply)) // RWTree.addTo(rwNode,newApply)
        
        ReplActor ! ("Eva", "popTable", "Apply rewrite")
		(newApply, true) 
	} else { 
        ReplActor ! ("Eva", "popTable", "Apply rewrite")
        (this, false)
    }
  }
  
  //////////////////// end GUI changes
  
  /**
   * By default applications match iff their parts match.  The trick here is
   * that the argument lists have to know what the top-level operator is in
   * order to successfully associatively match.
   * 
   * Hints passed in are ignored, and the operator is passed along as the hint.
   */
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) =
    // Only applies match other applies.
    subject match {
      case Apply(oop, oarg) => {
        // Try to match the operators, and then the arguments.  If both match,
        // then this matches.  If not, then this does not match.
        op.tryMatch(oop, binds, Some(op)) match {
          case fail: Fail =>
            Fail("Operators do not match.", this, subject, Some(fail))
          case Match(newbinds) =>
            // The operators match.  Now try to match the arguments.
            arg match {
              case as:AtomSeq => as.tryMatch(oarg, newbinds, Some(op))
              case _ => arg.tryMatch(oarg, newbinds, Some(op))
            }
            
          case Many(matches) =>
            // The operators match in multiple ways.  This seems unlikely, but
            // we consider it here anyway.
            Many(MatchIterator(arg.tryMatch(oarg, _, Some(op)), matches))
        }
      }
      case _ => Fail("Applications only match other applications.",
          this, subject)
    }
}

/**
 * Provide construction and extraction for an `Apply`.  This is the correct
 * place to come to make an application object.
 */
object Apply {
  /**
   * Extract the components of an apply and return them.
   * 
   * @param apply	The apply.
   * @return	A pair consisting of the operator and argument, in order.
   */
  def unapply(apply: Apply) = Some(apply.op, apply.arg)
  
  /**
   * Construct an application.  We key off the left hand side (the operator)
   * to decide how to handle this.  Specifically, automatic handling is
   * performed for [[ornl.elision.core.Applicable]] and
   * [[ornl.elision.core.Rewriter]].
   * 
   * If the right-hand side is ''not'' a term, then none of the special
   * handling described above is performed, and the applicative pair is
   * returned as-is.
   * 
   * Operators may have native handlers, and these may subsequently return
   * here.  To prevent an infinite loop, the native handler can specify
   * the `bypass` flag when it calls here to prevent the native handler
   * from being re-invoked.
   * 
   * @param op			The lhs of the apply, typically an operator.
   * @param arg			The rhs of the apply, typically an argument.
   * @param bypass	If true, bypass native operator handler invocations.
   * @return	The basic atom resulting from the application.
   */
  def apply(op: BasicAtom, arg: BasicAtom,
      bypass: Boolean = false): BasicAtom = {
      
    //////////////////// GUI changes
    ReplActor ! ("Eva","pushTable", "object Apply apply")
    // top node of this subtree
    ReplActor ! ("Eva", "addToSubroot", ("rwNode", "object Apply apply: ")) // val rwNode = RWTree.addToCurrent("object Apply apply: ") 
    ReplActor ! ("Eva", "addTo", ("rwNode", "op", "Operator: ", op)) // val opNode = RWTree.addTo(rwNode, "Operator: ", op) 
    ReplActor ! ("Eva", "addTo", ("rwNode", "arg", "Argument: ", arg)) // val argNode = RWTree.addTo(rwNode, "Argument: ", arg) 
    ReplActor ! ("Eva", "setSubroot", "rwNode") // RWTree.current = rwNode
    
    //////////////////// end GUI changes
        
    // Do not try to compute if metaterms are present.
    if (!op.evenMeta && !arg.isTerm) {
        val result = SimpleApply(op, arg)
        ReplActor ! ("Eva", "popTable", "object Apply apply")
        result
    } else {
      op match {
  		  case StringLiteral(typ, str) if arg.isInstanceOf[StringLiteral] =>
  		    // If the argument is also a string literal, then we want to simply
  		    // concatenate them.
  		    val result = StringLiteral(typ, str + arg.asInstanceOf[StringLiteral].value)
            ReplActor ! ("Eva", "popTable", "object Apply apply")
            result
  	    case app:Applicable =>
  	      try {
  		      // The lhs is applicable; invoke its apply method.  This will
  		      // return some atom, and that atom is the overall result.
  		      val result = app.doApply(arg, bypass)
              ReplActor ! ("Eva", "popTable", "object Apply apply")
              result
  	      } catch {
  	        case ex:java.lang.StackOverflowError =>
              // Trapped unbounded recursion.
                ReplActor ! ("Eva", "popTable", "object Apply apply")
  		        throw new LambdaUnboundedRecursionException(
  		            "Application results in unbounded recursion: (" +
  		            op.toParseString + ").(" + arg.toParseString + ")")
  	      }
  	    case rew:Rewriter =>
  	      // The lhs is a rewriter; invoke its rewrite method.  This will return
  	      // a pair.  We need to convert the pair to a binding.
  	      val (r_atom, r_flag) = rew.doRewrite(arg)
  	      val result = BindingsAtom(Bindings() +
  	          ("atom" -> r_atom) +
  	          ("flag" -> (if (r_flag) Literal.TRUE else Literal.FALSE)))
          ReplActor ! ("Eva", "popTable", "object Apply apply")
          result
  	    case _ =>
  	      // The lhs is something else.  It may be a variable or some other
  	      // expression that we have yet to evaluate.  Just build a simple
  	      // apply of the lhs and rhs.
  	      val result = SimpleApply(op, arg)
          ReplActor ! ("Eva", "popTable", "object Apply apply")
          result
	    }
    }
  }
}

/**
 * An ''operator apply''.  This is the common case of applying a known operator
 * to some argument list.
 * 
 * This has some special syntax (operator name juxtaposed with argument list
 * in parentheses) and provides special handling for the type (the type is
 * rewritten using the bindings resulting from matching the arguments against
 * the parameters).
 * 
 * Based on properties and any native handler, this may never be constructed
 * for an operator application.  '''Do not use this directly.'''  Instead,
 * use the methods in the [[ornl.elision.core.Apply]] companion object.
 * 
 * @param op			The operator.
 * @param arg			The argument list.
 * @param pabinds	The bindings from parameter name to argument.  Note that
 * 								if the operator is associative the parameters may be
 * 								synthetic!
 */
case class OpApply protected[core] (override val op: OperatorRef,
    override val arg: AtomSeq, val pabinds: Bindings) extends Apply(op, arg) {
  /**
   * Compute the type from the type specified by the operator, and the bindings
   * provided during parameter matching.  This allows rewriting otherwise
   * abstract type information to get a proper type.
   */
  val theType = op.operator.typ.rewrite(pabinds)._1
  
  def toParseString = toESymbol(op.name) + "(" + arg.toNakedString + ")"
  
  //////////////////// GUI changes
  override def rewrite(binds: Bindings) = {
	ReplActor ! ("Eva", "pushTable", "OpApply rewrite")
    // top node of this subtree
	ReplActor ! ("Eva", "addToSubroot", ("rwNode", "OpApply rewrite: ")) //val rwNode = RWTree.current
	
    // Rewrite the argument, but not the operator.  In reality, operators
    // should protect their arguments using De Bruijn indices, but that's
    // not implemented just yet.
    val pair = arg.rewrite(binds)
    if (pair._2) {
		ReplActor ! ("Eva", "setSubroot", "rwNode") // RWTree.current = rwNode
		val newApply = Apply(op, pair._1)
		ReplActor ! ("Eva", "addTo", ("rwNode", "", newApply)) //RWTree.addTo(rwNode, newApply)
		
        ReplActor ! ("Eva", "popTable", "OpApply rewrite")
        (newApply, true) 
	} else {
        ReplActor ! ("Eva", "popTable", "OpApply rewrite")
        (this, false)
    }
  }
  //////////////////// end GUI changes
}

/**
 * A ''simple apply''.  This is the class used if an apply "survives"
 * processing, such as when the right-hand side is not a term.
 * 
 * '''Do not use this directly.''' Instead, use the methods in the
 * [[ornl.elision.core.Apply]] companion object to create an apply using
 * the correct processing.
 * 
 * @param op		The operator.
 * @param arg		The argument.
 */
case class SimpleApply protected[core] (override val op: BasicAtom,
    override val arg: BasicAtom) extends Apply(op, arg) {
  /**
   * We take the type from the operator.  This may be an incomplete type, but
   * we cannot rewrite it yet because we don't know the full bindings.  This
   * might cause trouble with matching.
   */
  val theType = op.theType
  
  // When an integer literal is present, we have to surround it with parens
  // so that the system does not interpret the applicative dot as a decimal
  // point.  If a named root type is present, it can also cause trouble, so
  // we must explicitly annotate it.
  def toParseString = "(" +
  	(if (op.isInstanceOf[IntegerLiteral])
  	  "(" + op.toParseString + ")"
	  else if (op.isInstanceOf[NamedRootType])
	    op.toParseString + ":^TYPE"
    else op.toParseString) +
    "." + arg.toParseString + ")"
}
