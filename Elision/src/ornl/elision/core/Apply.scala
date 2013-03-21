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
======================================================================
* */
package ornl.elision.core

import scala.compat.Platform
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack

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
  lazy val isConstant = op.isConstant && arg.isConstant
  lazy val isTerm = op.isTerm && arg.isTerm
  lazy val depth = (op.depth max arg.depth) + 1
  lazy val deBruijnIndex = op.deBruijnIndex max arg.deBruijnIndex
  
  /** The hash code for this apply. */
  override lazy val hashCode = op.hashCode * 31 + arg.hashCode
  lazy val otherHashCode = op.otherHashCode + 8191*arg.otherHashCode
  
  override def equals(other: Any) = (other match {
      case oapp: Apply =>
        feq(oapp, this, (op == oapp.op) && (arg == oapp.arg))
        
      case _ =>
        false
    })

  def rewrite(binds: Bindings) = {
    val (nop, nof) = op.rewrite(binds)
    val (narg, naf) = arg.rewrite(binds)
    if (nof || naf) {
  		val newApply = Apply(nop, narg)
  		(newApply, true) 
  	} else { 
      (this, false)
    }
  }
  
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
    // Temporarily disable rewrite timeouts.
    val oldTimeout = BasicAtom.timeoutTime.value
    if (BasicAtom.rewriteTimedOut) {
      BasicAtom.timeoutTime.value = -1L
    }
    else {
      BasicAtom.timeoutTime.value = Platform.currentTime + 10*1000
    }

    // Do not try to compute if metaterms are present.
    var retval: BasicAtom = null
    if (!op.evenMeta && !arg.isTerm) {
      val result = SimpleApply(op, arg)
      retval = result
    } else {
      op match {
  		  case StringLiteral(typ, str) if arg.isInstanceOf[StringLiteral] =>
  		    // If the argument is also a string literal, then we want to simply
  		    // concatenate them.
  		    val result = StringLiteral(typ, str + arg.asInstanceOf[StringLiteral].value)
          retval = result
          
  	    case app:Applicable =>
  	      try {
  		      // The lhs is applicable; invoke its apply method.  This will
  		      // return some atom, and that atom is the overall result.
  		      val result = app.doApply(arg, bypass)
            retval = result
  	      } catch {
  	        case ex:java.lang.StackOverflowError =>
              // Trapped unbounded recursion.
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
          retval = result
          
  	    case _ =>
  	      // The lhs is something else.  It may be a variable or some other
  	      // expression that we have yet to evaluate.  Just build a simple
  	      // apply of the lhs and rhs.
  	      val result = SimpleApply(op, arg)
          retval = result
	    }
    }

    // Resume timing out rewrites.
    BasicAtom.timeoutTime.value = oldTimeout

    // Return the result.
    retval
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
  lazy val theType = op.operator.typ.rewrite(pabinds)._1
  
  override def rewrite(binds: Bindings) = {
    // If we have no bindings, don't rewrite the operator.
    if (binds == null) {
      (this, false)
    } else {
      // We have bindings. Rewrite the operator.
      // See if we have already rewritten this operator with these
      // bindings.
      (binds.rewrites get this) match {
        // We have already done this rewrite.
        case Some(rewrite) =>
          rewrite
        
        // We don't have a cached rewrite.
        case None =>
          // Rewrite the argument, but not the operator.  In reality, operators
          // should protect their arguments using De Bruijn indices, but that's
          // not implemented just yet.
          val pair = arg.rewrite(binds)
          if (pair._2) {
            val newApply = Apply(op, pair._1)
            binds.rewrites(this) = (newApply, true) 
            (newApply, true) 
          } else {
            binds.rewrites(this) = (this, false) 
            (this, false)
          }
      }
    }  
  }

  /**
   * Get the variables in the operator arguments.
   */
  override def getVariables(): Option[HashSet[BasicAtom]] = {
    // Make the result set to hold the variables.
    var r = new HashSet[BasicAtom]

    // This is used a lot, so it needs to be fast. We will find all
    // the variables here with a stack to avoid recursive calls.
    var work = new Stack[BasicAtom]
    var done = new HashSet[BasicAtom]
    work.push(this)
    while (!work.isEmpty) {
      work.pop match {

        // Are we working on an operator instance?
        case op: OpApply => {
          // Push all the operator arguments on the stack to check, if
          // we have not already checked this operator instance.
          if (!(done contains op)) {
            for (a <- op.arg) work.push(a)
            done += op
          }
        }

        // Did we find a variable?
        case v: Variable => {

          // Save the variable.
          r.add(v)
        }
        
        // Any other type of atom we ignore.
        case _ => {}
      }
    }

    return Some(r)
  }

  /**
   * Get the operators in the operator arguments, plus this operator.
   */
  override def getOperators(opNames: HashSet[String]): Option[HashSet[BasicAtom]] = {

    // Make the result set to hold the variables.
    var r = new HashSet[BasicAtom]

    // This is used a lot, so it needs to be fast. We will find all
    // the operators here with a stack to avoid recursive calls.
    var work = new Stack[BasicAtom]
    var done = new HashSet[BasicAtom]
    work.push(this)
    while (!work.isEmpty) {
      work.pop match {

        // Are we working on an operator instance?
        case currOp: OpApply => {

          // Is this one of the operators we are looking for?
          if (opNames contains currOp.op.operator.name) r.add(currOp)

          // Push all the operator arguments on the stack to check, if
          // we have not already checked this operator instance.
          if (!(done contains currOp)) {
            for (a <- currOp.arg) work.push(a)
            done += currOp
          }
        }
        
        // Any other type of atom we ignore.
        case _ => {}
      }
    }

    return Some(r)
  }
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
}
