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
 * ======================================================================
 * */
package ornl.elision.core

import java.util.HashMap

import scala.compat.Platform
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashSet
import scala.collection.mutable.Stack
import scala.collection.JavaConversions._
import ornl.elision.util.ElisionException
import ornl.elision.util.seqloop
import ornl.elision.core.BasicAtomComparator._

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
  override lazy val hashCode = op.hashCode * 12289 + arg.hashCode
  override lazy val otherHashCode = op.otherHashCode + 8191*arg.otherHashCode
  lazy val matchingCost = arg match {
    case as: AtomSeq => as.matchingCost
    case _           => 0
  }
  
  
  if (BasicAtom.trackedOperators.contains(
    this.op match {
      case x: Operator    => x.name
      case x: OperatorRef => x.name
      case _              => ""
    })) {
    hasTrackedOps = true
  }
  
  arg match {
    case x: AtomSeq => {
      seqloop(x, (i:Int) =>
        hasTrackedOps = hasTrackedOps || x(i).hasTrackedOps
      )
    }
    case _ => {}
  }

  /*
   * This speeds things up, but FastLinkedList does not work the way
   * it should.
   * 
  if (BasicAtom.trackedOperators.contains(
        this.op match {
          case x : Operator => x.name
          case x : OperatorRef => x.name
          case _ => ""
        }
    )) {
    myApplies.append(this)
    realApplies.add(this)
  }
  arg match {
    case x: AtomSeq => {
      for (a <- x) {
        myApplies.appendAll(a.myApplies)
        realApplies.addAll(a.realApplies)
      }
    }
    case _ => {}
  }
  println("Applies for " + this.toParseString)
  myApplies.reset()
  var node = myApplies.next()
  while (node != null) {
    val app = node.data
    println("Apply:   " + app.toParseString)
    node = myApplies.next()
  }
  myApplies.reset()
  node = myApplies.next()
  var fakeApplies : java.util.HashSet[Apply] = new java.util.HashSet[Apply]()
  while (node != null) {
    val app = node.data
    fakeApplies.add(app)
    node = myApplies.next()
  }
  for (app <- realApplies) {
    if (!fakeApplies.contains(app)) {
      println("** MISSING apply " + app.toParseString)
      println("** Curr apply = " + this.toParseString)
      println("** Applies of arguments:")
      arg match {
        case x: AtomSeq => {
          for (a <- x) {
            println("**  Arg = " + a.toParseString)
            a.myApplies.reset()
            node = a.myApplies.next()
            while (node != null) {
              val app = node.data
              println("**    " + app.toParseString)
              node = a.myApplies.next()
            }
          }
        }
        case _ => {}
      }
      throw new LambdaUnboundedRecursionException(arg.loc, "Apply tracking error")
    }
  }
  */

  override def equals(other: Any) = (other match {
      case oapp: Apply =>
        feq(oapp, this, ((op == oapp.op) && (arg == oapp.arg)))
        
      case _ =>
        false
    })

  def rewrite(binds: Bindings) = {
    val (nop, nof) = op.rewrite(binds)
    val (narg, naf) = arg.rewrite(binds)
    if (nof || naf) {
  		(Apply(nop, narg), true)
  	} else { 
      (this, false)
    }
  }
  
  def replace(map: Map[BasicAtom, BasicAtom]) = {
    map.get(this) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newop, flag1) = op.replace(map)
        val (newarg, flag2) = arg.replace(map)
        if (flag1 || flag2) {
          (Apply(newop, newarg), true)
        } else {
          (this, false)
        }
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
  def apply(op: BasicAtom, 
            arg: BasicAtom,
            bypass: Boolean = false): BasicAtom = {
    // Do not try to compute if metaterms are present.
    var retval: BasicAtom = null
    if (!op.evenMeta && !arg.isTerm) {
      retval = SimpleApply(op, arg)
    } else {
      op match {
  	case StringLiteral(typ, str) if arg.isInstanceOf[StringLiteral] =>
  	  // If the argument is also a string literal, then we want to simply
  	  // concatenate them.
  	  retval = StringLiteral(typ, str + arg.asInstanceOf[StringLiteral].value)
          
  	case app:Applicable =>
  	  try {
  	    // The lhs is applicable; invoke its apply method.  This will
  	    // return some atom, and that atom is the overall result.
  	    retval = app.doApply(arg, bypass)
  	  } catch {
  	    case ex:java.lang.StackOverflowError =>
              // Trapped unbounded recursion.
  	      throw new LambdaUnboundedRecursionException(arg.loc,
  		                                          "Application results in unbounded recursion: (" +
  		                                          op.toParseString + ").(" + arg.toParseString + ")")
  	  }
  	
  	case rew:Rewriter =>
  	  // The lhs is a rewriter; invoke its rewrite method.  This will return
  	  // a pair.  We need to convert the pair to a binding.
  	  val (r_atom, r_flag) = rew.doRewrite(arg)
  	retval = BindingsAtom(Bindings() +
  	                      ("atom" -> r_atom) +
  	                      ("flag" -> (if (r_flag) Literal.TRUE else Literal.FALSE)))
        
  	case _ =>
  	  // The lhs is something else.  It may be a variable or some other
  	  // expression that we have yet to evaluate.  Just build a simple
  	  // apply of the lhs and rhs.
  	  retval = SimpleApply(op, arg)
      }
    }

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

    // Do we need to compute this?
    if (myVars == null) {

      // This is used a lot, so it needs to be fast. We will find all
      // the variables here with a stack to avoid recursive calls.
      myVars = new HashSet[BasicAtom]
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
            myVars.add(v)
          }
          
          // Any other type of atom we ignore.
          case _ => {}
        }
      }

    }

    return Some(myVars)
  }

  /**
   * Get the operators in the operator arguments, plus this operator.
   */
  override def getOperators(opNames: HashSet[String]): Option[HashSet[BasicAtom]] = {

    this.synchronized {

      // Do we need to compute the operators contained in the current
      // Apply?
      if (myOperators == null) {

        // Yes, look for the tracked operators. Since looking for the
        // operators is a high cost action, we will look for all of them
        // at once.
        myOperators = new java.util.HashMap[String, HashSet[Apply]]()
        for (opName <- BasicAtom.trackedOperators) {
          myOperators.put(opName, new HashSet[Apply])
        }

        // This is used a lot, so it needs to be fast. We will find all
        // the variables here with a stack to avoid recursive calls.
        var work = new Stack[BasicAtom]
        var done = new HashSet[BasicAtom]
        work.push(this)
        while (!work.isEmpty) {
        work.pop match {

            // Are we working on an operator instance?
            case op: OpApply => {

              // Is this apply one of the ones we are looking for?
              val currName = op.op.name
            
              if (BasicAtom.trackedOperators.contains(currName)) {
                myOperators.get(currName).add(op)
              }

              // Push all the operator arguments on the stack to check, if
              // we have not already checked this operator instance and
              // the argument contains some of the operators instance we
              // are looking for.
              if (!(done contains op) && (op.hasTrackedOps)) {
                for (a <- op.arg) if (a.hasTrackedOps) work.push(a)
                done += op
              }
            }
          
            // Any other type of atom we ignore.
            case _ => {}
          }
        }      
      }

      // Collect up all of the desired operator instances.
      var r : HashSet[BasicAtom] = new HashSet[BasicAtom]()
      for (desiredOp <- opNames) {
        if (!BasicAtom.trackedOperators.contains(desiredOp)) {
          throw new ElisionException(arg.loc, "Operator '" + desiredOp +
                                     "' is not tracked. Add with BasicAtom::trackOperator().")
        }

        // Was a new operator to track added after we computed the
        // current myOperators map?
        if (myOperators.get(desiredOp) == null) {

          // For now just go and completely recompute myOperators.
          myOperators = null
          return getOperators(opNames)
        }
        r.addAll(myOperators.get(desiredOp))
      }
      return Some(r)
    }
  }

    /*
     * Faster, but FastLinkedList does not work the way it should.
     * 
    //println("** Getting operators " + opNames)
    var r : HashSet[BasicAtom] = new HashSet[BasicAtom]
    var missing : HashSet[String] = new HashSet[String]
    for (desiredOp <- opNames) {

      // Is this one of the operators we are tracking?
      if (BasicAtom.trackedOperators.contains(desiredOp)) {

        // Have we already computed this?
        if (myOperators.containsKey(desiredOp)) {
          r = r ++ myOperators.get(desiredOp)
        }
        
        // No, save it for later computation.
        else {
          missing = missing.+(desiredOp)
        }
      }
    }

    // Find any of the operators that we are have not already computed.
    if (missing.size() > 0) {

      var newOpMap : HashMap[String, HashSet[Apply]] = new HashMap[String, HashSet[Apply]]
      for (op <- missing) {
        newOpMap.put(op, new HashSet[Apply])
      }
      myApplies.reset()
      var node = myApplies.next()
      while (node != null) {
      //for (app <- myApplies) {
        val app = node.data
        val name = app.op match {
          case x : Operator => x.name
          case x : OperatorRef => x.name
          case _ => ""
        }
        if (missing.contains(name)) {
          newOpMap.put(name, newOpMap.get(name).+(app))
          r = r + app
        }
        node = myApplies.next()
      }

      // Cache the operators.
      for (op <- missing) {
        myOperators.put(op, newOpMap.get(op))
      }
    }

    //println("** Operators " + opNames + "in " + this.toParseString + " = " + r)
    return Some(r)
    }
    */
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
