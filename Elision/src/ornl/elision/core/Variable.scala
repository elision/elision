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

import scala.collection.mutable.HashSet
import scala.collection.mutable.OpenHashMap
import ornl.elision.util.other_hashify

/**
 * Represent a variable.
 *
 * == Structure and Syntax ==
 * A variable is indicated with a leading dollar sign (`$`) followed by a
 * valid symbol.  So the following are valid variables:
 * - `$``x`
 * - `$``Fred51_2`
 * - <code>$`1`</code>
 *
 * It is also possible to construct a "by name" variable.  This is a variable
 * that has an implicit guard (in addition to any other guards it may have)
 * that restricts the variable to only matching itself.  That is, the by-name
 * variable FOO matches only the variable FOO, and not anything else.  This
 * is denoted by enclosing the variable name in quotation marks.
 * - `$``"``FOO``"`
 * By-name variables otherwise behave as synonyms for the variable itself, so
 * if you directly bind `$``"FOO"` to 17, you have actually bound `$``FOO`
 * to 17.
 *
 * == Guards ==
 * A variable is allowed to have a guard.  The guard is substituted before the
 * variable is bound, and must evaluate to `true` to allow the binding to take
 * place.
 *
 * In fact, the variable "guard" is more.  The guard is specified as an atom
 * in curly braces after the variable name and before any type information.
 * For proposed binding of variable `$``x` to value `v`, with guard `g`, we do
 * the following.
 *
 * - If `g` is a [[ornl.elision.core.Rewriter]], then `g.a` is computed and
 *   if the flag is true, `$``x` is bound to the resulting atom.
 * - If `g` is a [[ornl.elision.core.Applicable]], then `g.a` is computed and
 *   `$``x` is bound the result.
 * - Otherwise `g` is assumed to be a predicate, and is rewritten with the
 *   potential bindings.  If the result is true, then `$``x` is bound to `v`.
 *
 * In all other cases the binding attempt is rejected.
 *
 * See the previous section for information on the implicit guard created for
 * a "by name" variable.
 *
 * == Type ==
 * Every variable must have a type, and the type can be `ANY`.
 *
 * == Equality and Matching ==
 * Variables are equal iff their name, type, guard, and labels are all equal.
 *
 * Variables can be bound, so that gets checked during matching.  A variable
 * pattern matches a subject iff it is already bound to that subject, or if it
 * is unbound and the types match.  Guards are not matched; this would be
 * problematic, as the guards are used to determine whether a match succeeds!
 * Labels are also not matched, as they serve a different semantic purpose.
 *
 * @param typ			The variable type.
 * @param name		The variable name.
 * @param guard		The variable's guard.  Default is true.
 * @param labels	Labels for this variable.  Default is none.
 * @param byName  If true, this is a "by name" variable.  Default is false.
 */
class Variable(typ: BasicAtom, val name: String,
  val guard: BasicAtom = Literal.TRUE,
  val labels: Set[String] = Set[String](),
  val byName: Boolean = false) extends BasicAtom {

  /** The prefix for this variable. */
  val prefix = "$"

  /** This variable is a term. */
  val isTerm = true
  val theType = typ
  val deBruijnIndex = 0
  /** Variables are not constant. */
  val isConstant = false
  val depth = 0
  /** By default, variables can be bound. */
  override val isBindable = true

  /**
   * This contains a single variable, itself.
   */
  override def getVariables(): Option[HashSet[BasicAtom]] = {
    val r = new HashSet[BasicAtom]
    r.add(this)
    return Some(r)
  }

  /**
   * Attempt to bind the variable.  The potential variable binding is added
   * to the provided bindings, and the variable guard is rewritten.  If the
   * guard is true, then the new bindings are returned.  If it is false, then
   * a [[ornl.elision.core.Fail]] instance is returned.
   *
   * @param subject		The atom to which the variable is to be bound.
   * @param binds			Other bindings that must be honored.
   */
  def bindMe(subject: BasicAtom, binds: Bindings): Outcome = {
    // If this is a by-name variable, reject immediately if the subject is not
    // a variable of the same name.
    if (byName) {
      subject match {
        case Variable(_, nm, _, _, _) if nm == name =>
        case _ => return Fail("By-name variable does not match.")
      }
    }
    // Check any guard.
    if (guard.isTrue) return Match(binds + (name -> subject))
    else {
        guard match {
          case rew: Rewriter =>
            // Rewrite the atom.
            val (newatom, flag) = rew.doRewrite(subject)
            if (flag) {
              return Match(binds + (name -> newatom))
            } else {
              return Fail("Variable guard rewriter returned false after rewrite.")
            }
          case app: Applicable =>
            // Apply and return.
            return Match(binds + (name -> app.doApply(subject)))
          case _ =>
            // Compute the bindings and check the guard.
            val newbinds = binds + (name -> subject)
            val newterm = guard.rewrite(newbinds)._1
            if (newterm.isTrue) return Match(newbinds)
            else return Fail("Variable guard failed.  Is now: " +
              newterm.toParseString)
        }
    }
  }

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
    hints: Option[Any]) = {
    // if the variable allows binding, and it is not already bound to a
    // different atom.  We also allow the variable to match ANY.
    if (isBindable) binds.get(name) match {
      case None =>
        // This is tricky.  We bind if we match against ANY.  Are
        // there unforseen consequences to this decision?  Otherwise we have
        // to add a binding of the variable name to the subject.
        bindMe(subject, binds)
      case Some(ANY) =>
        // We should re-bind this variable now.
        bindMe(subject, binds)
      case Some(atom) if subject == ANY || atom == subject =>
        // The variable is already bound, and it is bound to the subject, so
        // the match succeeds with the bindings as they are.
        Match(binds)
      case _ =>
        // The variable is already bound and it is bound to an unequal subject,
        // so the match fails.
        Fail("Variable " + this.toParseString +
          " is already bound to the term " + binds.get(name).get.toParseString +
          ".", this, subject)
    }
    else {
      // Variables that are not bindable cannot be bound, and cannot match
      // any subject.  This is to prevent allowing them to "match" a bindable
      // variable of the same name and type, and having chaos ensue.
      Fail("Variable is not bindable.", this, subject)
    }
  }

  def rewrite(binds: Bindings) = {
    // If we have no bindings, don't rewrite the variable.
    if (binds == null) {
      (this, false)
    } else {
      // If this variable is bound in the provided bindings, replace it with
      // the bound value.
      binds.get(name) match {
        case Some(atom) =>
          (atom, true)

        case None => {
          // Though the atom is not bound, its type still might have to be
          // rewritten.
          theType.rewrite(binds) match {
            case (newtype, true) =>
              (Variable(newtype, name), true)

            case _ => {
              (this, false)
            }
          }
        }
      }
    }
  }

  def replace(map: Map[BasicAtom, BasicAtom]) = {
    // Variables are complex critters.  We need to replace in (1) the type,
    // (2) the guard(s), and (3) the variable itself.  We try the easiest
    // case first.
    map.get(this) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag1) = theType.replace(map)
        val (newguard, flag2) = guard.replace(map)
        if (flag1 || flag2) {
          (Variable(newtype, name, newguard, labels, byName), true)
        } else {
          (this, false)
        }
    }
  }

  /**
   * Make a non-meta version of this variable.
   * @return  The new variable.
   */
  def asVariable = this

  /**
   * Make a meta version of this variable.
   * @return  The new metavariable.
   */
  def asMetaVariable = MetaVariable(typ, name, guard, labels, byName)

  override lazy val hashCode = typ.hashCode * 31 + name.hashCode
  override lazy val otherHashCode = typ.otherHashCode +
    8191 * (name.toString).foldLeft(BigInt(0))(other_hashify) + 1

  override def equals(varx: Any) = varx match {
    case ovar: Variable =>
      feq(ovar, this,
        ovar.theType == theType &&
          ovar.name == name &&
          ovar.guard == guard &&
          ovar.labels == labels &&
          ovar.isTerm == isTerm)

    case _ =>
      false
  }
}

/**
 * Simplified creation and matching of variables.
 */
object Variable extends {
  /**
   * Make a new variable instance.
   *
   * @param typ			The variable type.
   * @param name		The variable name.
   * @param guard   The variable's guard.  Default is true.
   * @param labels  Labels for this variable.  Default is none.
   * @param byName  If true, this is a "by name" variable.  Default is false.
   */
  def apply(typ: BasicAtom, name: String, guard: BasicAtom = Literal.TRUE,
    labels: Set[String] = Set[String](), byName: Boolean = false) =
    new Variable(typ, name, guard, labels, byName)

  /**
   * Extract the parts of a variable.
   *
   * @param vx	The variable.
   * @return	The type, name, guard, labels, and whether this is a "by name"
   *          variable.
   */
  def unapply(vx: Variable) = Some((vx.theType, vx.name, vx.guard, vx.labels,
    vx.byName))
}

/**
 * Define a metavariable.
 *
 * == Purpose ==
 * A metavariable is just like an ordinary variable, with the exception that
 * any metavariables in an atom make that atom a meta atom, and meta atoms
 * block evaluation in an apply.
 *
 * For example, consider `is_bindable(``$``x)`.  This will immediately
 * evaluate to `true` since `$``x` is bindable.  If we wanted to use this
 * as a guard for a variable, however, this won't work.  Instead we write
 * `is_bindable($``$``x)` using the metavariable, and evaluation is
 * deferred until the atom is rewritten.
 *
 * @param typ			The variable type.
 * @param name		The variable name.
 * @param guard   The variable's guard.  Default is true.
 * @param labels  Labels for this variable.  Default is none.
 * @param byName  If true, this is a "by name" variable.  Default is false.
 */
class MetaVariable(typ: BasicAtom, name: String,
  guard: BasicAtom = Literal.TRUE,
  labels: Set[String] = Set[String](),
  byName: Boolean = false)
  extends Variable(typ, name, guard, labels) {
  override val isTerm = false
  /** Metavariable prefix. */
  override val prefix = "$$"
  override lazy val hashCode = typ.hashCode * 37 + name.hashCode
  override lazy val otherHashCode = typ.otherHashCode +
    8193 * (name.toString).foldLeft(BigInt(0))(other_hashify) + 1

  /**
   * Make a non-meta version of this metavariable.
   * @return  The new variable.
   */
  override def asVariable = Variable(typ, name, guard, labels, byName)

  /**
   * Make a meta version of this metavariable.  I.e., do nothing.
   * @return  This metavariable.
   */
  override def asMetaVariable = this

  override def replace(map: Map[BasicAtom, BasicAtom]) = {
    // Variables are complex critters.  We need to replace in (1) the type,
    // (2) the guard(s), and (3) the variable itself.  We try the easiest
    // case first.
    map.get(this) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtype, flag1) = theType.replace(map)
        val (newguard, flag2) = guard.replace(map)
        if (flag1 || flag2) {
          (MetaVariable(newtype, name, newguard, labels, byName), true)
        } else {
          (this, false)
        }
    }
  }
}

/**
 * Companion object to make and match metavariables.
 */
object MetaVariable {
  /**
   * Make a new metavariable instance.
   *
   * @param typ			The variable type.
   * @param name		The variable name.
   * @param guard   The variable's guard.  Default is true.
   * @param labels  Labels for this variable.  Default is none.
   * @param byName  If true, this is a "by name" variable.  Default is false.
   */
  def apply(typ: BasicAtom, name: String, guard: BasicAtom = Literal.TRUE,
    labels: Set[String] = Set[String](), byName: Boolean) =
    new MetaVariable(typ, name, guard, labels, byName)

  /**
   * Extract the parts of a metavariable.
   *
   * @param vx	The variable.
   * @return	The type, name, guard, labels, and by-name status.
   */
  def unapply(vx: MetaVariable) = Some((vx.theType, vx.name, vx.guard,
    vx.labels, vx.byName))
}
