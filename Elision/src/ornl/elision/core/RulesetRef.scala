/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2013 by Stacy Prowell (sprowell@gmail.com).
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
package ornl.elision.core

import ornl.elision.context.RuleLibrary
import ornl.elision.context.Context
import ornl.elision.util.other_hashify

/**
 * A ruleset reference.
 * 
 * == Purpose ==
 * A ruleset reference provides a way to refer to all rules in a particular
 * ruleset as a single atom.
 *
 * == Structure and Syntax ==
 * A ruleset reference is a strategy - that is, a rewriter.  When placed on
 * the left side of an applicative equals rules from the ruleset are tried
 * until all are exhausted and none apply, or a single rule is applied.  That
 * is, at most one rule is applied.
 * 
 * Syntactically ruleset references are symbols of type `RSREF`.
 * 
 * == Type ==
 * All ruleset references have type `RSREF`.
 * 
 * == Equality and Matching ==
 * Ruleset references are equal iff they are the same symbol, and also match
 * iff they are the same symbol.  Since all ruleset refernces have the same
 * type, they are matched by name alone.
 */
abstract class RulesetRef extends BasicAtom with Rewriter {
  val depth = 0
  val deBruijnIndex = 0
  val constantPool = None
  val isTerm = true
  val isConstant = true
  val theType = RSREF
  /** The name of the referenced ruleset. */
  val name: String
  
  /**
   * Ruleset references cannot be rewritten.
   */
  def rewrite(binds: Bindings) = (this, false)
  
  def replace(map: Map[BasicAtom, BasicAtom]) = map.get(this) match {
    case None =>
      (this, false)
    
    case Some(atom) =>
      (atom, true)
  }
    
  override def hashCode = 61*name.hashCode
  lazy val otherHashCode = (name.toString).foldLeft(BigInt(0))(other_hashify)+1
  
  override def equals(other: Any) =
    (other match {
      case rr:RulesetRef => rr.name == name
      case _ => false
    })
}

/**
 * Simplify creation and matching of ruleset references.
 */
object RulesetRef {
  /**
   * Extract the parts of a ruleset reference.
   * 
   * @param rr		The reference.
   * @return	The ruleset name.
   */
  def unapply(rr: RulesetRef) = Some((rr.name))
  
  /**
   * Make a new reference to the named ruleset in the rule library of the given
   * context.
   * 
   * @param context		The context.
   * @param name			The ruleset name.
   * @return	The new reference.
   */
  def apply(context: Context, name: String) =
    context.ruleLibrary.makeRulesetRef(name)
  
  /**
   * Make a new reference to the named ruleset in the given rule library.
   * 
   * @param library		The rule library.
   * @param name			The ruleset name.
   * @return	The new reference.
   */
  def apply(library: RuleLibrary, name: String) =
    library.makeRulesetRef(name)
}
