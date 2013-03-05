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
package ornl.elision.parse

import ornl.elision.core.BasicAtom
import ornl.elision.core.Context
import ornl.elision.core.MapPair
import ornl.elision.core.AlgProp
import ornl.elision.core.Absorber
import ornl.elision.core.Identity
import ornl.elision.core.Associative
import ornl.elision.core.Commutative
import ornl.elision.core.Idempotent
import ornl.elision.core.Variable
import ornl.elision.core.MetaVariable
import ornl.elision.core.Lambda
import ornl.elision.core.SpecialForm
import ornl.elision.core.Apply
import ornl.elision.core.ANY
import ornl.elision.core.AtomSeq
import ornl.elision.core.NoProps
import ornl.elision.core.BindingsAtom
import ornl.elision.core.Bindings
import ornl.elision.core.Literal
import ornl.elision.core.OperatorRef
import ornl.elision.core.RulesetRef
import ornl.elision.core.TypeUniverse
import ornl.elision.core.SymbolLiteral
import ornl.elision.core.StringLiteral


/**
 * Base class for abstract syntax tree nodes.
 * 
 * @param TYPE  The type of `BasicAtom` held in this abstract syntax tree node.
 */
abstract class AST[+TYPE <: BasicAtom] {
  /**
   * Interpret this AST node to produce an atom of the specified type.
   * @param context   The context providing rulesets and operators.
   * @return  The generated atom.
   */
  def interpret(context: Context): TYPE
}


/**
 * Create abstract syntax tree nodes.
 */
object AST {
  /** Quick reference for an abstract syntax tree node holding a `BasicAtom`. */
  type BA = AST[BasicAtom]
  
  /**
   * Make a simple AST around a known atom.
   * 
   * @param atom  The atom to store.
   * @return  The AST node.
   */
  def known[KIND <: BasicAtom](atom: KIND) = new AST[KIND] {
    def interpret(context: Context) = atom
  }
  
  /**
   * Quick method to make a symbol AST.
   * 
   * @param value   Value of the symbol.
   * @param typ     The type AST.  If omitted, it is any.
   * @return  The new symbol AST.
   */
  def sym(value: String, typ: BA = any) = new AST[SymbolLiteral] {
    def interpret(context: Context) =
      SymbolLiteral(typ.interpret(context), Symbol(value))
  }
  
  /**
   * Quick method to make a string AST.
   * 
   * @param value   Value of the string.
   * @param typ     The type AST.  If omitted, it is any.
   * @return  The new string AST.
   */
  def string(value: String, typ: BA = any) = new AST[StringLiteral] {
    def interpret(context: Context) =
      StringLiteral(typ.interpret(context), value)
  }
  
  /**
   * Quick access to the type universe AST.
   */
  val typeuniverse = known(TypeUniverse)
  
  /**
   * Quick access to the ANY AST.
   */
  val any = known(ANY)
  
  /**
   * Make an AST for a map pair.
   * 
   * @param left  Left AST of the map pair (the pattern).
   * @param right Right AST of the map pair (the rewrite).
   * @return  The new map pair AST.
   */
  def mappair(left: BA, right: BA) = new AST[MapPair] {
    def interpret(context: Context) =
      MapPair(left.interpret(context), right.interpret(context))
  }
  
  /**
   * Make an AST node for applying one atom to another.
   * 
   * @param left  AST to left of applicative dot (the operator).
   * @param right AST to right of applicative dot (the argument).
   * @return  AST for the application.
   */
  def apply(left: BA, right: BA) = new BA {
    def interpret(context: Context) = 
      Apply(left.interpret(context), right.interpret(context))
  }
  
  /**
   * Make an absorber property AST.
   * 
   * @param atom  AST for the absorber.
   * @return  The new AST.
   */
  def absorber(atom: BA) = new AST[AlgProp] {
    def interpret(context: Context) = Absorber(atom.interpret(context))
  }
  
  /**
   * Make an identity property AST.
   * 
   * @param atom  AST for the identity.
   * @return  The new AST.
   */
  def identity(atom: BA) = new AST[AlgProp] {
    def interpret(context: Context) = Identity(atom.interpret(context))
  }
  
  /**
   * Make an associative property AST.
   * 
   * @param atom  AST for the associative condition.
   * @return  The new AST.
   */
  def associative(atom: BA) = new AST[AlgProp] {
    def interpret(context: Context) = Associative(atom.interpret(context))
  }
  
  /**
   * Make a commutative property AST.
   * 
   * @param atom  AST for the commutative condition.
   * @return  The new AST.
   */
  def commutative(atom: BA) = new AST[AlgProp] {
    def interpret(context: Context) = Commutative(atom.interpret(context))
  }
  
  /**
   * Make an idempotent property AST.
   * 
   * @param atom  AST for the idempotent condition.
   * @return  The new AST.
   */
  def idempotent(atom: BA) = new AST[AlgProp] {
    def interpret(context: Context) = Idempotent(atom.interpret(context))
  }
  
  /**
   * Make a variable AST.  This includes metavariables and "by name" variables.
   * 
   * @param meta    True iff this should be a metavariable.
   * @param name    The variable name.
   * @param byname  True iff this is a by-name variable.
   * @param guard   The guard AST.
   * @param typ     The type AST.
   * @param tags    A list of tags for the variable.
   * @return  The new variable AST.
   */
  def variable(meta: Boolean, name: String, byname: Boolean, guard: Option[BA],
      typ: Option[BA], tags: List[String]) = new AST[Variable] {
    def interpret(context: Context) = if (meta) {
      MetaVariable(typ.getOrElse(any).interpret(context), name,
          guard.getOrElse(known(true: BasicAtom)).interpret(context),
          tags.toSet, byname)
    } else {
      Variable(typ.getOrElse(any).interpret(context), name,
          guard.getOrElse(known(true: BasicAtom)).interpret(context),
          tags.toSet, byname)
    }
  }
  
  /**
   * Make a lambda AST.
   * 
   * @param param   The parameter AST.
   * @param body    The body AST.
   * @return  The new lambda AST.
   */
  def lambda(param: AST[Variable], body: BA) = new AST[Lambda] {
    def interpret(context: Context) =
      Lambda(param.interpret(context), body.interpret(context))
  }
  
  /**
   * Make a special form AST.
   * 
   * @param tag     The tag AST.
   * @param content The content AST.
   * @return  The new special form AST.
   */
  def special(tag: BA, content: BA) = new BA {
    def interpret(context: Context) =
      SpecialForm(tag.interpret(context), content.interpret(context))
  }
  
  /**
   * Combine algorithmic properties into a single AST.
   * 
   * @param props   A list of algorithmic property AST's.
   * @return  The new algorithmic property AST.
   */
  def algprop(props: List[AST[AlgProp]]) = new AST[AlgProp] {
    def interpret(context: Context) = props.foldLeft(NoProps: AlgProp) {
      (sofar, next) => sofar and next.interpret(context)
    }
  }
  
  /** Quick reference to the no-properties AST. */
  val noprops = algprop(List())
  
  /**
   * Make an atom sequence AST.
   * 
   * @param props   The algorithmic properties AST.
   * @param atoms   The sequence of atom AST's.
   * @return  The new sequence AST.
   */
  def atomseq(props: AST[AlgProp], atoms: List[BA]) = new AST[AtomSeq] {
    def interpret(context: Context) =
      AtomSeq(props.interpret(context),
          atoms.map(_.interpret(context)).toIndexedSeq)
  }
  
  /**
   * Construct a bindings AST.
   * 
   * @param pairs   A list of pairs consisting of name (string) and value AST.
   * @return  The new bindings AST.
   */
  def binding(pairs: List[(String, BA)]) = new AST[BindingsAtom] {
    def interpret(context: Context) = Bindings(pairs.map {
      (pair) => (pair._1, pair._2.interpret(context))
    }:_*)
  }
  
  /**
   * Construct an operator reference AST.
   * 
   * @param name    The name of the operator.
   * @return  The new AST.
   */
  def opref(name: String) = new AST[OperatorRef] {
    def interpret(context: Context) = context.operatorLibrary(name)
  }
  
  /**
   * Construct a ruleset reference AST.
   * 
   * @param name    The ruleset name.
   * @return  The new AST.
   */
  def rsref(name: String) = new AST[RulesetRef] {
    def interpret(context: Context) = context.ruleLibrary(name)
  }
}
