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
package ornl.elision.core

/**
 * Traverse an atom hierarchy.  The traversal is depth-first.  The atom is
 * visited, then its type is visited recursively (optionally), then the
 * children of the atom are visited.
 * 
 * The visitor can terminate the traversal by returning `false`.  This stops
 * further traversal.
 * 
 * To use this, make an instance and specify whether types should be visited.
 * Then pass an atom and a visitor to the `apply` method.  The `typ` argument
 * specifies whether the provided atom is a type (true) or term (false).  This
 * is passed to the visitor.
 * 
 * == Visitors ==
 * The visitor takes two arguments: the atom to visit and a flag indicating
 * whether the provided atom was reached as the type of an atom (true) or
 * as a term (false).  The visitor returns true to continue the traversal,
 * and false to terminate it.
 * 
 * == Types ==
 * Traversal is complicated by the fact that every atom has a type, and the
 * type can contain arbitrary atoms.  To make this a little better the visitor
 * is provided with a flag that indicates whether the atom is a type or term.
 * If true, the atom is the type of another atom.  If false, it is a term.
 * Note that children of the type are terms, and their types are types.
 * 
 * Type traversal is optional.  If disabled, then types are not visited.
 * 
 * == Handling Recursion ==
 * The traversal always terminates when `^TYPE` is reached, and does not
 * descend into the LIST(...) type to avoid an infinite recursion.
 * 
 * == Caution: Lazy Types ==
 * Note that exploration will cause lazy types to be generated, and can be
 * costly as a result.  To avoid this disable visiting types.
 * 
 * == Example ==
 * An example may help.  Given the atom `foo(5, 9, bar(21, \``$``x:INTEGER.``$``x))`
 * the traversal visits the nodes in the following way, with type visiting
 * enabled:
 * 
 * {{{
 * foo(5,9,bar(21,\$`:1`:INTEGER.$`:1`:INTEGER))
 *  INTEGER
 *   ^TYPE
 *  foo:OPREF
 *   OPREF
 *    ^TYPE
 *  %AC(5,9,bar(21,\$`:1`:INTEGER.$`:1`:INTEGER))
 *   LIST(INTEGER)
 *    ^TYPE
 *   %AC
 *    ^TYPE
 *    true
 *     BOOLEAN
 *      ^TYPE
 *    true
 *     BOOLEAN
 *      ^TYPE
 *   5
 *    INTEGER
 *     ^TYPE
 *   9
 *    INTEGER
 *     ^TYPE
 *   bar(21,\$`:1`:INTEGER.$`:1`:INTEGER)
 *    INTEGER
 *     ^TYPE
 *    bar:OPREF
 *     OPREF
 *      ^TYPE
 *    %(21,\$`:1`:INTEGER.$`:1`:INTEGER)
 *     LIST(ANY)
 *      ^TYPE
 *     %
 *      ^TYPE
 *     21
 *      INTEGER
 *       ^TYPE
 *     \$`:1`:INTEGER.$`:1`:INTEGER
 *      MAP(INTEGER,INTEGER)
 *       ^TYPE
 *       MAP:OPREF
 *        OPREF
 *         ^TYPE
 *       %(INTEGER,INTEGER)
 *        LIST(^TYPE)
 *         ^TYPE
 *        %
 *         ^TYPE
 *        INTEGER
 *         ^TYPE
 *        INTEGER
 *         ^TYPE
 *      $`:1`:INTEGER
 *       INTEGER
 *        ^TYPE
 *       true
 *        BOOLEAN
 *         ^TYPE
 *      $`:1`:INTEGER
 *       INTEGER
 *        ^TYPE
 *       true
 *        BOOLEAN
 *         ^TYPE
 * }}}
 * 
 * Alternately you can visit without types.
 * 
 * {{{
 * foo(5,9,bar(21,\$`:1`:INTEGER.$`:1`:INTEGER))
 * foo:OPREF
 * %AC(5,9,bar(21,\$`:1`:INTEGER.$`:1`:INTEGER))
 * %AC
 * true
 * true
 * 5
 * 9
 * bar(21,\$`:1`:INTEGER.$`:1`:INTEGER)
 * bar:OPREF
 * %(21,\$`:1`:INTEGER.$`:1`:INTEGER)
 * %
 * 21
 * \$`:1`:INTEGER.$`:1`:INTEGER
 * $`:1`:INTEGER
 * true
 * $`:1`:INTEGER
 * true
 * }}}
 * 
 * The above traversal is explained below.
 * 
 *  1. Visit `foo(...)`, an operator application.
 *  1. Visit the operator `foo:OPREF`.
 *  1. Visit the argument list `%AC(...)`, an atom sequence.
 *  1. Visit the properties `%AC`.
 *  1. Visit the associative specification `true`.
 *  1. Visit the commutative specification `true`.
 *  1. Visit each of the arguments.  The first is `5`.
 *  1. Visit the second argument `9`.
 *  1. Visit the third argument `bar(...)`, an operator application.
 *  1. Visit the operator `bar:OPREF`.
 *  1. Visit the argument list `%(...)`, an atom sequence.
 *  1. Visit the properties `%`.
 *  1. Visit each of the arguments.  The first is `21`.
 *  1. Visit the second argument, a lambda.
 *  1. Visit the lambda variable ```$``:1`.
 *  1. Visit the lambda variable's guard `true`.
 *  1. Visit the lambda body, consisting of a variable.
 *  1. Visit the variable's guard, `true`.
 * 
 * @param withtypes If true, visit types.  If false, do not.
 * @author Stacy Prowell (prowellsj@ornl.gov)
 */
class AtomWalker(withtypes: Boolean) {
  
  import AtomWalker.Visitor
  
  /**
   * Visit the type of an atom.
   * 
   * @param atom      The atom whose type is to be visited.
   * @param visitor   The visitor.
   * @return  False iff the visitor stopped traversal.
   */
  private def _gotype(atom: BasicAtom, visitor: Visitor) =
    (!withtypes) || apply(atom.theType, visitor, true)
  
  /* Trouble at the Old Well, or Why Traversal is Harder Than It Looks
   * =================================================================
   * 
   * The first issue with traversal is that some types are self-referencing.
   * The main culprit is ^TYPE, whose type is ^TYPE, whose type is ^TYPE, ...
   * This is easily handled in the case where we process Literals.
   * 
   * The other culprit is more subtle.  Suppose you are given the atom %().
   * The type of this is LIST(ANY).  Great.  We know the type of LIST(ANY) is
   * ^TYPE, and we've already handled that, so we're all good, right?  Well,
   * no.
   * 
   * LIST(ANY) is actually an apply of the form LIST:OPREF.%(ANY).  The
   * right-hand side is %(ANY), whose type is... wait for it... LIST(ANY).
   * Okay, so this is trouble.  This is not a trivial loop as with ^TYPE.
   * 
   * The type hierarchy is well-defined and simple.
   * %(): LIST(ANY): ^TYPE
   * 
   * The trouble arises when we explore the LIST(ANY).  So... what should
   * the exploration of this look like?
   * 
   * Visit %().  Then visit its type LIST(ANY).  It is tempting to stop at
   * this point, but consider %(1).  It's type is LIST(INTEGER).  Don't we
   * want to visit that INTEGER in there?  Even more important, consider
   * %($x:$T).  It's type is LIST($T).  Shouldn't we visit that $T?
   * 
   * Anything we find by visiting the LIST(...) we would have already found
   * by visiting the term, so there's really not much point.  We simply stop
   * the traversal when we encounter LIST(...).  Of course, we do still report
   * its type ^TYPE.
   * 
   * This means when we see %() the traversal looks like:
   * %(), LIST(ANY), ^TYPE
   * Of course, then we visit the algorithmic properties node, and get:
   * %, ^TYPE
   */
  
  /**
   * Visit an optional atom.  If the optional atom is not `None`, then the
   * atom is unwrapped and passed to the visitor.
   * 
   * @param atom    The optional atom to visit.
   * @param visitor The visitor.
   * @return  False iff the visitor stopped traversal.
   */
  def apply(atom: Option[BasicAtom], visitor: Visitor,
      typ: Boolean): Boolean = {
    atom match {
      case None => true
      case Some(atom) => apply(atom, visitor, typ)
    }
  }

  /**
   * Handle visiting a literal.  This is where named root types and the type
   * universe are handled.
   * 
   * @param atom      The atom to visit.
   * @param visitor   The visitor.
   * @param typ       Is this atom a type?
   * @return  False iff the visitor terminates the traversal.
   */
  def apply(atom: Literal[_], visitor: Visitor, typ: Boolean): Boolean =
    visitor(atom, typ) &&
    (if (atom != TypeUniverse) _gotype(atom, visitor) else true)
  
  /**
   * Handle visiting an algorithmic property node.
   * 
   * @param atom      The atom to visit.
   * @param visitor   The visitor.
   * @param typ       Is this atom a type?
   * @return  False iff the visitor terminates the traversal.
   */
  def apply(atom: AlgProp, visitor: Visitor, typ: Boolean): Boolean =
    visitor(atom, typ) &&
    _gotype(atom, visitor) &&
    apply(atom.absorber, visitor, false) &&
    apply(atom.associative, visitor, false) &&
    apply(atom.commutative, visitor, false) &&
    apply(atom.idempotent, visitor, false) &&
    apply(atom.identity, visitor, false)
  
  /**
   * Handle visiting a special form.  This is where operators, rules, and
   * other special forms are handled.
   * 
   * @param atom      The atom to visit.
   * @param visitor   The visitor.
   * @param typ       Is this atom a type?
   * @return  False iff the visitor terminates the traversal.
   */
  def apply(atom: SpecialForm, visitor: Visitor, typ: Boolean): Boolean =
    visitor(atom, typ) &&
    _gotype(atom, visitor) &&
    apply(atom.tag, visitor, false) &&
    apply(atom.content, visitor, false)
  
  /**
   * Handle visiting an atom.  This is where general dispatch occurs to other
   * `apply` methods, and most atoms are handled.
   * 
   * @param atom      The atom to visit.
   * @param visitor   The visitor.
   * @param typ       Is this atom a type?
   * @return  False iff the visitor terminates the traversal.
   */
  def apply(atom: BasicAtom, visitor: Visitor, typ: Boolean): Boolean = {
    atom match {
      // Process literals.
      case lit: Literal[_] =>
        apply(lit, visitor, typ)
      
      // Process algebraic properties.
      case ap: AlgProp =>
        apply(ap, visitor, typ)
        
      // Process special forms.
      case sf: SpecialForm =>
        apply(sf, visitor, typ)
      
      case OperatorRef(operator) =>
        visitor(atom, typ) &&
        _gotype(atom, visitor)
        
      case Apply(lhs, rhs) =>
        visitor(atom, typ) &&
        _gotype(atom, visitor) &&
        (if (lhs == SymbolicOperator.LIST) {
          true
        } else {
          apply(lhs, visitor, false) &&
          apply(rhs, visitor, false)          
        })
        
      case AtomSeq(props, atoms) =>
        visitor(atom, typ) &&
        _gotype(atom, visitor) &&
        apply(props, visitor, false) &&
        atoms.forall(apply(_, visitor, false))
        
      case BindingsAtom(binds) =>
        visitor(atom, typ) &&
        _gotype(atom, visitor) &&
        binds.forall(pair => apply(pair._2, visitor, false))
        
      case Lambda(lvar, body) =>
        visitor(atom, typ) &&
        _gotype(atom, visitor) &&
        apply(lvar, visitor, false) &&
        apply(body, visitor, false)
        
      case MapPair(left, right) =>
        visitor(atom, typ) &&
        _gotype(atom, visitor) &&
        apply(left, visitor, false) &&
        apply(right, visitor, false)
        
      case RulesetRef(name) =>
        visitor(atom, typ) &&
        _gotype(atom, visitor)
        
      case mvari: MetaVariable =>
        visitor(atom, typ) &&
        _gotype(atom, visitor) &&
        apply(mvari.guard, visitor, false)
        
      case vari: Variable =>
        visitor(atom, typ) &&
        _gotype(atom, visitor) &&
        apply(vari.guard, visitor, false)
    }
  }
}

/**
 * Provide simple access to an atom walker.  This implements the visitor
 * pattern.
 * 
 * To use this pass the atom and the visitor to the `apply` method.
 */
object AtomWalker {

  /**
   * Type of the visitor.  Visitors take an atom and a Boolean, and must
   * return a Boolean.  The atom is the atom being visited, and the Boolean
   * argument is true iff the atom is a type, and false if not.  If the
   * visitor wishes to cancel traversal it should return false.  Otherwise
   * to continue traversal it returns true.
   * 
   * A simple visitor that prints each atom as it is visited is:
   * `(atom,_) => println(atom.toParseString)`
   * 
   * A simple visitor that counts variable instances is:
   * {{{
   * var count = 0
   * def countvars(atom: BasicAtom, typ: Boolean) = {
   *   if (atom.isInstanceOf[Variable]) count++
   *   true
   * }
   * }}}
   * Then pass the closure `countvars _`.
   */
  type Visitor = (BasicAtom, Boolean) => Boolean
  
  /**
   * Start atom traversal at the given atom, invoking the visitor for every
   * atom encountered.  This implements the visitor pattern.
   * 
   * For details on how this all works, see the documentation for
   * [[ornl.elision.core.AtomWalker.Visitor]] and especially the companion
   * class.
   * 
   * @param atom      The atom to visit.
   * @param visitor   The visitor to invoke for every atom.
   * @param withtypes If true, visit types.  If false (the default), do not.
   * @return  False iff the visitor terminates the traversal.
   */
  def apply(atom: BasicAtom, visitor: Visitor, withtypes: Boolean = false): Boolean = 
    (new AtomWalker(withtypes))(atom, visitor, false)
}
