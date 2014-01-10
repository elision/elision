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
package ornl.elision.context

import scala.collection.mutable.Set
import ornl.elision.core.BasicAtom
import ornl.elision.core.Bindings
import ornl.elision.core.Fickle
import ornl.elision.core.Operator
import ornl.elision.core.OperatorRef
import ornl.elision.core.RewriteRule
import ornl.elision.core.RulesetRef
import ornl.elision.core.SymbolicOperator
import ornl.elision.core.TypedSymbolicOperator
import ornl.elision.core.AtomWalker
import ornl.elision.core.Literal
import ornl.elision.core.MapPair
import ornl.elision.core.SymbolLiteral
import ornl.elision.core.toEString
import ornl.elision.core.Variable
import ornl.elision.dialects.ScalaGenerator
import ornl.elision.dialects.ElisionGenerator
import ornl.elision.util.Cache
import ornl.elision.util.Version
import ornl.elision.util.Version.build
import ornl.elision.util.Version.major
import ornl.elision.util.Version.minor
import ornl.elision.util.toQuotedString
import ornl.elision.core.Dialect

/**
 * A context provides access to operator libraries and rules, along with
 * the global set of bindings in force at any time.
 * 
 * Additionally the context maintains a cache for use during runtime.  This
 * cache is reflected through an `Executor`, but is actually maintained here.
 *
 * '''This class is likely to change.'''
 *
 * == Use ==
 * In general it is not necessary to make an instance; one is typically
 * provided by a higher-level (semantically) class.
 *
 * This class provides for management of four things:
 *  - A set of [[ornl.elision.core.Bindings]].
 *  - An instance of [[ornl.elision.context.OperatorLibrary]].
 *  - Rulesets.
 *  - "Automatic" rewriting of atoms using rules.
 */
class Context extends Fickle with Mutable with Cache {

  override def clone = {
    val clone = new Context
    clone.binds = this.binds.clone
    clone.operatorLibrary = this.operatorLibrary.clone
    clone.ruleLibrary = this.ruleLibrary.clone
    clone
  }

  //======================================================================
  // Global bindings management.
  //======================================================================

  /** The current bindings. */
  private var _binds: Bindings = Bindings()

  /**
   * Bind a variable in this context.
   *
   * @param vname		The variable name to bind.
   * @param atom		The atom to bind to the variable.
   * @return	This context.
   */
  def bind(vname: String, atom: BasicAtom) = {
    _binds += (vname -> atom)
    this
  }

  /**
   * Unbind a variable in this context.
   *
   * @param vname		The variable name.
   * @return	This context.
   */
  def unbind(vname: String) = {
    _binds -= vname
    this
  }

  /**
   * Get the current bindings for this context.
   *
   * @return	The bindings for this context.
   */
  def binds = _binds

  /**
   * Set the bindings to use. Any prior value is lost.
   *
   * @param bindings    The new bindings.
   * @return            This context.
   */
  def binds_=(bindings: Bindings) = {
    require(bindings != null)
    _binds = bindings
    this
  }

  //======================================================================
  // Operator library management.
  //======================================================================

  /** The current operator library. */
  private var _oplib: OperatorLibrary = _

  /**
   * Get the current operator library.  If none has explicitly been set, then
   * a default instance is created and returned.
   *
   * @return	The current operator library.
   */
  def operatorLibrary = {
    if (_oplib == null) { _oplib = new OperatorLibrary() }
    _oplib
  }

  /**
   * Set the operator library to use.  Any prior value is lost.
   *
   * @param lib	The new operator library.
   * @return	This context.
   */
  def operatorLibrary_=(lib: OperatorLibrary) = {
    require(lib != null)
    _oplib = lib
    this
  }

  //======================================================================
  // Rule library management.
  //======================================================================

  /** The current rule library. */
  private var _rulelib: RuleLibrary = _

  /**
   * Get the current rule library.  If none has explicitly been set, then
   * a default instance is created and returned.
   *
   * @return	The current rule library.
   */
  def ruleLibrary = {
    if (_rulelib == null) { _rulelib = new RuleLibrary() }
    _rulelib
  }

  /**
   * Set the rule library to use.  Any prior value is lost.
   *
   * @param lib	The new rule library.
   * @return	This context.
   */
  def ruleLibrary_=(lib: RuleLibrary) = {
    require(lib != null)
    _rulelib = lib
    this
  }
  
  //======================================================================
  // Serialization
  //======================================================================
  
  /* How Serialization Works
   * =======================
   * Serialization is complicated by the fact that some operators depends on
   * other operators in their definition.  It is also the case that an operator
   * named X might depend on another operator named X, captured as a closure.
   * Likewise some operators and other objects depend on rulesets, and those
   * have to be declared appropriately.  Finally, the order of rule declaration
   * is significant.
   * 
   * Serialization of a context works as follows.
   * (1) Serialize the rules in the order they were declared.  Whenever an
   *     operator or ruleset is encountered, emit its declaration right there,
   *     and also emit anything it depends on.
   * (2) Emit any ruleset declarations not already visited.
   * (3) Emit any operators not already visited, first emitting any operators
   *     and rulesets they depend on.
   * (4) Emit the bindings.
   * 
   * If we are creating Scala code, then we have to handle native operators.
   * To make that simpler, we use a trick to avoid compilation of the native
   * operators.  The trick is to emit the native handler code so it is
   * installed at operator creation, which is allowed by a protected method.
   */
  
  /**
   * Declare an atom to this context.  If the atom is the kind that is held in
   * the context (an operator, a rule, a ruleset name, a map pair that maps a
   * variable to an atom) then it is added to the correct registry.
   * 
   * @param atom    The atom to declare.
   * @return  The atom.
   */
  def declare(atom: BasicAtom) = atom match {
    case op: Operator =>
      operatorLibrary.add(op)
      op
      
    case SymbolLiteral(_, sym) =>
      ruleLibrary.declareRuleset(sym.name)
      RulesetRef(this, sym.name)
      
    case rule: RewriteRule =>
      ruleLibrary.add(rule)
      rule
      
    case MapPair(vari: Variable, value: BasicAtom) =>
      bind(vari.name, value)
      value
      
    case x: Any =>
      x
  }
  
  /**
   * Write the context to the given appendable, as Scala source.  This assumes
   * that the context is available as `context`, and consists of the commands
   * to create the context.
   * 
   * @param app     The appendable to get the context.  By default a new string
   *                buffer is created.
   */
  def write(app: Appendable = new StringBuffer) = {
    // Now we can append the code to create the context.  To do this, we first
    // traverse the rule list and generate all the rules, in the order they are
    // present in the library.
    var known = Known()
    for (rule <- ruleLibrary.getAllRules) {
      // Write the rule and any dependencies.  The new set of "known" stuff is
      // returned, and we preserve it.
      known = traverse(app, rule, known, 'scala)
    } // Write all rules and their dependencies.
    
    // Now we can write any remaining unknown operators.  Just traverse the
    // operators and trust the system to write any that are unknown.
    for (operator <- operatorLibrary.getAllOperators) {
      known = traverse(app, operator, known, 'scala)
    } // Write all rules and their dependencies.
    
    // Any remaining rulesets can be written now.
    for (ruleset <- ruleLibrary.getAllRulesets) {
      known = traverse(app, RulesetRef(ruleLibrary, ruleset), known, 'scala)
    } // Add any missed rulesets.
    
    // Enable those rulesets that need to be enabled.
    import ornl.elision.util.toQuotedString
    for (ruleset <- ruleLibrary.getActiveRulesets) {
      app.append("context.ruleLibrary.enableRuleset(%s)\n".format(
          toQuotedString(ruleset)))
    } // Enable the rulesets that need to be enabled.
    
    // Emit the bindings.
    for (bind <- binds) {
      app.append("context.bind(%s, %s)\n" format (
          toQuotedString(bind._1), bind._2.toString))
    } // Write all bindings.
    
    // Emit the cache.  The cache can contain arbitrary stuff, so here we
    // only preserve one item: the list of included files.
    val included = fetchAs[Set[String]]("read_once.included", Set[String]())
    app.append("    import scala.collection.mutable.Set\n")
    app.append("    val set = scala.collection.mutable.Set(")
    app.append(included map (toQuotedString(_)) mkString (","))
    app.append(")\n")
    app.append("    context.stash(\"read_once.included\", set)\n")
  }
  
  /**
   * Maintain the set of known operators and rulesets.  Note that this is
   * immutable
   * 
   * @param operators The set of known operators.
   * @param rulesets  The set of known rulesets.
   */
  case class Known(operators: Set[Operator] = Set(),
      rulesets: Set[RulesetRef] = Set()) {
    
    /**
     * Test whether an atom (an operator or ruleset) is known.
     * 
     * @param atom    Can be any atom, but only operators and rulesets have
     *                the potential to be "known."
     * @return  True iff the atom is known.
     */
    def apply(atom: BasicAtom) = atom match {
      case op: Operator if operators.contains(op) => true
      case rs: RulesetRef if rulesets.contains(rs) => true
      case _ => false
    }
    
    /**
     * Add an atom to the known set.
     * 
     * @param atom    The atom to add.
     * @return  A new `Known` instance.
     */
    def +(atom: BasicAtom) = atom match {
      case op: Operator =>
        Known(operators + op, rulesets)
        
      case rs: RulesetRef =>
        Known(operators, rulesets + rs)
    }
  }
  
  /**
   * Write an atom's dependencies, then write the atom itself.  The
   * dependencies considered are operators and rulesets.
   * 
   * The idea is that by processing the resulting declarations in order the
   * same atom is reconstructed.
   * 
   * @param app         An appendable to get the output.
   * @param target      The atom.
   * @param known       The known items.
   * @param kind        The format for the output.  Can be either `'elision`
   *                    or `'scala`.
   * @param withhandler If true then emit the handler object in the stream
   *                    to be added to the `NativeCompiler`.
   * @return  The updated known 
   */
  def traverse(app: Appendable, target: BasicAtom, known: Known,
      kind: Symbol, withhandler: Boolean = true): Known = {
    // Go and get the items, in the order they should be declared.
    val pair = collect(target, known, List())
    val newknown = pair._1
    val thelist = pair._2
    
    // Boilerplate text for both cases.
    val pre = Map(
        'elision -> ("{!_($atom) #handler=\"\"\"context.declare(args(0))\"\"\""+
        		" #evenmeta=true}.%("),
        'scala -> "context.declare(")
    val post = Map(
        'elision -> ")",
        'scala -> ")")
    
    // Write the atoms.
    for (atom <- thelist) {
      // Write each atom.  If we are writing Scala code, the handler is requested,
      // and this is an operator with a handler, then create and write the handler
      // object now so it gets compiled along with everything else.
      if (kind == 'scala && withhandler) {
        target match {
          case tso: TypedSymbolicOperator =>
            // See if the operator has a native handler.
            tso.handlertxt match {
              case Some(text) =>
                // Found a handler.  Convert it to an object and write it in the
                // stream.
                NativeCompiler.writeStash(tso.loc.source, tso.name, text, app)
                
              case _ =>
            }
            
          case _ =>
        }
      }
      app.append(pre(kind))
      kind match {
        case 'scala =>
          // Ruleset references are unusual.  We need to process them as symbols.
          // The reason for this is that there is no corresponding atom to
          // convert them into, like there is for operator references.  See
          // the declare method for how this is handled.
          val what = target match {
            case rr: RulesetRef => Literal(Symbol(rr.name))
            case x => x
          }
          Dialect.serialize('scala, app, what)
          
        case _ =>
          // Ruleset references are unusual.  We need to process them as symbols.
          // The reason for this is that there is no corresponding atom to
          // convert them into, like there is for operator references.  See
          // the declare method for how this is handled.
          val what = target match {
            case rr: RulesetRef => Literal(Symbol(rr.name))
            case x => x
          }
          Dialect.serialize('elision, app, what)
      }
      app.append(post(kind)).append('\n')
    } // Write all atoms, in order.
    newknown
  }
  
  /**
   * Construct a list of all an atom's dependencies, including the atom itself.
   * The dependencies considered are operators, rulesets, etc.  The list is
   * returned in the order the atoms should be declared.
   * 
   * The idea is that by processing the resulting declarations in order the
   * same atom is reconstructed.  Internally-defined operators are instances
   * of [[ornl.elision.core.SymbolicOperator]], and these are skipped.
   * 
   * @param target      The atom.
   * @param known       The known items.
   * @param initial     Initial content of the list - possibly including a
   *                    prior execution.
   * @return  The updated list (based on initial) and known. 
   */
  def collect(target: BasicAtom, known: Known,
      initial: List[BasicAtom]): (Known, List[BasicAtom]) = {
    // Skip known stuff.
    if (known(target)) return (known, initial)
    
    // (1) Skip over internally defined operators (MAP, LIST, xx).  These are
    //     SymbolicOperators, but not TypedSymbolicOperators.
    // (2) For operator references actually traverse the operator.
    target match {
      case tso: TypedSymbolicOperator =>
        
      case so: SymbolicOperator =>
        return (known, initial)
        
      case or: OperatorRef =>
        return collect(or.operator, known, initial)
        
      case _ =>
    }

    // Keep a version of known we can modify.  Then we will add known stuff
    // as we write it.
    var newknown = known
    var newlist = initial
    
    // A visitor to collect mentioned stuff.  Note that the symbolic
    // operators are hard-coded, and we never collect them.
    def visitor(atom: BasicAtom, istype: Boolean) = {
      if (atom != target) {
        atom match {
          case op: TypedSymbolicOperator =>
            if (! known(op)) {
              val pair = collect(op, newknown, newlist)
              newknown = pair._1
              newlist = pair._2
            }
              
          case sop: SymbolicOperator =>
            
          case op: Operator =>
            if (! known(op)) {
              val pair = collect(op, newknown, newlist)
              newknown = pair._1
              newlist = pair._2
            }
              
          case or: OperatorRef =>
            val op = or.operator
            if (! known(op)) {
              val pair = collect(op, newknown, newlist)
              newknown = pair._1
              newlist = pair._2
            }
              
          case rs: RulesetRef =>
            if (! known(rs)) {
              val pair = collect(rs, newknown, newlist)
              newknown = pair._1
              newlist = pair._2
            }
              
          case rule: RewriteRule =>
              val pair = collect(rule, newknown, newlist)
              newknown = pair._1
              newlist = pair._2
              
          case _ =>
        }
      }
      true
    }
    
    // Collect all the atoms this atom depends on.
    target match {
      case opref: OperatorRef =>
        AtomWalker(opref.operator, visitor, true)
        
      case rule: RewriteRule =>
        for (rsname <- rule.rulesets) {
          AtomWalker(RulesetRef(this.ruleLibrary, rsname), visitor, true)
        } // Explore all the referenced rulesets.
        AtomWalker(target, visitor, true)
        
      case _ =>
        AtomWalker(target, visitor, true)
    }
    
    // Write this atom.
    newlist :+= target
    
    // This is now a known operator or ruleset - if that's what it is.
    return (target match {
      case op: Operator => newknown + op
      case opref: OperatorRef => newknown + opref.operator
      case rs: RulesetRef => newknown + rs
      case _ => newknown
    }, newlist)
  }

  //======================================================================
  // Printing.
  //======================================================================

  /**
   * Generate a newline-separated list of rules that can be parsed using the
   * atom parser to reconstruct the set of rules in this context.
   *
   * @return	The parseable rule sets.
   */
  def toParseString = {
    val app = new StringBuffer
    app append "// START of context.\n"
    // Now we can append the code to create the context.  To do this, we first
    // traverse the rule list and generate all the rules, in the order they are
    // present in the library.
    var known = Known()
    for (rule <- ruleLibrary.getAllRules) {
      // Write the rule and any dependencies.  The new set of "known" stuff is
      // returned, and we preserve it.
      known = traverse(app, rule, known, 'elision)
    } // Write all rules and their dependencies.
    
    // Now we can write any remaining unknown operators.  Just traverse the
    // operators and trust the system to write any that are unknown.
    for (operator <- operatorLibrary.getAllOperators) {
      known = traverse(app, operator, known, 'elision)
    } // Write all rules and their dependencies.
    
    // Any remaining rulesets can be written now.
    for (ruleset <- ruleLibrary.getAllRulesets) {
      known = traverse(app, RulesetRef(ruleLibrary, ruleset), known, 'elision)
    } // Add any missed rulesets.
    
    // Enable those rulesets that need to be enabled.
    for (ruleset <- ruleLibrary.getActiveRulesets) {
      app.append("{!_()#handler=\"\"\"context.ruleLibrary." +
      		"enableRuleset(%s);_no_show\"\"\"}.%%()\n" format (toEString(ruleset)))
    } // Enable the rulesets that need to be enabled.
    
    // Emit the bindings.
    for (bind <- binds) {
      app.append("{!_()#handler=\"\"\"context.bind(%s,%s);_no_show\"\"\"}" +
      		".%%()\n" format (toEString(bind._1), bind._2.toString))
    } // Write all bindings.
    
    // Emit the cache.  The cache can contain arbitrary stuff, so here we
    // only preserve one item: the list of included files.
    val included = fetchAs[Set[String]]("read_once.included", Set[String]())
    app.append("{!_()#handler=\"\"\"\n")
    app.append("  import scala.collection.mutable.Set\n")
    app.append("""  val set = scala.collection.mutable.Set(""")
    app.append(included map (toEString(_)) mkString (","))
    app.append(")\n")
    app.append("  context.stash[Set[String]](\"read_once.included\", set)\n")
    app.append("  _no_show\n")
    app.append("\"\"\"}.%()\n")

    // Emit the cache.
    app append "// END of context.\n"
    app.toString()
  }

  /**
   * Return Scala code that can be compiled to generate this context.
   * 
   * @return  Compilable Scala code for this context.
   */
  override def toString = {
    val buf = new StringBuffer
    write(buf)
    buf.toString()
  }
}
