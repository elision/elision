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

import ornl.elision.util.ElisionException

/**
 * A context provides access to operator libraries and rules, along with
 * the global set of bindings in force at any time.
 *
 * '''This class is likely to change.'''
 *
 * == Use ==
 * In general it is not necessary to make an instance; one is typically
 * provided by a higher-level (semantically) class.
 *
 * This class provides for management of four things:
 *  - A set of [[ornl.elision.core.Bindings]].
 *  - An instance of [[ornl.elision.core.OperatorLibrary]].
 *  - Rulesets.
 *  - "Automatic" rewriting of atoms using rules.
 */
class Context extends Fickle with Mutable {

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
  // Printing.
  //======================================================================

  /**
   * Generate a newline-separated list of rules that can be parsed using the
   * atom parser to reconstruct the set of rules in this context.
   *
   * @return	The parseable rule sets.
   */
  def toParseString = {
    val buf = new StringBuilder
    buf append "// START of context.\n"
    buf append "// START of operator library.\n"
    buf append operatorLibrary.toParseString
    buf append "// END of operator library.\n"
    buf append "// START of rule library.\n"
    buf append ruleLibrary.toParseString
    buf append "// END of rule library.\n"
    buf append "// END of context.\n"
    buf.toString()
  }

  /**
   * Generate a newline-separated list of rules that can be parsed by Scala
   * to reconstruct the set of rules in this context.
   *
   * @return	The parseable rule sets.
   */
  override def toString = {
    val buf = new StringBuilder
    buf append "object LoadContext {\n"
    buf append "  import ornl.elision.core._\n\n"
    buf append "  val _context = new Context()\n\n"
    buf append "  def main(args: Array[String]) {\n"
    buf append "    Ops(_context)\n"
    buf append "    Rules(_context)\n"
    buf append "  }\n"
    buf append "  def apply():Context = {\n"
    buf append "    Ops(_context)\n"
    buf append "    Rules(_context)\n"
    buf append "    _context\n"
    buf append "  }\n"
    buf append "}\n\n"
    buf append operatorLibrary.toString
    buf append ruleLibrary.toString
    buf.toString()
  }
  //  override def toString = {
  //    val buf = new StringBuilder
  //    buf append "import ornl.elision.core._\n"
  //    buf append "object LoadContext {\n"
  //    buf append "  def main(args: Array[String]) {\n"
  //    buf append "    val _context = new Context()\n"
  //    buf append "    _mkoplib(_context)\n"
  //    buf append "    _mkrulelib(_context)\n"
  //    buf append "    println(_context.toParseString)\n"
  //    buf append "  }\n"
  //    buf append operatorLibrary.toString
  //    buf append ruleLibrary.toString
  //    buf append "}\n"
  //    buf.toString()
  //  }
}
