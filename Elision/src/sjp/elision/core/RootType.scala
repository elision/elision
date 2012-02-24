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
package sjp.elision.core

/**
 * A "root type" is a simple type that is well-known and used globally in the
 * system.  It cannot be rewritten, and matches only itself.
 */
abstract class RootType extends BasicAtom {
	// The index of a symbol or root type is zero.
  val deBruijnIndex = 0
  
  /**
   * Unless overridden, the depth of all root types is zero.
   */
  val depth = 0
  
  /**
   * Try to match this type against the provided atom.
   * @param subject	The subject to match.
   * @param binds		The bindings to honor.
   * @return	The outcome of the match.
   */
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    // A root type matches only itself.
    if (this == subject) Match(binds)
    else Fail("This type matches only itself.", this, subject)

  /**
   * The root types cannot be rewritten.
   * @param binds	The bindings.
   * @return	This type.
   */
  def rewrite(binds: Bindings) = (this, false)
}

/**
 * Trivial root type with a specified name and the type universe as its type.
 * @param name	The name.
 */
case class NamedRootType(name: String) extends RootType {
  // All named root types are in the type universe.
  val theType = TypeUniverse
  
  val isConstant = true
  
  // The parse string is the name, as a symbol.
  def toParseString = toESymbol(name)
  
  // We have to override the Scala to protect the string.
  override def toString = "NamedRootType(" + toEString(name) + ")"
  
  override lazy val hashCode = name.hashCode()
}

//======================================================================
// Make some well-known types.
// Note that the type universe gets its own entire class, since it is
// special (it has itself as its type).
//======================================================================

/** The STRING type. */
object STRING extends NamedRootType("STRING")
/** The SYMBOL type. */
object SYMBOL extends NamedRootType("SYMBOL")
/** The INTEGER type. */
object INTEGER extends NamedRootType("INTEGER")
/** The FLOAT type. */
object FLOAT extends NamedRootType("FLOAT")
/** The BOOLEAN type. */
object BOOLEAN extends NamedRootType("BOOLEAN")
/** A type for all rules. */
object RULETYPE extends NamedRootType("RULETYPE")
/** A type for all operators. */
object OPTYPE extends NamedRootType("OPTYPE")

/**
 * The unusual type ANY that matches anything.
 */
object ANYTYPE extends NamedRootType("ANYTYPE") {
  override def tryMatch(subject: BasicAtom, binds: Bindings = new Bindings) =
    Match(binds)
}
