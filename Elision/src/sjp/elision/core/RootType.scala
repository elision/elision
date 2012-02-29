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
 * 
 * The implementation of `RootType` does not allow these types to have children.
 */
abstract class RootType extends BasicAtom {
	/** The index of a symbol or root type is zero. */
  val deBruijnIndex = 0
  
  /** Unless overridden, the depth of all root types is zero. */
  val depth = 0

  /**
   * Try to match this type against the provided atom.  Note that root types
   * match only themselves, so the match works iff the subject is equal to this
   * pattern.
   * 
   * @param subject	The subject to match.
   * @param binds		The bindings to honor.
   * @return	The outcome of the match.
   */
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings) =
    // A root type matches only itself.
    if (this == subject) Match(binds)
    else Fail("This type matches only itself.", this, subject)

  /**
   * The root types cannot be rewritten, as they do not have children.
   * 
   * @param binds	The bindings.
   * @return	This type.
   */
  def rewrite(binds: Bindings) = (this, false)
}

/**
 * Provide a very simple implementation of `RootType`.  This is a trivial root
 * type with a specified name and the type universe as its type.
 * 
 * Named root types assume they can be parsed by their name, only.  This means
 * the rewriter must know about all named root types.  In order to make sure
 * we know all the root types, we keep a //global// registry of them in the
 * companion object.
 * 
 * @param name	The name.
 */
case class NamedRootType(name: String) extends RootType {
  // Save this instance in the registry.  This is essential so that the parser
  // can find root types.
  NamedRootType.set(this)
  
  /** All named root types are in the type universe. */
  val theType = TypeUniverse
  
  /** All named root types are constants. */
  val isConstant = true
  
  /** Root types contain no constant children. */
  val constantPool = None
  
  /**
   * The parse string is the name of the root type, as a symbol.  For this to
   * work the parser must know about all these types.
   */
  def toParseString = toESymbol(name)
  
  /** Generate the Scala, and protect the string. */
  override def toString = "NamedRootType(" + toEString(name) + ")"
  
  /** Generate the hash code. */
  override lazy val hashCode = name.hashCode()
}

/**
 * The companion object maintains a registry of the named root types that have
 * been created.  This can be used during parsing to detect the use of named
 * root types.
 */
object NamedRootType {
  /** The map of names to known root types. */
  private val _known = scala.collection.mutable.HashMap[String,NamedRootType]()
  
  /**
   * Get the named root type, if it is known.
   * 
   * @param name	The potential root type name.
   * @return	The root type, if there is one, or None if there is not.
   */
  def get(name: String) = _known.get(name)
  
  /**
   * Save the named root type.
   * 
   * @param typ	The named root type.
   * @return	The named root type.  This might be a different instance if
   * 					the type has already been declared.
   */
  def set(typ: NamedRootType) = _known.getOrElseUpdate(typ.name, typ)
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
/** A type for all strategies. */
object STRATEGY extends NamedRootType("STRATEGY")
/** A type for all bindings. */
object BINDINGS extends NamedRootType("BINDINGS")

/**
 * The unusual type ANY that matches anything.
 */
object ANYTYPE extends NamedRootType("ANYTYPE") {
  override def tryMatch(subject: BasicAtom, binds: Bindings = new Bindings) =
    Match(binds)
}
