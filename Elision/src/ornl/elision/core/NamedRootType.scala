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

import ornl.elision.util.other_hashify

/**
 * Provide a very simple implementation of `RootType`.  This is a trivial root
 * type with a specified name and the type universe as its type.
 * 
 * == Registry ==
 * Named root types assume they can be parsed by their name, only.  This means
 * the rewriter must know about all named root types.  In order to make sure
 * we know all the root types, we keep a //global// registry of them in the
 * companion object.
 * 
 * The registry is updated by the constructor of this class, so there is no
 * need to update it manually.
 * 
 * == Use ==
 * If you want to create a new named root type, you should actually create an
 * object that extends this class.  The following is an example of the Elision
 * `BINDING` root type.
 * {{{
 * object BINDING extends NamedRootType("BINDING")
 * }}}
 * This registers the root type, and also causes the type to be available by
 * name in the Scala environment.
 * 
 * If you use this approach, be sure to reference the new type before you try
 * to parse it.  This causes the type to be constructed, and thus entered into
 * the registry.  If you aren't able to parse the root type, or it parses as
 * a symbol, this is the most likely cause.
 * 
 * @param name	The name.
 */
class NamedRootType protected (val name: String)
extends SymbolLiteral(TypeUniverse, Symbol(name)) {
  // Save this instance in the registry.  This is essential so that the parser
  // can find root types.
  NamedRootType._set(this)
  
  /**
   * Try to match this type against the provided atom.  Note that root types
   * match only themselves, so the match works iff the subject is equal to this
   * pattern.
   */
  override def tryMatchWithoutTypes(subject: BasicAtom, 
                                    binds: Bindings,
	                            hints: Option[Any]) = {
    // A root type matches only itself.
    if (this == subject) Match(binds)
    else Fail("This type matches only itself.", this, subject)
  }

  /**
   * The root types cannot be rewritten, as they do not have children.
   */
  override def rewrite(binds: Bindings) = (this, false)
  
  /** Generate the hash code. */
  override lazy val hashCode = 12289 * name.hashCode
  override lazy val otherHashCode = (name.toString).foldLeft(0L)(other_hashify)
  
  /**
   * Because of careful use of names, two named root types are equal
   * iff they have the same name.
   */
  override def equals(other: Any) = other match {
    case NamedRootType(oname) if name == oname => true
    case _ => false
  }
}

/**
 * The companion object maintains a registry of the named root types that have
 * been created.  This can be used during parsing to detect the use of named
 * root types.
 * 
 * This companion object also provides methods to make and match named root
 * types.
 */
object NamedRootType {
  /** The map of names to known root types. */
  private lazy val _known =
    scala.collection.mutable.OpenHashMap[String,NamedRootType]()
  
  /**
   * Make a new named root type.  In general you can avoid this method, and
   * prefer instead to extend the class.  However, if you need to create a
   * named root type programmatically, this will work.
   * 
   * @param name		The name of the new root type.
   * @return	The new root type.
   */
  def apply(name: String) =
    _known.getOrElseUpdate(name, new NamedRootType(name))
  
  /**
   * Extract the name of the root type.
   * 
   * @param nrt	The named root type.
   * @return	The name.
   */
  def unapply(nrt: NamedRootType) = Some(nrt.name)
  
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
  private def _set(typ: NamedRootType) = _known.getOrElseUpdate(typ.name, typ)

  // Initialize the default set of root types.  This just forces them to all
  // get constructed, and then installed.  It must be the last thing in this
  // object.
  List(STRING, SYMBOL,
      INTEGER, FLOAT, BITSTRING,
      BOOLEAN, RULETYPE,
      OPREF, STRATEGY,
      BINDING,
      ANY, NONE)
}

//======================================================================
// Make some well-known types.
// Note that the type universe gets its own entire class, since it is
// special (it has itself as its type).  In particular, it is not a
// "named root type."
//======================================================================

/** The STRING type. */
object STRING extends NamedRootType("STRING")
/** The SYMBOL type. */
object SYMBOL extends NamedRootType("SYMBOL")
/** The INTEGER type. */
object INTEGER extends NamedRootType("INTEGER")
/** The FLOAT type. */
object FLOAT extends NamedRootType("FLOAT")
/** The BITSTRING type. */
object BITSTRING extends NamedRootType("BITSTRING")
/** The BOOLEAN type. */
object BOOLEAN extends NamedRootType("BOOLEAN")
/** A type for all rules. */
object RULETYPE extends NamedRootType("RULETYPE")
/** A type for all operators. */
object OPREF extends NamedRootType("OPREF")
/** A type for all rulesets. */
object RSREF extends NamedRootType("RSREF")
/** A type for all strategies. */
object STRATEGY extends NamedRootType("STRATEGY")
/** A type for all bindings. */
object BINDING extends NamedRootType("BINDING")

/**
 * The unusual type ANY that matches anything (even NONE).
 */
object ANY extends NamedRootType("ANY") {
  override def tryMatch(subject: BasicAtom, binds: Bindings = Bindings(),
      hints: Option[Any] = None) = Match(binds)
}

/**
 * The unusual type NONE that matches just itself and ANY.
 */
object NONE extends NamedRootType("NONE") {
  override def tryMatch(subject: BasicAtom, binds: Bindings = Bindings(),
      hints: Option[Any] = None) =
    if (subject == NONE || subject == ANY) Match(binds)
    else Fail("NONE only matches itself and ANY.")
}
