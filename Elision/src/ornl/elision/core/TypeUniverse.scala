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
 * ======================================================================*/
package ornl.elision.core

import ornl.elision.util.other_hashify

/**
 * Define the unique type universe.
 * 
 * ==Structure and Syntax==
 * The type universe is an object (a singleton).  There is exactly one instance
 * (a constant) represented by the string `^TYPE`.
 * 
 * ==Type==
 * The type universe is a ''root type'', so it is its own type.
 * 
 * ==Equality and Matching==
 * The type universe is equal only to itself, and matches only itself.
 */
object TypeUniverse extends SymbolLiteral(null, Symbol("^TYPE")) {
  /** The type of the type universe is itself. */
  override val theType = this

  /**
   * Type universe matching is a special case, and the basis case, for matching
   * types.
   * 
   * @param subject	The subject to match.
   * @param binds		The bindings to honor.
   * @return	The outcome of the match.
   */
  override protected def matchTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) = tryMatchWithoutTypes(subject, binds, hints)
    
  /**
   * Try to match this type against the provided atom.  Note that root types
   * match only themselves, so the match works iff the subject is equal to this
   * pattern.
   */
  override def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) =
    // A root type matches only itself.
    if (this == subject) Match(binds)
    else Fail("This type matches only itself.", this, subject)

  /**
   * The root types cannot be rewritten, as they do not have children.
   */
  override def rewrite(binds: Bindings) = (this, false)
  
  override def replace(map: Map[BasicAtom, BasicAtom]) =
    map.get(this) match {
      case None => (this, false)
      case Some(atom) => (atom, true)
    }
    
  /** Compute the hash code. */
  override lazy val hashCode = 12289 * value.hashCode
  override lazy val otherHashCode = (value.toString).foldLeft(0L)(other_hashify)  
  override def equals(other: Any) = TypeUniverse eq other.asInstanceOf[AnyRef]
}
