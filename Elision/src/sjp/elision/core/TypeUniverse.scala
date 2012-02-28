/*======================================================================
 *       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 * The Elision Term Rewriter
 * 
 * Copyright (c) 2012 by Stacy Prowell (sprowell@gmail.com)
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
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
======================================================================*/
package sjp.elision.core

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
object TypeUniverse extends RootType {
  /** The type of the type universe is itself. */
  val theType = this
  
  /** The type universe is constant. */
  val isConstant = true

  /** The type universe is represented by ^TYPE. */
  def toParseString = "^TYPE"
  
  /** The type universe contains no constant children. */
  val constantPool = None

  /**
   * Type universe matching is a special case, and the basis case, for matching
   * types.
   * 
   * @param subject	The subject to match.
   * @param binds		The bindings to honor.
   * @return	The outcome of the match.
   */
  override protected def matchTypes(subject: BasicAtom, binds: Bindings) =
    tryMatchWithoutTypes(subject, binds)
    
  /** The type universe is known (in Scala) as `TypeUniverse`. */
  override def toString = "TypeUniverse"
    
  /** Compute the hash code. */
  override lazy val hashCode = toParseString.hashCode
}
