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
 * @author ysp
 *
 */
case class MatchAtom(pat: BasicAtom) extends BasicAtom with Applicable {
  /** The type of this atom. */
  val theType = OperatorLibrary.MAP(ANY, BINDING)
  
  /** The apply is constant iff its pattern is. */
  val isConstant = pat.isConstant
  
  /** This is a term iff its pattern is. */
  val isTerm = pat.isTerm
  
  /** The constant pool aggregated from the child. */
  val constantPool = Some(BasicAtom.buildConstantPool(13, pat))
  
  /** The depth is one more than the depth of the child. */
  val depth = pat.depth + 1
  
  /** The De Bruijn index computed from the child. */
  val deBruijnIndex = pat.deBruijnIndex
  
  /** The hash code for this apply. */
  override lazy val hashCode = "match".hashCode * 31 + pat.hashCode

  /**
   * Apply this match atom to the given atom.  This performs the matc
   * and returns either the bindings of the first match, or Nothing if
   * it does not match.
   * 
   * @param atom	The subject ot match.
   * @return	Bindings, or Nothing.
   */
  def doApply(atom: BasicAtom): BasicAtom = pat.tryMatch(atom) match {
    case fail:Fail => NONE
    case Match(binds) => BindingsAtom(binds)
    case Many(iter) => BindingsAtom(iter.next)
  }

  /**
   * Try to match this atom against another.
   * 
   * @param subject	The subject.
   * @param binds		Bindings to honor.
   * @param hints		Hints.
   */
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]): Outcome = subject match {
    case MatchAtom(opat) => pat.tryMatch(opat, binds, hints)
    case _ => Fail("Subject is not a match atom.", this, subject)
  }

  /**
   * Rewrite this atom with the provided bindings.
   * 
   * @param binds		The bindings to apply.
   * @return	The rewitten atom, and a flag indicating success.
   */
  def rewrite(binds: Bindings) = pat.rewrite(binds) match {
    case (newpat, true) => (MatchAtom(newpat), true)
    case _ => (this, false)
  }

  /**
   * Make a parseable string for this match atom.
   * 
   * @return	A parseable atom.
   */
  def toParseString() = "{ match " + pat.toParseString + " }"
  
  /**
   * Make a Scala parseable representation of this atom.
   * 
   * @return 	A parseable atom.
   */
  override def toString = "MatchAtom(" + pat.toString + ")"
}
