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
class MatchAtom(tag: BasicAtom, val pat: BasicAtom)
extends SpecialForm(tag, pat) with Applicable {
  /** The type of this atom. */
  override val theType = OperatorLibrary.MAP(ANY, BINDING)

  /**
   * Apply this match atom to the given atom.  This performs the matc
   * and returns either the bindings of the first match, or Nothing if
   * it does not match.
   * 
   * @param atom	The subject to match.
   * @return	Bindings, or Nothing.
   */
  def doApply(atom: BasicAtom): BasicAtom = pat.tryMatch(atom) match {
    case fail:Fail => NONE
    case Match(binds) => BindingsAtom(binds)
    case Many(iter) => BindingsAtom(iter.next)
  }
  
  /**
   * Make a Scala parseable representation of this atom.
   * 
   * @return 	A parseable atom.
   */
  override def toString = "MatchAtom(" + pat.toString + ")"
}

object MatchAtom {
  def apply(sfh: SpecialFormHolder): MatchAtom = {
    sfh.requireBindings.fetchAs[AtomSeq]("") match {
      case Args(atom: BasicAtom) =>
        new MatchAtom(sfh.tag, sfh.content)
      case x =>
        throw new SpecialFormException(
            "Did not find exactly one pattern: " + x.toParseString)
    }
  }
  def apply(pat: BasicAtom) = new MatchAtom(Literal('match), pat)
}