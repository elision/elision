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
 * This is the generalized "special form" of atom.
 * 
 * @param tag			The tag identifying this particular form.
 * @param content	The content of the atom.
 */
class SpecialForm(val tag: BasicAtom, val content: BasicAtom)
extends BasicAtom {

  val depth = (tag.depth max content.depth) + 1
  val deBruijnIndex = tag.deBruijnIndex max content.deBruijnIndex
  val isConstant = tag.isConstant && content.isConstant
  val theType = TypeUniverse
  val isTerm = tag.isTerm && content.isTerm
  val constantPool = Some(BasicAtom.buildConstantPool(17, tag, content))
  
  override def equals(other: Any) = other match {
    case sf:SpecialForm => tag == sf.tag && content == sf.content
    case _ => false
  }

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]): Outcome = subject match {
    case sf:SpecialForm =>
      tag.tryMatch(sf.tag) match {
        case fail:Fail => Fail("Tags do not match.", tag, sf.tag, Some(fail))
        case Match(newbinds) => content.tryMatch(sf.content, newbinds, hints)
        case Many(matches) =>
          Many(MatchIterator(content.tryMatch(sf.content, _, hints), matches))
      }
    case _ => Fail("Special forms match only special forms.", this, subject)
  }

  def rewrite(binds: Bindings) = {
    val newtag = tag.rewrite(binds)
    val newcontent = content.rewrite(binds)
    if (newtag._2 || newcontent._2) (SpecialForm(newtag._1, newcontent._1), true)
    else (this, false)
  }
  
  override def toString = "SpecialForm(" + tag + ", " + content + ")"

  def toParseString(): String =
    "{: " + tag.toParseString + " " + content.toParseString + " :}"
}

object SpecialForm {
  def apply(tag: BasicAtom, content: BasicAtom) = tag match {
    case _ => new SpecialForm(tag, content)
  }
}
