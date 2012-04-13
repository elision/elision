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
 * @param first		The first list of atoms in the form.
 * @param other		Other tagged lists in the form, if any.
 */
class SpecialForm(
    val tag: SymbolLiteral,
    val first: AtomSeq,
    val other: List[(SymbolLiteral, AtomSeq)]) extends BasicAtom {
  
  private val _children = List(tag, first) :::
  		other.foldLeft(List(tag, first))((x,y) => x ::: List(y._1, y._2))
  val depth = _children.foldLeft(0)(_ max _.depth) + 1
  val deBruijnIndex = _children.foldLeft(0)(_ max _.deBruijnIndex)
  val isConstant = _children.forall(_.isConstant)
  val theType = TypeUniverse
  val isTerm = _children.forall(_.isTerm)
  val constantPool = Some(BasicAtom.buildConstantPool(17, _children:_*))

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]): Outcome = subject match {
    case sf:SpecialForm =>
      SequenceMatcher.tryMatch(OmitSeq(tag, first), OmitSeq(sf.tag, sf.first))
    case _ => Fail("Special forms match only special forms.", this, subject)
  }

  def rewrite(binds: Bindings) = {
    var changed = false
    val newfirst = AtomSeq(first.props, first.map {
      atom =>
        val newatom = atom.rewrite(binds)
        changed |= newatom._2
        newatom._1
    })
    val newother = other.map {
      item =>
        (item._1, AtomSeq(item._2.props, item._2.map {
          atom =>
            val newatom = atom.rewrite(binds)
            changed |= newatom._2
            newatom._1
        }))
    }
    if (changed) (SpecialForm(tag, newfirst, newother), true) else (this, false)
  }
  
  override def toString = "SpecialForm(" + tag + ", " + first + ", " + other + ")"

  def toParseString(): String = "{ " + tag.toParseString +
  		first.mkParseString(" ", ", ", "") + other.map {
    item =>
      val key = "#" + item._1.toParseString
      val stuff = item._2.mkParseString(" ", ", ", "")
      key + stuff
  }.mkString(" ", "", "") + " }"
}

object SpecialForm {
  def apply(tag: SymbolLiteral, first: AtomSeq,
      other: List[(SymbolLiteral, AtomSeq)]) =
    new SpecialForm(tag, first, other)
}
