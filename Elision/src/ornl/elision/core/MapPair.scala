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
======================================================================*/
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
package ornl.elision.core

/**
 * Provide an ordered pair that also serves as a very simple kind of rewrite
 * rule.
 * 
 * == Purpose ==
 * The map pair can use used to construct a pair, but when applied on the
 * left hand side of the applicative dot, it tries to match the right-hand
 * side against its left-hand side.  If the match succeeds, the result is
 * the rewrite of the map pair's right-hand side and a true flag.
 * 
 * At present there is no way to specify general guards, so the first match
 * of the left hand side is used immediately and unconditionally.  (Variable
 * guards are, of course, honored.)
 */
case class MapPair(left: BasicAtom, right: BasicAtom) extends BasicAtom
with Rewriter {
	/** A map pair is actually a strategy. */
  val theType = STRATEGY
  val isConstant = left.isConstant && right.isConstant
  val depth = (left.depth max right.depth) + 1
  val constantPool = Some(BasicAtom.buildConstantPool(11, left, right))
  val isTerm = left.isTerm && right.isTerm
  val deBruijnIndex = left.deBruijnIndex max right.deBruijnIndex
  override lazy val hashCode = left.hashCode * 31 + right.hashCode

  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]): Outcome = subject match {
    case MapPair(oleft, oright) =>
      SequenceMatcher.tryMatch(Vector(left, right),
          Vector(oleft, oright), binds)
    case _ =>
      Fail("Subject of match is not a pair.", this, subject)
  }

  def rewrite(binds: Bindings): (BasicAtom, Boolean) = {
    val newleft = left.rewrite(binds)
    val newright = right.rewrite(binds)
    if (newleft._2 || newright._2) (MapPair(newleft._1, newright._2), true)
    else (this, false)
  }

  /**
   * Apply this map pair to the given atom, yielding a potentially new atom.
   * The first match with the left-hand side is used to rewrite the right.
   * 
   * @param atom	The atom to rewrite.
   * @return	A pair consisting of a potentially new atom and a flag indicating
   * 					success or failure.
   */
  def doRewrite(atom: BasicAtom) = left.tryMatch(atom) match {
    case file:Fail => (atom, false)
    case Match(binds) =>
      val res = right.rewrite(binds)
      (res._1, true)
    case Many(iter) =>
      val res = right.rewrite(iter.next)
      (res._1, true)
  }

  def toParseString = "(" + left.toParseString + " -> " +
  		right.toParseString + ")"

  /**
   * Get the Scala code to create this instance.
   */
  override def toString = "MapPair(" + left.toString + ", " +
  		right.toString + ")"
}
