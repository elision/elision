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

import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer

/**
 * A bindings atom wraps a set of bindings and allows them to be treated as if
 * they were an atom (matched and rewritten, for instance).  Since this is
 * costly, and since bindings are critical to the operation of the rewriter,
 * this class is provided and typically operates by implicit conversion.
 *
 * ==Structure and Syntax==
 * 
 * ==Type==
 * 
 * ==Equality and Matching==
 * 
 */
case class BindingsAtom(mybinds: Bindings) extends BasicAtom with Applicable {
  require(mybinds != null, "Bindings are null.")
  
  /** The type of a bindings atom is the special bindings type. */
  val theType = BINDING
  
  /** This atom is constant iff the bound value of each variable is constant. */
  val isConstant = mybinds.values.forall(_.isConstant)
  
  /** The binding is a term iff all values are terms. */
  val isTerm = mybinds.values.forall(_.isConstant)
  
  /** The De Bruijn index is the maximum index of the bindings. */
  val deBruijnIndex = mybinds.values.foldLeft(0)(_ max _.deBruijnIndex)
  
  /** The depth is equal to the maximum depth of the bindings, plus one. */
  val depth = mybinds.values.foldLeft(0)(_ max _.depth) + 1
  
  /**
   * Build the constant pool.  This might be costly, and we might not need it
   * that often, so we make it lazy.
   */
  lazy val constantPool =
    Some(BasicAtom.buildConstantPool(theType.hashCode, mybinds.values.toSeq:_*))

  /**
   * Match this bindings atom against the provided atom.
   * 
   * Two binding atoms match iff they bind the same variables to terms that
   * can be matched.  The variables that are bound cannot be matched against
   * variables, but the bindings can be.
   * 
   * @param subject	The atom to match.
   * @param binds		Bindings to honor.
   * @param hints		Optional hints.
   * @return	The result of the match.
   */
  def tryMatchWithoutTypes(subject: BasicAtom, binds: Bindings,
      hints: Option[Any]) =
    subject match {
    case BindingsAtom(obinds) =>
      // The bindings must bind the same variables.  Check that first.
      if (mybinds.keySet != obinds.keySet) {
        Fail("Bindings bind different variables.", this, subject)
      }
      // Now iterate over the keys.  The ordering does not matter.  This creates
      // two lists of atoms that we then match using the sequence matcher.
      var mine = OmitSeq[BasicAtom]()
      var theirs = OmitSeq[BasicAtom]()
      for ((key, value) <- mybinds) {
        mine :+= value
        theirs :+= obinds(key)
      } // Build lists of atoms.
      SequenceMatcher.tryMatch(mine, theirs)
    case _ => Fail("Bindings can only match other bindings.", this, subject)
  }

  /**
   * Rewrite these bindings with the provided bindings.
   * 
   * @param binds	The binds to use to rewrite these bindings.
   * @return	A pair consisting of the potentially modified bindings, and a
   * 					Boolean true iff these bindings were changed in any way by the
   * 					rewrite.
   */
  def rewrite(binds: Bindings) = {
    var changed = false
    var newmap = Bindings()
    for ((key, value) <- mybinds) {
      val (newvalue, valuechanged) = value.rewrite(binds)
      changed |= valuechanged
      newmap += (key -> newvalue)
    } // Rewrite all bindings.
    if (changed) (BindingsAtom(newmap), true) else (this, false)
  }

  /**
   * The parseable representation of a bindings atom is roughly equivalent to
   * that of an object, except that the keyword "bind" is used.
   * 
   * @return	A parseable version of this atom.
   */
  def toParseString() = "{ bind " + (mybinds.map(pair =>
    toESymbol(pair._1) + " -> " + pair._2.toParseString)).mkString(", ") + " }"

  /**
   * Generate a Scala parseable representation of this atom.  This requires that
   * the variables names be processed to make them strings.  The toString
   * provided by the case class is insufficient.
   * 
   * @return	A Scala parseable string.
   */
  override def toString() = "BindingsAtom(" + mybinds.map(pair =>
    toEString(pair._1) + " -> " + pair._2).mkString("Map(", ",", ")") + ")"
    
  override def equals(other: Any) = other match {
    case BindingsAtom(obinds) if (obinds == mybinds) => true
    case _ => false
  }
  
  def doApply(atom: BasicAtom) =
    // Check the argument to see if it is a single symbol.
    atom match {
      case SymbolLiteral(SYMBOL, sym) =>
        // Try to extract the symbol from the binding.  If it is not there,
        // then the answer is Nothing.
        mybinds.get(sym.name) match {
          case Some(oatom) => oatom
          case _ => Literal.NOTHING
        }
      case _ =>
	      // Try to rewrite the argument using the bindings and whatever we get
	      // back is the result.
	      atom.rewrite(mybinds)._1
    }
}