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

/** Simple operator prototype creation and pattern matching. */
object Proto {
  /**
   * Make a new operator prototype.
   * 
   * @param name				The operator name.
   * @param typ					The type of a fully-applied operator.
   * @param parameters	The formal parameters.
   * @return	The new operator prototype.
   */
  def apply(name: String, typ: BasicAtom, parameters: (String, BasicAtom)*) =
    OperatorPrototype(name, parameters.map(x => Variable(x._2, x._1)).toList, typ)
    
  /**
   * Deconstruct an operator prototype into its components for a pattern match.
   * This works with up to five parameters.
   * 
   * @param op		The operator prototype.
   * @return	The components of the operator prototype, organized by operator
   * 					name, then operator type, and then each of the formal parameters.
   */
  def unapply(op: OperatorPrototype) = op.pars match {
    case List() => Some(op.name, op.typ)
    case List(v1) => Some(op.name, op.typ, v1)
    case List(v1, v2) => Some(op.name, op.typ, v1, v2)
    case List(v1, v2, v3) => Some(op.name, op.typ, v1, v2, v3)
    case List(v1, v2, v3, v4) => Some(op.name, op.typ, v1, v2, v3, v4)
    case List(v1, v2, v3, v4, v5) => Some(op.name, op.typ, v1, v2, v3, v4, v5)
    case _ => None
  }
}


object Native {
  def apply(proto: OperatorPrototype, props: OperatorProperties,
      impl: (Bindings => BasicAtom)) = {
    val nat = NativeOperatorDefinition(proto, props)
    val op = Operator(nat)
    op.handler =
      Some((op: Operator, args: AtomList, binds: Bindings) => impl(binds))
    op
  }
}