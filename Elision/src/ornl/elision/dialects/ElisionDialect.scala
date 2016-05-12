/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2013 by Stacy Prowell (sprowell@gmail.com).
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
package ornl.elision.dialects

import scala.io.Source
import ornl.elision.core.BasicAtom
import ornl.elision.core.Dialect
import ornl.elision.parse.FastEliParser
import ornl.elision.parse.Failure
import ornl.elision.util.Loc
import ornl.elision.parse.Success
import java.io.Reader

/**
 * Provide the Elision dialect.  Atoms can be parsed and serlialized.
 */
class ElisionDialect extends Dialect {

  override val canSerialize = true
  override val canParse = true
  
  override def serialize(app: Appendable, atom: BasicAtom, limit: Int = -1) = {
    ElisionGenerator.apply(atom, app, limit)
  }
  
  override def parse(name: String, reader: Reader) =
    (new FastEliParser(name, false)).parseAtoms(
        reader, ornl.elision.core.knownExecutor.context) match {
    case Success(atoms) =>
      Dialect.Success(atoms)
      
    case Failure(msg) =>
      Dialect.Failure(Loc.internal, msg)
  }
}