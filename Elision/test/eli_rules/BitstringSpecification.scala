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
package eli_rules

import ornl.elision.repl.ERepl
import ornl.elision.repl.ReplMain
import org.specs2.mutable.Specification
import org.junit.runner.RunWith
import org.specs2.runner.JUnitRunner
import ornl.elision.cli.Setting
import java.io.File
import ornl.elision.cli.CLI
import scala.collection.mutable.HashMap
import ornl.elision.cli.CLI.CLIState
import ornl.elision.parse.Processor._
import ornl.elision.core.BasicAtom
import util.ProcessorUtil

/**
 * @author James Black
 *
 */
@RunWith(classOf[JUnitRunner])
class BitStringSpecification extends Specification with ProcessorUtil {
  repl.parse("", """inc("src/bootstrap/Bitstring.eli")""")

  "The 'bits' operation" should {
    val teststr = "bits(5, 8)"
    "atom created for 8 bit number" in {
      val atom = repl.parse("test1", teststr)
      println(atom.toString)
    }
    "atom rewritten for 8 bit number" in {
      val atoms = repl.parse("test1", teststr)
      atoms match {
        case repl.ParseSuccess(atoms) =>
          println(atoms.toString)
        case repl.ParseFailure(atoms) =>
          println(atoms.head)
      }
    }
    val teststr2 = "bits(-5, 8)"
    "atom created for 8 bit number negative" in {
      val atom = repl.parse("test1", teststr)
      println(atom.toString)
    }
    "atom rewritten for 8 bit number negative" in {
      val atoms = toBasicAtom(repl.parse("test1", teststr)) { atoms =>
        println(atoms.toString)
      } { msg => println(msg) }
    }
    val teststr3 = "bits(-5, 5)"
    "atom created for 5 bit number negative" in {
      val atom = repl.parse("test1", teststr)
      println(atom.toString)
    }
    "atom created for 5 bit number negative" in {
      val atoms = toBasicAtom(repl.parse("test1", teststr)) { atoms =>
        println(atoms.toString)
      } { msg => println(msg) }
    }
  }
  "The 'bit_from_binary_string' operation" should {
    val teststr = """bit_from_binary_string("11100101011000110111011101")"""
    "atom created for string" in {
      val atom = repl.parse("test2", teststr)
      println(atom.toString)
    }
    "atom after rewriting" in {
      val atoms = toBasicAtom(repl.parse("test1", teststr)) { atoms =>
        println(atoms.toString)
      } { msg => println(msg) }
    }
  }
  "The 'bit_length' operation" should {
    var teststr = "bit_length(bits(5, 8))"
    "atom created for 8 bit number" in {
      val atom = repl.parse("test1", teststr)
      println(atom.toString)
    }
    "atom after rewriting" in {
      val atoms = toBasicAtom(repl.parse("test1", teststr)) { atoms =>
        println(atoms.toString)
      } { msg => println(msg) }
    }
    val teststr2 = """bit_length(bit_from_binary_string("11100101011000110111011101"))"""
    "atom created for string" in {
      val atom = repl.parse("test2", teststr2)
      println(atom.toString)
    }
    "atom after rewriting" in {
      val atoms = toBasicAtom(repl.parse("test1", teststr2)) { atoms =>
        println(atoms.toString)
      } { msg => println(msg) }
    }
  }
  "The 'bit_as_binary_string' operation" should {
    var teststr = "bit_as_binary_string(bits(5, 8))"
    "atom created for 8 bit number" in {
      val atom = repl.parse("test1", teststr)
      println(atom.toString)
    }
    "atom after rewriting" in {
      val atoms = toBasicAtom(repl.parse("test1", teststr)) { atoms =>
        println(atoms.toString)
      } { msg => println(msg) }
    }
    val teststr2 = """bit_as_binary_string(bit_from_binary_string("11100101011000110111011101"))"""
    "atom created for string" in {
      val atom = repl.parse("test2", teststr2)
      println(atom.toString)
    }
    "atom after rewriting" in {
      val atoms = toBasicAtom(repl.parse("test1", teststr2)) { atoms =>
        println(atoms.toString)
      } { msg => println(msg) }
    }
  }
  "The 'bit_concat' operation" should {
    var teststr = "bit_concat(bits(5, 8), bits(" + 0xFCD + ",12))"
    "atom created concat to number" in {
      val atom = repl.parse("test1", teststr)
      println(atom.toString)
    }
    "atom after rewriting" in {
      val atoms = toBasicAtom(repl.parse("test1", teststr)) { atoms =>
        println(atoms.toString)
      } { msg => println(msg) }
    }
    var teststr2 = "bit_as_binary_string(bit_concat(bits(5, 8), bits(" + 0xFCD + ",12)))"
    "atom created for concat to binary string" in {
      val atom = repl.parse("test1", teststr2)
      println(atom.toString)
    }
    "atom after rewriting" in {
      val atoms = toBasicAtom(repl.parse("test1", teststr2)) { atoms =>
        println(atoms.toString)
      } { msg => println(msg) }
    }
  }
  "The 'bit_is_negative_signed' operation" should {
    val teststr = """bit_is_negative_signed(bit_from_binary_string("111001010110"))"""
    "test negative_signed with negative number" in {
      val atom = repl.parse("test2", teststr)
      println(atom.toString)
    }
    "atom after rewriting" in {
      val atoms = toBasicAtom(repl.parse("test1", teststr)) { atoms =>
        println(atoms.toString)
        atoms.toString must_== "true"
      } { msg => println(msg) }
    }
    val teststr2 = """bit_is_negative_signed(bit_from_binary_string("011001010110"))"""
    "test negative_signed with positive number" in {
      val atom = repl.parse("test2", teststr2)
      println(atom.toString)
    }
    "atom after rewriting" in {
      val atoms = toBasicAtom(repl.parse("test1", teststr2)) { atoms =>
        println(atoms.toString)
      } { msg => println(msg) }
    }
  }
  "The 'bit_is_zero' operation" should {
    val teststr = """bit_is_zero(bit_from_binary_string("0000000"))"""
    "test bit_is_zero with zero in bits" in {
      val atom = repl.parse("test2", teststr)
      println(atom.toString)
    }
    "atom after rewriting" in {
      toBasicAtom(repl.parse("test1", teststr)) { atoms =>
        println(atoms.toString)
        atoms.toString must_== "true"
      } { msg => println(msg) }
    }
    val teststr2 = "bit_is_zero(bits(-0, 3))"
    "test bit_is_zero with negative zero number" in {
      val atom = repl.parse("test2", teststr2)
      println(atom.toString)
    }
    "atom after rewriting" in {
      val atoms = toBasicAtom(repl.parse("test1", teststr2)) { atoms =>
        atoms.toString must not be "true"
      } { msg => println(msg) }
    }
  }
  //bit_is_zero
  //signed_value
  //unsigned_value
}