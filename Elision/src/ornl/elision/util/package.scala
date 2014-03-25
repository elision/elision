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
======================================================================
* */
package ornl.elision

/**
 * This is the utility package for Elision.
 * 
 * This package provides common services and support for other parts of the
 * Elision system.  Any part of Elision may use it, but it cannot use other
 * parts of the Elision system!  That is, it is a leaf in the use hierarchy.
 */
package object util {
  /**
   * Turn a string into a properly-escaped double-quoted string.  The following
   * transformations are performed.
   * {{{
   * double quotation mark   -> \"
   * newline                 -> \n
   * tab                     -> \t
   * carriage return         -> \r
   * backslash               -> \\
   * }}}
   * The resulting string is enclosed in double quotation marks.
   * 
   * @param str The string.
   * @return  The string with special character escaped.
   */
  def toQuotedString(str: String) = {
    var buf = new scala.collection.mutable.StringBuilder
    buf ++= "\""
    for (ch <- str) {
      ch match {
        case '"' => buf ++= "\\\""
        case '\n' => buf ++= """\n"""
        case '\t' => buf ++= """\t"""
        case '\r' => buf ++= """\r"""
        case '\\' => buf ++= """\\"""
        case _ => buf ++= ch.toString
      }
    }
    buf ++= "\""
    buf.toString
  }
  
  /**
   * Provide a convenient method to compute a hash code from many different
   * objects.  This is intended to be suitable for a few basic cases.
   * 
   * If you want to create a hash code for two objects alice and bob, try this.
   * {{{
   * 0 hashify alice hashify bob
   * }}}
   * 
   * If you want to create a hash code for a sequence of objects in stuff, try
   * this.
   * {{{
   * stuff.foldLeft(0)(_ hashify _)
   * }}}
   * 
   * If you want to combine your hash code with your children's hash codes,
   * try this.
   * {{{
   * children.hashify(hashCode)(_ hashfiy _)
   * }}}
   * 
   * @param hash    The initial hash code.
   * @param obj     The next object whose hash should be added.
   */
  def hashify(hash: Int = 0, obj: Any) = {
    hash * 31 + obj.hashCode
  }

  /**
   * Compute an alternate hash code from many different objects. An
   * alternate hash code is used in some cases to provide 2 different
   * hash codes for an Elision object. These 2 hash codes are used to
   * lower the chances of a hash collision (both different hash codes
   * will need to collide for a hash collision to occur).
   * 
   * If the object provides an "other" hash code, it must have a field or
   * a field-like method `otherHashCode` that returns a `BigInt`.  This is
   * used to compute the hash code returned by this method.
   *
   * @param hash    The initial hash code.
   * @param obj     The next object whose hash should be added.
   */
  def other_hashify(hash: BigInt = 0, obj: Any): BigInt = {
    obj match {
      case ohc: HasOtherHash => hash + 8191*ohc.otherHashCode
      case _ => hash + 8191*obj.hashCode
    }
  }
}
