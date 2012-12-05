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

import scala.collection.immutable.HashMap
import scala.collection.mutable.HashSet
import org.parboiled.errors.ParsingException
import ornl.elision.parse.AtomParser

/**
 * The core classes and definitions that make up the Elision runtime.
 * 
 * == Design Goals ==
 *  - Every atom that can be programmatically created can be written using
 *    the toParseString, and the result can be parsed by AtomParser.
 *  - Every atom that can be programmatically created has a toString method
 *    that generates valid Scala code to reproduce the atom.
 *  - Avoid global data and singletons.
 *  - Simple API.
 */
package object core {
  import scala.collection.immutable.Map
  
  /** Provide a fancy name for the type universe. */
  val `^TYPE` = TypeUniverse
  
  /**
   * Maintain an executor to use.  The provided default executor always
   * throws an exception when invoked from a native handler, so you must
   * replace this with something rational as soon as reasonable.
   */
  implicit var knownExecutor: Executor = new Executor {
    val console = PrintConsole 
    val context = new Context()
    def parse(text: String): ParseResult = ParseFailure(
        "This default executor cannot parse text; override this with a full" +
        "executor implementation to properly support parsing from within" +
        "native operators.")
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
   * @param hash		The initial hash code.
   * @param obj			The next object whose hash should be added.
   */
  def hashify(hash: Int = 0, obj: Any) = hash * 31 + obj.hashCode

  /**
   * Compute an alternate hash code from many different objects. An
   * alternate hash code is used in some cases to provide 2 different
   * hash codes for an Elision object. These 2 hash codes are used to
   * lower the chances of a hash collision (both different hash codes
   * will need to collide for a hash collision to occur).
   *
   * @param hash		The initial hash code.
   * @param obj			The next object whose hash should be added.
   */
  def other_hashify(hash: BigInt = 0, obj: Any): BigInt = {
    obj match {
      case atom: BasicAtom => hash + 8191*atom.otherHashCode
      case _ => hash + 8191*obj.hashCode
    }
  }
  
  /**
   * Attempt to parse the given string and return an atom.
   * 
   * @param str			The string to parse.
   * @param context	The context.
   * @param trace		Whether to trace the parse.
   * @return	The result of parsing, which may be None.
   */
  def parse(str: String, context: Context, trace: Boolean = false, toggle: Boolean = false) = {
    val ap = new AtomParser(context, trace, toggle)
    try {
      ap.parseAtoms(str) match {
        case AtomParser.Failure(_) => None
        case AtomParser.Success(atoms) => Some(atoms)
      }
    } catch {
      case th: ParsingException => println(th.getMessage)
    }
  }

  /**
   * Turn a string into a properly-escaped symbol.  Symbols must start with a
   * letter or underscore and consist of only letters, numbers, and the
   * underscore.  If this is not the case, the symbol text must be enclosed in
   * backticks, and any contained backticks and quotation marks must be escaped.
   * {{{
   * Fred   Johnny12   Cat_Dog   _Dog
   * ``     ` Jason`   `65$`
   * }}}
   * 
   * @param str	The symbol text.
   * @return	The symbol with special characters escaped.
   */
  def toESymbol(str: String): String = {
    // The empty string is a special case.
    if (str.isEmpty) return "``"
    // True and false are also special cases.
    if (str == "true" || str == "false") return "`" + str + "`"
    // Build up the return value.
    var buf = new scala.collection.mutable.StringBuilder
    // Look for badness.  The first character is special.
    var bad = false
    val sh = str.head
    if (sh != '_' && !sh.isLetter) {
      bad = true
      if (sh == '`') buf ++= "\\"
    }
    buf ++= sh.toString
    str.tail.map {
      ch =>
        if (ch != '_' && !ch.isLetterOrDigit) {
          bad = true
          ch match {
            case '`' => buf ++= """\`"""
		        case '\n' => buf ++= """\n"""
		        case '\t' => buf ++= """\t"""
		        case '\r' => buf ++= """\r"""
		        case '\\' => buf ++= """\\"""
		        case _ => buf ++= ch.toString
          }
        } else buf ++= ch.toString
    }
    if (bad) "`" + buf.toString + "`" else buf.toString
  }

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
   * @param str	The string.
   * @return	The string with special character escaped.
   */
  def toEString(str: String) = {
    var buf = new scala.collection.mutable.StringBuilder
    buf ++= "\""
    for (ch <- str) {
      ch match {
        case '"' => buf ++= """\""""
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
   * Issue a warning.  This should be wired to whatever error reporting
   * mechanism you want to use.
   * 
   * @param text	The text of the warning.
   */
  def warn(text: String) {
    println("WARNING: " + text)
  }
  
  //======================================================================
  // WARNING: Implicit modification of containers!
  //======================================================================
    
  /**
   * Magically add a `mkParseString`, roughly equivalent to `mkString`, to
   * every sequence of objects that extend [[ornl.elision.core.BasicAtom]].
   * Try that with Java!
   * 
   * @param seq		The sequence to get the new method.
   */
  implicit def giveMkParseString[A <: BasicAtom](seq: Seq[A]): {
    def mkParseString(pre: String, mid: String, post: String): String
  } = new {
    /**
     * Make a string from a sequence.
     * 
     * @param pre		The prefix text, if any.
     * @param mid		The text to insert between each item.
     * @param post	The suffix text, if any.
     * @return	The new string.
     */
    def mkParseString(pre: String, mid: String, post: String) =
      seq.map(_.toParseString).mkString(pre, mid, post)
  }
  
  /**
   * Invoking the `omit` method on an indexed sequence automagically transforms
   * it into an omit sequence with the original sequence as its backing store.
   * 
   * @param seq	The original sequence.
   */
  implicit def toOmitSeq[A](seq: IndexedSeq[A]): {
    def omit(index: Int): OmitSeq[A]
  } = new {
    /**
	   * Omit a single element from this list, returning a new list.  This is
	   * done "in place" so it should be fast.  As omits mount, lookup time
	   * can suffer, approaching linear time.
	   * 
	   * @param index	The zero-based index to omit.
	   * @return	The new list.
	   */
    def omit(index: Int) = {
      new OmitSeq2[A](seq, index)
    }
  }
  
  /**
   * The omit sequence class.
   */
  abstract class OmitSeq[A] extends IndexedSeq[A] {

    val otherHashCode: BigInt

    /**
     * Omit a single element from this list, returning a new list.  This is
     * done "in place" so it should be fast.  As omits mount, lookup time
     * can suffer, approaching linear time.
     * 
     * @param index The zero-based index to omit.
     * @return  The new list.
     */
    def omit(index: Int): IndexedSeq[A] = {
      var r = new OmitSeq2(this, index)
      r.isDistinct = this.isDistinct
      r
    }
    
    /**
     * Insert a sequence into the list starting at the given (zero-based) index.
     * This is done "in place" so it should be fast.  As inserts mount, lookup
     * time can suffer, as with omits.
     * 
     * @param index The zero-based index of the first inserted element.
     * @param items The items to insert.  If this is empty, nothing is done.
     * @return  The new list.
     */
    def insert(index: Int, items: IndexedSeq[A]): IndexedSeq[A] = {
      if (items.size == 0) this else new OmitSeq3(this, index, items)
    }

    var isDistinct = false

    override def distinct: IndexedSeq[A] = {
      
      // Is this sequence already distinct?
      if (this.isDistinct) this

      // Is this sequence empty? If so we are already distinct.
      else if (this.size == 0) this

      // Are we handling a sequence of BasicAtoms?
      else if (this(0).isInstanceOf[BasicAtom]) {

        // Use the BasicAtom hash code pair to test for uniqueness.
        var tmp = scala.collection.immutable.Vector.empty[BasicAtom]
        var index = 0
        var seenElems = new HashSet[(Int,BigInt)]
        while (index < this.size) {
          val elem = this(index).asInstanceOf[BasicAtom]
          if (!seenElems.contains((elem.hashCode, elem.otherHashCode))) {
            tmp = tmp :+ elem
            seenElems += new Tuple2(elem.hashCode, elem.otherHashCode)
          }
          index += 1
        }
        
        // The result is known to be distinct.
        var r = new OmitSeq1(tmp)
        r.isDistinct = true
        r.asInstanceOf[IndexedSeq[A]]
      }
      
      // We are not handling basic atoms. Use the default
      // implementation.
      else {
        
        // Compute the distinct omit sequence.
        var r = new OmitSeq1(super.distinct)

        // The result is known to be distinct.
        r.isDistinct = true
        r
      }
    }
  }
  
  /**
   * Provide convenient construction of an omit sequence and automatic
   * (implicit) transformation of indexed collections to an omit sequence.
   */
  object OmitSeq {

    var debug = false
    /**
     * Convert an indexed sequence to an omit sequence.
     * 
     * @param backing	The backing sequence.
     * @return	The new omit sequence.
     */
    implicit def fromIndexedSeq[A](backing: IndexedSeq[A]): OmitSeq[A] =
      new OmitSeq1[A](backing)
      
    /**
     * Make a new omit sequence that is initially empty.
     * 
     * @return An empty sequence.
     */
    def apply[A](): OmitSeq[A] = new OmitSeq1(Seq[A]().toIndexedSeq)
    
    /**
     * Make a new omit sequence from the given items.
     * 
     * @param items	The items of the new sequence.
     * @return	The new sequence.
     */
    def apply[A](items: A*): OmitSeq[A] = new OmitSeq1[A](items.toIndexedSeq)
  }
  
  /**
   * Construct an omit sequence that simply wraps a backing sequence but does
   * not actually omit or insert anything.
   * 
   * @param backing	The backing sequence.
   */
  private class OmitSeq1[A](backing: IndexedSeq[A]) extends OmitSeq[A] {

    // Proxy to backing sequence.
    override val length = backing.length

    /**
     * Hash code for an OmitSeq. It looks like the default hashCode
     * computation for an IndexedSeq uses toString (which is bad), so
     * hashCode has been overwritten.
     */
    override lazy val hashCode = backing.hashCode
    lazy val otherHashCode = backing.foldLeft(BigInt(0))(other_hashify)+1
    
    // Proxy to backing sequence.
    def apply(index: Int) = {
      val r = backing(index)
      if (OmitSeq.debug) println("OmitSeq1[" + index + "] = " + 
                                 r + ", len = " + this.length)
      r
    }
  }
  
  /**
   * Construct an omit sequence that omits a single element from the backing
   * sequence.
   * 
   * @param backing	The backing sequence.
   * @param omit		The (zero-based) index of the item to omit.
   */
  private class OmitSeq2[A](backing: IndexedSeq[A], omit: Int)
  extends OmitSeq[A] {

    override lazy val hashCode = this.foldLeft(0)(hashify)+1
    lazy val otherHashCode = this.foldLeft(BigInt(0))(other_hashify)+1

    /** Length is one less than the backing sequence. */
		override val length = backing.length - 1
		
		/** Return the requested element by zero-based index. */
		override def apply(index: Int) = {
		  val r = if (index >= omit) backing(index+1) else backing(index)
      if (OmitSeq.debug) println("OmitSeq2[" + index + "] = " + 
                                 r + ", len = " + this.length)
      r
    }
	}
  
  /**
   * Construct an omit sequence that inserts another sequence of elements at
   * a given index.
   * 
   * @param backing The backing sequence.
   * @param insert  The (zero-based) index of the items to insert.
   * @param items   The sequence of items to be inserted.
   */
  private class OmitSeq3[A](backing: IndexedSeq[A], insert: Int,
      items: IndexedSeq[A]) extends OmitSeq[A] {

    // Some stored constants for fast reference.
    private val _il = items.length
    private val _tip = _il + insert
    
    /** Length is the backing sequence plus the inserted items. */
    override val length = backing.length + _il

    override lazy val hashCode = this.foldLeft(0)(hashify)+1
    lazy val otherHashCode = this.foldLeft(BigInt(0))(other_hashify)+1
    
    /** Return the requested element by zero-based index. */
    override def apply(index: Int) = {
      val r = if (index < insert) backing(index)
              else {
                if (index < _tip) items(index - insert)
                else backing(index - _il)
              }
      if (OmitSeq.debug) {
        println("OmitSeq3 part1 = " + backing)
        println("OmitSeq3 part2 = " + items)
        println("OmitSeq3[" + index + "] = " + 
                r + ", len = " + this.length)
      }
      r
    }
  }

  //======================================================================
  // Strange debugging methods.
  //======================================================================
  
  def D(atom: BasicAtom) = {
    println(atom.toParseString)
    atom
  }
  
  def D[A <: Seq[BasicAtom]](seq: A) = {
    println(seq.mkParseString("{",",","}"))
    seq
  }
  
  //======================================================================
  // WARNING: Here be IMPLICITS!
  //======================================================================
  
  /** Convert a Scala symbol to a variable. */
  implicit def symToVariable(symbol: Symbol) = Variable(ANY, symbol.name)
  
  /** Convert a variable to a Scala symbol. */
  implicit def variableToSym(variable: Variable) = Symbol(variable.name)
  
  /** Convert an integer to an integer literal. */
  implicit def intToLiteral(value: Int) = Literal(value)

  /** Convert an integer to an integer literal. */
  implicit def intToLiteral(value: BigInt) = Literal(value)
  
  /** Convert an integer literal to an integer. */
  implicit def literalToInt(value: IntegerLiteral) = value.value
  
  /** Convert a string to a string literal. */
  implicit def strToLiteral(string: String) = Literal(string)
  
  /** Convert a string literal to a string. */
  implicit def literalToStr(value: StringLiteral) = value.value
  
  /** Convert a Scala Boolean to a Boolean literal. */
  implicit def boolToLiteral(bool: Boolean) = Literal(bool)
  
  /** Convert a Boolean literal to a Scala Boolean. */
  implicit def literalToBool(value: BooleanLiteral) = value.value
  
  /**
   * Where needed, convert a bindings object into a bindings atom (wrap).
   * 
   * @param binds	The bindings.
   * @return	The new bindings atom.
   */
  implicit def wrapBindingsAtom(binds: Bindings) = new BindingsAtom(binds)
  
  /**
   * Where needed, convert a bindings atom to a simple bindings object (unwrap).
   * 
   * @param atom	The bindings atom.
   * @return	The bindings.
   */
  implicit def unwrapBindingsAtom(atom: BindingsAtom) = atom.mybinds
}
