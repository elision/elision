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
import sjp.elision.ElisionException

/**
 * Construction of a special form failed for the specified reason.
 * 
 * @param msg	A human-readable message.
 */
class SpecialFormException(msg: String) extends ElisionException(msg)

class SpecialFormHolder(val tag: BasicAtom, val content: BasicAtom) {
  def requireBindings = content match {
    case ba:BindingsAtom => new BindingsHolder(tag, ba)
    case _ =>
      throw new SpecialFormException(
          "Form " + tag.toParseString +
          " expected bindings as content, but instead found: " +
          content.toParseString + ".")
  }
  
  def toSpecialForm() = new SpecialForm(tag, content)
}

class BindingsHolder(val tag: BasicAtom, val content: BindingsAtom) {
  def require(keys: String*) {
  	keys foreach { key =>
    	if (!content.contains(key))
    		throw new SpecialFormException(
    				"Form " + tag.toParseString +
    				" requires key " + toESymbol(key) + " but it was not given.")
  	}
  }
  def allow(keys: String*) {
    val badkeys = content.keySet -- keys
    if (!badkeys.isEmpty) {
      throw new SpecialFormException(
          "Form " + tag.toParseString +
          " does not allow " + badkeys.mkString("{", ",", "}") + ".")
    }
  }
  def check(test: Map[String,Boolean]) {
    allow(test.keySet.toSeq:_*)
    test foreach {
      pair => pair match {
        case (key, true) => require(key)
        case _ =>
      }
    }
  }
  def fetchAs[TYPE](key: String, default: Option[TYPE] = None)
  (implicit m: scala.reflect.Manifest[TYPE]): TYPE = {
    content.get(key) match {
      case None => default match {
        case Some(value) => value
        case None =>
	        throw new SpecialFormException(
	            "Form " + tag.toParseString +
	            " requires key " + toESymbol(key) + " but it was not given.")
      }
      case Some(item) =>
        if (!key.isInstanceOf[TYPE])
          throw new SpecialFormException(
              "The value for key " + toESymbol(key) + " of form " +
              tag.toParseString + " is of the wrong type: " + item.toParseString)
        else
          item.asInstanceOf[TYPE]
    }
  }
  
  def toSpecialForm() = new SpecialForm(tag, content)
}

/**
 * This is the generalized "special form" of atom.
 * 
 * @param tag			The tag identifying this particular form.
 * @param content	The content of the atom.
 */
class SpecialForm(val tag: BasicAtom, val content: BasicAtom)
extends BasicAtom {

  lazy val depth = (tag.depth max content.depth) + 1
  lazy val deBruijnIndex = tag.deBruijnIndex max content.deBruijnIndex
  lazy val isConstant = tag.isConstant && content.isConstant
  val theType: BasicAtom = TypeUniverse
  lazy val isTerm = tag.isTerm && content.isTerm
  lazy val constantPool = Some(BasicAtom.buildConstantPool(17, tag, content))
  override lazy val hashCode = tag.hashCode * 31 + content.hashCode
  
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

/**
 * Construction and matching of special forms.
 * 
 * Known special forms register their handlers here.  The apply method
 * handles dispatch to the appropriate implementation.m
 */
object SpecialForm {
  def apply(tag: BasicAtom, content: BasicAtom) = {
    val sfh = new SpecialFormHolder(tag, content)
    tag match {
	    case sl:SymbolLiteral => sl.value match {
	      case 'bind => BindingsAtom(sfh)
	      case 'rule => RewriteRule(sfh)
	      case 'prop => AlgProp(sfh)
	      case 'match => MatchAtom(sfh)
	      case _ => sfh.toSpecialForm
	    }
	    case _ => sfh.toSpecialForm
	  }
  }
}
