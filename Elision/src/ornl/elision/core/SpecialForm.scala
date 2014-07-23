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
 * ======================================================================
 * */
package ornl.elision.core

import ornl.elision.core.BasicAtomComparator._
import ornl.elision.util.ElisionException
import ornl.elision.util.Loc

/**
 * Construction of a special form failed for the specified reason.
 * 
 * @param loc Location of the bad special form.
 * @param msg	A human-readable message.
 */
class SpecialFormException(loc: Loc, msg: String)
extends ElisionException(loc, msg)

/**
 * Hold the data from parsing (or constructing) a special form.  This
 * really just holds the two parts: the ''tag'' and the ''content'',
 * and provides a few methods for working with the content.
 * 
 * These should only be built by the parser.
 * 
 * @param loc     The location of this special form's definition.
 * @param tag			The tag.
 * @param content	The content.
 */
class SpecialFormHolder(val loc: Loc, val tag: BasicAtom, val content: BasicAtom) {
  /**
   * Require that the content be a binding.  If the content is not, then
   * an exception is thrown (`SpecialFormException`).
   * 
   * @return	A `BindingsHolder` instance.
   */
  def requireBindings = content match {
    case ba:BindingsAtom => new BindingsHolder(loc, tag, ba)
    case _ =>
      throw new SpecialFormException(loc,
          "Form " + tag.toParseString +
          " expected bindings as content, but instead found: " +
          content.toParseString + ".")
  }
  
  /**
   * Extract a special form instance from this holder.  This directly creates
   * a special form; it does not interpret it based on the tag.  If the latter
   * is what you want, use the `interpret` method.
   * 
   * @return	The special form instance.
   */
  def toSpecialForm() = new SpecialForm(loc, tag, content)
  
  /**
   * Interpret this based on the tag, and return the resulting special form.
   * 
   * @return	The correct atom based on the tag.
   */
  def interpret = SpecialForm(loc, tag, content)
}

/**
 * Hold data from a special form where the content is a bindings atom.
 * 
 * @param loc       The location where this special form arose.
 * @param tag				The tag.
 * @param content		The content (a bindings atom).
 */
class BindingsHolder(loc: Loc, val tag: BasicAtom, val content: BindingsAtom) {
  /**
   * Require that the bindings atom specify all the listed keys.  If any
   * are missing, a `SpecialFormException` is thrown.  Other keys not in
   * the list are ignored.
   * 
   * @param keys		The keys to test.
   */
  def require(keys: String*) {
  	keys foreach { key =>
    	if (!content.contains(key))
    		throw new SpecialFormException(loc,
    				"Form " + tag.toParseString +
    				" requires key " + toESymbol(key) + " but it was not given.")
  	}
  }
  
  /**
   * Require that the bindings atom specify only the listed keys.  If any
   * other keys are found, a `SpecialFormException` is thrown.  Keys in the
   * list may or may not be present.
   * 
   * @param keys		The keys to test.
   */
  def allow(keys: String*) {
    val badkeys = content.keySet -- keys
    if (!badkeys.isEmpty) {
      throw new SpecialFormException(loc,
          "Form " + tag.toParseString +
          " does not allow key(s) " +
          badkeys.map(toESymbol(_)).mkString("", ", ", "") + ".")
    }
  }
  
  /**
   * Require that the bindings atom specify exactly one of the two provided
   * keys.  If both or neither is present, then a `SpecialFormException` is
   * thrown.
   * 
   * @param key1			A key.
   * @param key2			A key.
   * @return	The key that is present.
   */
  def either(key1: String, key2: String): String = {
    val has1 = content.contains(key1)
    val has2 = content.contains(key2)
    if (has1 && has2) {
      throw new SpecialFormException(loc,
          "Form " + tag.toParseString +
          " requires either key " + toESymbol(key1) +
          " or " + toESymbol(key2) +
          " (not both), but both were given.")
    }
    if (!has1 && !has2) {
      throw new SpecialFormException(loc,
          "Form " + tag.toParseString +
          " requires either key " + toESymbol(key1) +
          " or " + toESymbol(key2) +
          ", but neither was given.")
    }
    if (has1) key1 else key2
  }
  
  /**
   * Check the bindings.  The provided map indicates which keys are required,
   * and which are optional.  Only the keys in the map are permitted.
   * 
   * If a key is mapped to `true`, then it is required to be present.  If a
   * key is mapped to `false`, then it is optional.
   * 
   * Errors result in a `SpecialFormException`.
   * 
   * @param test			The key mapping.
   */
  def check(test: Map[String,Boolean]) {
    allow(test.keySet.toSeq:_*)
    test foreach {
      pair => pair match {
        case (key, true) => require(key)
        case _ =>
      }
    }
  }
  
  /**
   * Fetch the value for the specified key from the bindings.  If no default
   * value is specified (or is `None`), then the key is ''required'' to be
   * present.  If a default is specified, and the key is not present, then
   * the default is returned.  On error a `SpecialFormException` is thrown.
   * 
   * The value for the key is cast to the specified `TYPE`.  If the value is
   * not of the specified `TYPE`, then a `SpecialFormException` is thrown.
   * 
   * @param TYPE				Type of the atom to return.
   * @param key					The key.
   * @param default			Optional default value if the key is not present.
   * @return	The value of the key, cast to the correct type.
   */
  def fetchAs[TYPE](key: String, default: Option[TYPE] = None)
  (implicit mTYPE: scala.reflect.Manifest[TYPE]): TYPE = {
    content.get(key) match {
      case None => default match {
        case Some(value) => value
        case None =>
	        throw new SpecialFormException(loc,
	            "Form " + tag.toParseString +
	            " requires key " + toESymbol(key) + " but it was not given.")
      }
      case Some(item) =>
        if (mTYPE >:> Manifest.classType(key.getClass))
          throw new SpecialFormException(loc,
              "The value for key " + toESymbol(key) + " of form " +
              tag.toParseString + " is of the wrong type: " +
              item.toParseString + ". Expected " + mTYPE.toString +
              " but got " + Manifest.classType(key.getClass) + ".")
        else
          item.asInstanceOf[TYPE]
    }
  }
  
  /**
   * Determine whether the given key is present in this holder.
   * @param key   The key to check.
   * @return  True iff the key is present.
   */
  def has(key: String) = content.contains(key)
  
  /**
   * Extract a special form instance from this holder.  This directly creates
   * a special form; it does not interpret it based on the tag.  If the latter
   * is what you want, use the `interpret` method.
   * 
   * @return	The special form instance.
   */
  def toSpecialForm() = new SpecialForm(loc, tag, content)
  
  /**
   * Interpret this based on the tag, and return the resulting special form.
   * 
   * @return	The correct atom based on the tag.
   */
  def interpret = SpecialForm(loc, tag, content)
}

/**
 * This is the generalized "special form" of atom.
 * 
 * == Purpose ==
 * Special forms unify a lot of different syntax in the library, and this
 * supports more unified matching and rewriting.
 * 
 * A special form is a pair of atoms.
 * 
 * @param loc     The location where this special form arose.
 * @param tag			The tag identifying this particular form.
 * @param content	The content of the atom.
 */
class SpecialForm(loc: Loc, val tag: BasicAtom, val content: BasicAtom)
extends BasicAtom(loc) {

  lazy val depth = (tag.depth max content.depth) + 1
  lazy val deBruijnIndex = tag.deBruijnIndex max content.deBruijnIndex
  lazy val isConstant = tag.isConstant && content.isConstant
  /** All special forms use the type ANY as their type. */
  val theType: BasicAtom = ANY
  lazy val isTerm = tag.isTerm && content.isTerm

  override lazy val hashCode = tag.hashCode * 12289 + content.hashCode
  override lazy val otherHashCode = tag.otherHashCode + 8191*content.otherHashCode
  
  override def equals(other: Any) = other match {
    case sf:SpecialForm =>
      feq(sf, this, tag == sf.tag && content == sf.content)
      
    case _ =>
      false
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
    if (newtag._2 || newcontent._2) {
      (SpecialForm(loc, newtag._1, newcontent._1), true)
    } else {
      (this, false)
    }
  }
  
  def replace(map: Map[BasicAtom, BasicAtom]) = {
    map.get(this) match {
      case Some(atom) =>
        (atom, true)
      case None =>
        val (newtag, flag1) = tag.replace(map)
        val (newcontent, flag2) = content.replace(map)
        if (flag1 || flag2) {
          (SpecialForm(loc, newtag, newcontent), true)
        } else {
          (this, false)
        }
    }
  }
}

/**
 * Construction and matching of special forms.
 * 
 * The apply method handles dispatch to the appropriate implementation of each
 * known special type.
 */
object SpecialForm {
  /**
   * Make the appropriate object from the special form data.
   * 
   * @param loc       The location where this special form arose.
   * @param tag				The form tag.
   * @param content		The content.
   * @return	The constructed special form.
   */
  def apply(loc: Loc, tag: BasicAtom, content: BasicAtom): BasicAtom = {
    val sfh = new SpecialFormHolder(loc, tag, content)
    tag match {
	    case sl:SymbolLiteral => sl.value match {
	      case 'map => MapStrategy(sfh)
	      case 'binds => BindingsAtom(sfh)
	      case 'rule => RewriteRule(sfh)
	      case 'match => MatchAtom(sfh)
	      case 'operator => Operator(sfh)
	      //case RulesetStrategy.tag => RulesetStrategy(sfh, context)
	      case _ => sfh.toSpecialForm
	    }
	    case _ => sfh.toSpecialForm
	  }
  }
  
  /**
   * Extract the parts of the special form.
   * 
   * @param sf    The special form.
   * @return  The tag and content.
   */
  def unapply(sf: SpecialForm) = {
    Some((sf.tag, sf.content))
  }
}
