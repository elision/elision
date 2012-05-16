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
package ornl.elision.core

/**
 * Encapsulate a simple atom to perform basic matching.
 * 
 * == Purpose ==
 * This atom holds a single pattern ''P''.  When applied to another atom ''A'',
 * it tries to match ''A'' against pattern ''P''.  If the match fails, then
 * `NONE` is returned.  The first match that satisfies the guards (if any) is
 * returned.
 * 
 * == Rules ==
 * This is almost a rewrite rule.  Consider the following rule.
 * {{{
 * { rule $x:INTEGER -> 0 #if equal($$x,5) }
 * }}}
 * We can ''almost'' write this as follows, provided we are very careful with
 * metavariables.
 * {{{
 * \$$x.(({match $$x:INTEGER #if equal($$x,5)}.$$x).0)
 * }}}
 * The reason the above two are not the same is that in the second case
 * we might get `NONE` as the result from the match and this will not
 * get "digested" by the applicative dot.
 */
class MatchAtom(sfh: SpecialFormHolder,
    val pattern: BasicAtom, val guards: AtomSeq)
extends SpecialForm(sfh.tag, sfh.content) with Applicable {
  /** The type of this atom. */
  override val theType = SymbolicOperator.MAP(ANY, BINDING)
  
  /**
   * Perform the matching.
   * 
   * @param subject		The subject to match.
   * @param binds			Bindings to honor.
   * @return	Some bindings if the match was successful, and `None` if not.
   */
  private def _doMatching(subject: BasicAtom,
      binds: Bindings = Bindings()): Option[Bindings] = {
    // Local function to check the guards.
    def checkGuards(candidate: Bindings): Boolean = {
      for (guard <- guards) {
        val (newguard, _) = guard.rewrite(candidate)
        if (!newguard.isTrue) return false
      }
      true
    }
    
    // Try to match the given atom against the pattern.
    pattern.tryMatch(subject, binds) match {
      case fail:Fail => return None
      case Match(newbinds) =>
        // We got a match.  Check the guards.
        if (checkGuards(newbinds)) Some(newbinds) else return None
      case Many(iter) =>
        // We might have many matches.  We search through them until we find
        // one that satisfies the guards, or until we run out of candidates.
        for (newbinds <- iter) {
          if (checkGuards(newbinds)) return Some(newbinds)
        }
        return None
    }
  }

  /**
   * Apply this match atom to the given atom.  This performs the match
   * and returns either the bindings of the first match, or `NONE` if
   * it does not match.
   * 
   * @param subject	The subject to match.
   * @param bypass	Whether to bypass native handlers.
   * @return	Bindings, or Nothing.
   */
  def doApply(subject: BasicAtom, bypass: Boolean) = _doMatching(subject) match {
    case None => NONE
    case Some(binds) => BindingsAtom(binds)
  }
}

/**
 * Provide construction and matching.
 */
object MatchAtom {
  /** The special form tag. */
  val tag = Literal('match)
  
  /**
   * Make a new pair from the provided special form data.
   * 
   * @param	sfh		Parsed special form data.
   */
  def apply(sfh: SpecialFormHolder): MatchAtom = {
    val bh = sfh.requireBindings
    bh.check(Map(""->true, "if"->false))
    val guards = bh.fetchAs[AtomSeq]("if", Some(EmptySeq))
    bh.fetchAs[AtomSeq]("") match {
      case Args(atom: BasicAtom) =>
        new MatchAtom(sfh, atom, guards)
      case x =>
        throw new SpecialFormException(
            "Did not find exactly one pattern: " + x.toParseString)
    }
  }
  
  /**
   * Make a new match atom from the provided parts.
   * 
   * @param pattern		The pattern to match.
   * @param guards		The guards, if any.
   */
  def apply(pattern: BasicAtom, guards: BasicAtom*) = {
    val guardseq = AtomSeq(NoProps, guards.toIndexedSeq)
    val binds = Bindings() + (""->AtomSeq(NoProps, pattern)) + ("if"->guardseq)
    val sfh = new SpecialFormHolder(tag, binds)
    new MatchAtom(sfh, pattern, guardseq)
  }
  
  /**
   * Extract the pattern atom and any guards.
   * 
   * @param ma	The match atom.
   * @return	The pattern and then guards.
   */
  def unapply(ma: MatchAtom) = Some((ma.pattern, ma.guards))
}
