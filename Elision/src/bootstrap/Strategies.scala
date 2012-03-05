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
package bootstrap

/**
 * Bootstrap the strategies.
 */
object Strategies {

  val defs = """
  // Declare the rulesets.
  { rulesets IF, STRAT }
    
  // Declare the if operator and the rules to make it work.
  { operator if($test: BOOLEAN, $thenpart: $T @lazy, $elsepart: $T @lazy): $T }
  { rule if(true, $thenpart, $elsepart) -> $thenpart ruleset IF level 2 }
  { rule if(false, $thenpart, $elsepart) -> $elsepart ruleset IF level 2 }
    
  // The no-op strategy leaves atoms unmodified.  The flag is always set to
  // true.
  { operator s_noop(): STRATEGY }
  { rule (s_noop().$a) -> { bind atom->$a, flag->true } ruleset STRAT level 1 }
    
  // Declare operators to modify the flag returned from a strategy.
  { operator s_true($s: STRATEGY): STRATEGY }
  { rule (s_true($s).$a) -> ($s.$a).{bind atom->$atom, flag->true} }
  { operator s_false($s: STRATEGY): STRATEGY }
  { rule (s_false($s).$a) -> ($s.$a).{bind atom->$atom, flag->false} }
    
  // Declare the if strategy.  This executes the first strategy, and checks
  // the flag.  If it is true, the second strategy is executed.  Otherwise the
  // third strategy is executed.  The flag is the result of the last strategy
  // executed.
  { operator s_if($test: STRATEGY, $thenpart: STRATEGY @lazy,
  	$elsepart: STRATEGY @lazy): STRATEGY }
  { rule (s_if($test, $thenpart, $elsepart).$a) ->
    ($test.$a).if($flag, ($thenpart.$atom), ($elsepart.$atom))
    ruleset STRAT level 1 }
    
  // The while strategy executes the subordinate strategy for as long as it
  // yields the true flag.
  { operator s_while($test: STRATEGY): STRATEGY }
  { rule (s_while($test).$a) ->
    (s_if($test, s_while($test), s_noop()).$a) ruleset STRAT level 1 }
    
  { operator s_then($part: STRATEGY): STRATEGY is associative }
  { rule (s_then($p, $q).$a) ->
    ($q.(($p.$a).$atom)) ruleset STRAT level 1 }
    
  { operator s_and($part: STRATEGY): STRATEGY is associative }
  { rule (s_and($p, $q).$a) ->
    (s_if($p, $q, s_noop()).$a) ruleset STRAT level 1 }
    
  { operator s_or($part: STRATEGY): STRATEGY is associative }
  { rule (s_or($p, $q).$a) ->
    (s_if($p, s_noop(), $q).$a) ruleset STRAT level 1 }
    
  { operator s_td($part: STRATEGY): STRATEGY}
  { rule s_td($s).$a -> s_then($s,{map s_td($s)}).$a ruleset STRAT level 1}
  """
}
