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
 * @author ysp
 *
 */
object BuiltinOperators {

  /**
   * Text to be executed by the system.  We store this in a `CDATA` block not
   * just for fun, but so that the content is preserved.  A verbatim block
   * would not work, since Elision uses the same verbatim block format as
   * Scala.  So... add text to execute inside the `CDATA` element.
   */
  val text =
<execute>
<![CDATA[
def({ operator #name=typeof #cases %($x:$T)->$T
      #description="Extract and show the type of the argument."
      #detail=
"""Given a single argument, extract the type $T of that argument $x
and return the extracted type."""
})

def({ operator #name=getop #params=%($x:OPREF)
      #description="Given an operator reference, return the operator."
      #detail=
"""Given the operator reference $x, resolve the reference to the
actual operator and return the operator."""
})

def({ operator #name=bind #params=%($v,$a)
      #description="Bind a variable to an atom."
      #detail=
"""Bind the variable $v to the atom $a in the current context.  Variable
bindings are applied after an atom is parsed and constructed, but before
it is displayed by the REPL.  Variables can be arbitrarily re-bound."""
})

def({ operator #name=unbind #params=%($v)
      #description="Unbind a variable."
      #detail=
"""Forget any binding for variable $v in the current context."""
})

def({ operator #name=showbinds #params=%()
      #description="Show the bindings in the current context."
      #detail=
"""Show all the bindings present in the current context, except for
bindings to variables whose name starts with an underscore, as these
are considered "private." """
})

def({ operator #name=equal #params=%C($x,$y) #type=BOOLEAN
      #description="Report whether two atoms are equal."
      #detail=
"""Test whether the two atoms $x and $y are considered equal by the
system."""
})

def({ operator #name=timing #params=%($f:BOOLEAN)
      #description="Toggle showing the evaluation time."
      #detail=
"""Toggle showing the total elapsed "wall clock" time to evaluate an
atom.  The result shows minutes, seconds, and milliseconds."""
})

def({ operator #name=context #params=%()
      #description="Show the content of the current context."
      #detail=
"""Show the bindings, operators, and rules currently in the current
context."""
})

def({ operator #name=stacktrace #params=%()
      #description="Toggle whether or not a stack trace is shown."
      #detail=
"""For non-Elision exceptions the exception message is shown, but the
stack trace is typically suppressed (this is not true for runtime
erorrs; the stack trace is always shown for these).  This will enable
(or disable) showing the stack trace for all non-Elision exceptions."""
})

def({ operator #name=eval #params=%($atom)
      #description="Force fast rewrite using the current bindings."
      #detail=
"""The context bindings are typically applied after an atom is parsed
and constructed.  This forces any bound variables in $atom to be 
rewritten immediately.  It is most useful when $atom is the argument
to some other operator."""
})

def({ operator #name=read #params=%($filename: STRING)
      #description="Read the content of the specified file."
      #detail=
"""Read the file specified by $filename, executing each line of the
file as though it were typed at the REPL prompt while quiet mode was
in effect (so most output is suppressed).  No numbered repl bindings
are generated from this action."""
})

def({ operator #name=write #params=%($filename: STRING)
      #description="Write the content of the current context to a file."
      #detail=
"""Write the current context to the file specified by $filename."""
})

def({ operator #name=_help_op #params=%($r: OPREF)
      #description="Display detailed help for an operator."
      #detail=
"""Display help text for the operator referenced by $r."""
})

def({ operator #name=_help_all #params=%()
      #description="Display a list of known operators."
      #detail=
"""Display a list of known operators, along with a short description
of each one (if given).  Operators whose name begins with an underscore
are suppressed in this list."""
})

def({ operator #name=help #cases %($r:OPREF)->_help_op($r), _help_all:OPREF
      #description="Get general help or help on an operator."
      #detail=
"""With no argument, list all operators.  If provided with an operator
reference, give detailed help on that operator."""
})

def({ operator #name=traceparse #params=%()
      #description="Toggle tracing of the parser."
      #detail=
"""Enable or disable tracingin the parser.  This is really only useful
if you are trying to debug the parser, and want to know in excruciating
detail how something parses.  It may generate a *lot* of output.

This is optional; if multiple parsers are present, some may not support
tracing."""
})

def({ operator #name=tracematch #params=%()
      #description="Toggle tracing of matching attempts."
      #detail=
"""Enable or disable tracing of match attempts.  This generates output
from the matcher as it attempts to match atoms.  The matcher is called
for a variety of reasons, so you may see some unexpected output here."""
})

def({ operator #name=showscala #params=%()
      #description="Toggle showing the Scala version of parsed atoms."
      #detail=
"""Enable or disable display of the Scala code to create each atom that
the system displays.  The assumption is that you have imported the core
package for Elision."""
})

def({ operator #name=showprior #params=%()
      #description=
"Toggle showing the atom prior to applying bindings and rewriting."
      #detail=
"""When enabled, additional output is generated that shows the parsed atom
prior to applying any bindings from the current context and prior to any
rewriting attempts."""
})

def({ operator #name=history #params=%()
      #description="Show the current history."
      #detail=
"""Show the current history of lines executed.  This history is numbered,
and numbered items in the history can be recalled later.  The history
is saved between sessions, if possible."""
})

def({ operator #name=quiet #params=%()
      #description="Toggle disable most output."
      #detail=
"""Enable or disable printing of most output.  Explicitly requested output is
always displayed (such as operator help).  Quiet mode is indicated with the
q> prompt."""
})

def({ operator #name=declare
      #params=%ACI($r1: SYMBOL, $r2: SYMBOL)
      #type=SYMBOL
      #description="Declare one or more rulesets."
      #detail=
"""Declare the named rulesets.  Rulesets must be declared before use, but they
can be re-declared without error."""
})

def({ operator #name=enable #params=%($x: SYMBOL)
      #description="Enable the specified ruleset."
      #detail=
"""Enable the specified ruleset for automatic rewriting.  Rules in the ruleset
will be automatically applied to atoms."""
})
def({ operator #name=disable #params=%($x: SYMBOL)
      #description="Disable the specified ruleset."
      #detail=
"""Disable the specified ruleset for automatic rewriting.  Rules in the ruleset
may still be used if they are also in another enabled ruleset."""
})

def({ operator #name=setlimit #params=%($limit: INTEGER)
      #description="Set the automatic rewrite limit."
      #detail=
"""Specify the maximum number of times to rewrite an atom using the automatic
rewriter.  Using zero disables the rewriter."""
})

def({ operator #name=setdebruijn #params=%($enable: BOOLEAN)
      #description="Enable or disable the use of DeBruijn indices."
      #detail=
"""Specify whether to use DeBruijn indices for lambdas.  Disabling this is
only useful when you are debugging lambdas."""
})

def({ operator #name=setdescend #params=%($enable: BOOLEAN)
      #description="Set whether to descend into children during rewriting."
      #detail=
"""Specify whether to descend into children during rewriting.  If true then 
try to rewrite children of atoms."""
})

def({! setautodefine($flag: BOOLEAN)
      #description="Set whether to automatically declare operators."
      #detail=
"""If $flag is true, then subsequent operators encountered are automatically
added to the context.  If false, this is disabled."""
})

def({ operator #name=setroundtrip #params=%($enable: BOOLEAN)
      #description="Set whether to perform round-trip testing."
      #detail=
"""Specify whether to perform round-trip testing.  When an atom is entered
and evaluated, the result is converted to a string, and the string parsed.
If the original is not equal to the result, then a "round trip parse"
failure is printed.  You will want to disable this if you are trying
to trace either the rewriter or the parser."""
})

def({ operator #name=rewrite #params=%()
      #description="Toggle whether automatic rewriting is allowed."
      #detail=
"""Toggle whether to perform automatic rewriting.  The system will perform
rewriting based on the enabled rulesets and the rewrite limit, if
automatic rewriting is enabled."""
})

def({ operator #name=showrules #params=%($atom)
      #description="Show the rules that may apply to a given atom."
      #detail=
"""The atom $atom is passed to the current context's rule library and the
rules that will be used to rewrite it are returned and printed."""
})

def({ operator #name=mod #params=%($b: INTEGER, $d: INTEGER) #type=INTEGER
      #description="Compute the remainder from division."
      #detail=
"""Compute $b mod $d, returning the remainder when $b is divided by $d."""
})

def({ operator #name=neg #params=%($x: INTEGER) #type=INTEGER
      #description="Negate an integer."
      #detail=
"""Compute the negation of the integer $x; that is, compute 0 - $x."""
})

def({ operator #name=add #params=%AC!ID[0]($x: INTEGER, $y: INTEGER)
      #type=INTEGER
      #description="Compute the integer sum of the arguments."
      #detail=
"""Compute the sum of the integer arguments."""
})

def({ operator #name=is_bindable #params=%($x) #type=BOOLEAN
      #description="Determine whether an atom is bindable."
      #detail=
"""If the atom $x is bindable, return true.  Otherwise, return false.  This
is primarily useful for guards.  You will probably need to protect
variables by making them into metavariables ($$ instead of $) to
prevent zealous evaluation."""
})

def({ operator #name=fail #params=%()
      #description="Generate a runtime exception."
      #detail=
"""Generate a runtime exception (a VerifyError).  This causes a core dump and
may be useful if you are trying to debug the system."""
})

]]>
</execute>
}