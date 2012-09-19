#!/usr/bin/env scala -cp latest/elision.jar:bin:.
#
# Test writing using Kryo.
!#

import ornl.elision.repl._
import ornl.elision.core._
import com.esotericsoftware.kryo._
import com.esotericsoftware.kryo.io._

// Make, and bootstrap, a REPL.  Then write the context so that it is clear that
// it contains some declarations.
val repl = new ERepl
repl.banner()
println(repl.context.operatorLibrary.toParseString)
println("="*70)
repl.bootstrap()
println(repl.context.operatorLibrary.toParseString)
println("="*70)

// Now write the context using Kryo to the file context.bin.
val kryo = new Kryo
val output = new Output(new java.io.FileOutputStream("context.bin"))
kryo.writeObject(output, repl)
output.flush
output.close

// Done.
sys.exit(0)

// vim: set ts=4 sw=4 et:
