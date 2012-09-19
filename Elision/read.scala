#!/usr/bin/env scala -classpath bin:latest/elision.jar:.
#
# Test reading using Kryo.
!#

import ornl.elision.core._
import ornl.elision.repl._
import com.esotericsoftware.kryo._
import com.esotericsoftware.kryo.io._

// Read the file context.bin with Kryo.
val kryo = new Kryo
kryo.setInstantiatorStrategy(new org.objenesis.strategy.StdInstantiatorStrategy());
val input = new Input(new java.io.FileInputStream("context.bin"))
val repl = kryo.readObject(input, classOf[ERepl])
input.close

// See if things work.
repl.banner()
println(repl.context.toParseString)

// Done.
sys.exit(0)

// vim: set ts=4 sw=4 et:
