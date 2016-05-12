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
package ornl

import scala.collection.immutable.BitSet

/** 
 * Elision is a term rewriter.
 * 
 * This is the root package of the Elision system, and it is responsible for
 * startup and command line processing.
 * 
 * Starting the Elision system is the job of [[ornl.elision.Main]].
 * 
 * This package uses [[ornl.elision.cli]] and [[ornl.elision.util]], but should
 * not use any other package.  No other package in Elision may use this package.
 * 
 * == Packages ==
 * There are several sub-packages.
 *  - [[ornl.elision.actors]] contains actors used for internal communication.
 *  - [[ornl.elision.cli]] contains classes for interpreting the CLI.
 *  - [[ornl.elision.core]] is the core package that contains the primary classes.
 *  - [[ornl.elision.dialects]] provides generators to convert atoms to strings.
 *  - [[ornl.elision.gui]] contains the Elision GUI, Eva.
 *  - [[ornl.elision.parse]] contains the Elision parser(s).
 *  - [[ornl.elision.profile]] contains the Elision profiler.
 *  - [[ornl.elision.repl]] contains the Elision REPL(s) and related classes.
 *  - [[ornl.elision.syntax]] contains the syntax highlighting system.
 *  - [[ornl.elision.util]] contains common utilities used by the Elision system.
 * 
 * Packages in Elision are organized into a use hierarchy.  In general, packages
 * may use sub-packages, but not the reverse.  Packages may use sibling packages,
 * provided this does not result in a cycle.
 *  
 * == Punch List ==
 * This is the current punch list for Elision.  This list gets picked up by
 * [[http://eclipse.org Eclipse]].
 * 
 *  - TODO Need package.scala files for all packages.  Stacy.
 *  - TODO Implicit corecions. Mark.
 *  - TODO Split compilable context into stages to avoid load problems.  Stacy
 *  - TODO Infix. DEFER.
 */
package object elision
