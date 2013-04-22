/*       _ _     _
 *   ___| (_)___(_) ___  _ __
 *  / _ \ | / __| |/ _ \| '_ \
 * |  __/ | \__ \ | (_) | | | |
 *  \___|_|_|___/_|\___/|_| |_|
 *
 * Copyright (c) 2013 by Stacy Prowell (sprowell@gmail.com).
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
package ornl.elision.repl

import ornl.elision.context.Context
import java.io.FileWriter
import ornl.elision.core.BasicAtom
import ornl.elision.core.RulesetRef
import ornl.elision.core.TypedSymbolicOperator
import ornl.elision.context.NativeCompiler
import ornl.elision.core.Literal
import ornl.elision.dialects.ScalaGenerator

/**
 * Generate compilable Scala source files for a context.
 */
object ContextGenerator {
  
  /**
   * Generate the context as compilable Scala source.  The generated files
   * all use the specified base name.  If this does not specify an absolute
   * path, then the files are placed in the "current working folder," which
   * is determined by the operating system.
   * 
   * The purpose of this is to write a compilable version of the entire context
   * as a single object that, when executed, creates and returns a complete
   * context object.  While this is effective for those items that are
   * contained in the context, it does not necessarily capture those items
   * contained in the executor.
   * 
   * The basic public structure of the file that is created is the following.
   * 
   * {{{
   * object SC21e4fe213a6c {
   *   def main(args: Array[String]) {
   *     ...
   *   }
   *   def populate(context: Context) {
   *     ...
   *   }
   * }
   * }}}
   * 
   * The `main` method can be used to start the context and enter the REPL from
   * the prompt.  The `populate` method adds the necessary information to the
   * provided context.
   * 
   * @param fn          Base file name for files to be generated.
   * @param context     The context to write.
   */
  def generate(fn: String, context: Context) {
    val dot = fn.lastIndexOf('.')
    val slash = fn.lastIndexOf('/') max -1
    val filename = (if (dot > 0) fn.substring(0,dot) else fn) + ".scala"
    val basename =
      (if (dot > 0) fn.substring(slash+1,dot) else fn.substring(slash+1))
    val file = new FileWriter(filename)
    
    // Write boilerplate.
    import ornl.elision.util.Version.{major, minor, build}
    val prop = System.getProperties
    file.append(
        """|/**
           | * Saved context source.  This source file was automatically created.
           | * Elision is Copyright (c) UT-Battelle, LLC.  All rights reserved.
           | *
           | *  - Created on: %s
           | *  - Elision version: %s
           | *  - Elision build: %s
           | *  - Scala version: %s
           | *  - Java vendor: %s
           | *  - Java version: %s
           | *  - OS name: %s
           | *  - OS version: %s
           | *  - Architecture: %s
           | */
           |import ornl.elision.core._
           |import ornl.elision.util.Loc
           |import ornl.elision.repl._
           |import ornl.elision.parse.ProcessorControl
           |import ornl.elision.context.Context
           |object %s {
           |  def main(args: Array[String]) {
           |    // Process the command line arguments.
           |    ReplMain.prep(args) match {
           |      case None =>
           |      case Some(settings) =>
           |        // Build a REPL.
           |        val repl = new ERepl(settings)
           |        // Install the REPL.
           |        knownExecutor = repl
           |        // Reset the context and then populate it.
           |        repl.context = new Context()
           |        populate(repl.context)
           |        // Start the REPL, but don't bootstrap.
           |        ProcessorControl.bootstrap = false
           |        repl.run()
           |        // Done!
           |        repl.clean()
           |    }
           |  }
           |  def populate(context: Context) {
           |""".stripMargin format (
               new java.util.Date,
               major+"."+minor,
               build,
               scala.util.Properties.versionString,
               prop.get("java.vendor"),
               prop.get("java.version"),
               prop.get("os.name"),
               prop.get("os.version"),
               prop.get("os.arch"),
               basename
               )
           )
    
    // Boilerplate text for both cases.
    val pre = "context.declare("
    val post = ")"
           
    // Now we can append the code to create the context.  To do this, we first
    // traverse the rule list and collect all the rules in the order they are
    // present in the library.
    var known = context.Known()
    var thelist = List[BasicAtom]()
    // Now we can collect any remaining unknown operators.  Just traverse the
    // operators and trust the system to write any that are unknown.
    for (operator <- context.operatorLibrary.getAllOperators) {
      val pair = context.collect(operator, known, thelist)
      known = pair._1
      thelist = pair._2
    } // Collect all rules and their dependencies.
    
    for (rule <- context.ruleLibrary.getAllRules) {
      // Write the rule and any dependencies.  The new set of "known" stuff is
      // returned, and we preserve it.
      val pair = context.collect(rule, known, thelist)
      known = pair._1
      thelist = pair._2
    } // Collect all rules and their dependencies.
    
    // Any remaining rulesets can be collected now.
    for (ruleset <- context.ruleLibrary.getAllRulesets) {
      val pair = context.collect(RulesetRef(context.ruleLibrary, ruleset),
          known, thelist)
      known = pair._1
      thelist = pair._2
    } // Collect any missed rulesets.
    
    // Write the pieces now.
    val width = 10 // How many items per stage?
    var stage = 1
    var size = thelist.size
    while (size > 0) {
      file.append("    _stage%d(context)\n" format stage)
      stage += 1
      size -= width
    } // Write the stage invocations.
    file.append("    _complete(context)\n")
    file.append("  } // End of populate.\n")
    stage = 1
    size = 0
    for (item <- thelist) {
      // If at the start of a stage, write the stage declaration.
      if (size == 0) {
        file.append("  private def _stage%d(context: Context) {\n" format stage)
      }
      // Write the item.
      item match {
        case tso: TypedSymbolicOperator =>
          // See if the operator has a native handler.
          tso.handlertxt match {
            case Some(text) =>
              // Found a handler.  Convert it to an object and write it in the
              // stream.
              NativeCompiler.writeStash(tso.loc.source, tso.name, text, file)
              
            case _ =>
          }
          
        case _ =>
      }
      file.append(pre)
      // Ruleset references are unusual.  We need to process them as symbols.
      // The reason for this is that there is no corresponding atom to
      // convert them into, like there is for operator references.  See
      // the declare method for how this is handled.
      val what = item match {
        case rr: RulesetRef => Literal(Symbol(rr.name))
        case x => x
      }
      ScalaGenerator(what, file)
      file.append(post).append('\n')
      // Count the item.
      size += 1
      if (size >= width) {
        // Okay, we have closed out this stage.
        file.append("  } // End of _stage%d.\n" format stage)
        stage += 1
        size = 0
      }
    } // Write the list.
    // If necessary, close the last stage.
    if (size != 0) {
      file.append("  } // End of _stage%d.\n" format stage)
    }
    
    // Now emit the complete method that handles the remaining items.
    file.append("  private def _complete(context: Context) {\n")
    
    // Enable those rulesets that need to be enabled.
    import ornl.elision.util.toQuotedString
    for (ruleset <- context.ruleLibrary.getActiveRulesets) {
      file.append("context.ruleLibrary.enableRuleset(%s)\n".format(
          toQuotedString(ruleset)))
    } // Enable the rulesets that need to be enabled.
    
    // Emit the bindings.
    for (bind <- context.binds) {
      file.append("context.bind(%s, %s)\n" format (
          toQuotedString(bind._1), bind._2.toString))
    } // Write all bindings.
    
    // Emit the cache.  The cache can contain arbitrary stuff, so here we
    // only preserve one item: the list of included files.
    import scala.collection.mutable.Set
    val included = context.fetchAs[Set[String]]("read_once.included", Set[String]())
    file.append("    import scala.collection.mutable.Set\n")
    file.append("    val set = scala.collection.mutable.Set(")
    file.append(included map (toQuotedString(_)) mkString (","))
    file.append(")\n")
    file.append("    context.stash(\"read_once.included\", set)\n")
    
    // Done.  Close up the object.
    file.append("  } // End of _complete.\n")
    file.append("} // End of object.\n")
    file.flush()
    file.close()
  }
}