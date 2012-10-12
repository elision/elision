package ornl.elision

import ornl.elision.repl.ERepl
import sys.process._

object MakeContext extends App {
  val repl = new ERepl
  
  if (args.length != 2) {
    print(help)
    System.exit(1)
  } else {
    // bootstrap and read Experimental before changing the working dir
    repl.bootstrap()
    repl.read("bootstrap/Experimental.eli", false)
    
    // get the directory we're working in if given an absolute path
    // otherwise keep using the current directory as the working path
    val regex = "(.*/)?(.*)".r
    val regex(dir,file) = args(0)
    if(dir != null) repl.setProperty("path",dir)
    
    // read the file and export the context
    repl.read(file, false)
    println("Exporting context to "+ args(1))
    repl.context.operatorLibrary("exportContext")(args(1))
    
    println("Done.")
    System.exit(0)
  }
  
  /**
   * Provide command line help.
   */
  def help = {
    val buf = new StringBuilder
    buf.append(
        """|Usage: DumpContext [input .eli file] [output .scala file]
           |
           |DumpContext will read an ELI file and dump the generated
           |context to the specified .scala file for compilation. Once
           |the .scala file is compiled, include the generated .class files
           |on the classpath to ornl.elision.Main to skip loading bootstrap.eli
           |and use the generated .class files instead.
           |""".stripMargin)
    buf
  }
  
}