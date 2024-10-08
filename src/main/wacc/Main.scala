package src.main.wacc

import parsley.{Failure, Success}

import scala.sys.exit
import java.io.{BufferedWriter, FileWriter, IOException}

object Main {

  private final val syntaxErrCode: Int = 100
  private final val semanticErrCode: Int = 200

  private final val optimiseFlag = "-o"

  def main(args: Array[String]): Unit = {
    args.headOption match {
      // Attempt to parse, then analyse and finally generate code
      case Some(filepath) =>
        val source =
          try scala.io.Source.fromFile(filepath)
          catch { case _: IOException => println(s"Invalid filename or path: $filepath"); exit(1) }
        val program =
          try source.getLines().mkString("\n")
          finally source.close()
        val filename = filepath.split("/").last
        println(s"Compiling $filename...")

        program match {
          case "" => println("Please enter a valid filepath!")
          case _ =>
            parser.parse(program) match {
              case Success(program) =>
                analyser.analyse(program) match {
                  case "" =>
                    val outputFile = filename.replaceFirst("\\.\\w+$", ".s")
                    val writer = new BufferedWriter(
                      new FileWriter(outputFile)
                    )
                    try {
                      val optimise_? = args.contains(optimiseFlag)
                      val optimisedTree = if (optimise_?) treeOptimiser.optimise(program)
                                          else program
                      val (unoptimised, funcs) = generator.generate(optimisedTree)
                      val optimised = if (optimise_?) codeOptimiser.optimise(unoptimised, funcs)
                                      else unoptimised
                      val formatted = formatter.format(optimised, x86Formatter)
                      writer.write(formatted)
                      println(s"Compilation successful! Output written to $outputFile")
                    } finally writer.close()
                  case msg =>
                    println(
                      s"Semantic errors detected during compilation!\n" ++
                        s"Exit code $semanticErrCode returned:\n$msg"
                    )
                    exit(semanticErrCode)
                }
              case Failure(msg) =>
                println(
                  s"Errors detected during compilation!\n" ++
                    s"Exit code $syntaxErrCode returned:\n$msg"
                )
                exit(syntaxErrCode)
            }
        }
      case None => println("Please enter a filepath")
    }
  }
}
