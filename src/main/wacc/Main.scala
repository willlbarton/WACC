package src.main.wacc

import parsley.{Failure, Success}
import scala.sys.exit
import java.io.IOException

object Main {

  private final val syntaxErrCode: Int = 100
  private final val semanticErrCode: Int = 200

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

        program match {
          case "" => println("Please enter a valid filepath!")
          case _ =>
            parser.parse(program) match {
              case Success(program) =>
                analyser.analyse(program) match {
                  case "" => generator.generate(program, x86Formatter)
                  case msg =>
                    println(
                      s"Semantic errors detected during compilation!\n" ++
                        s"Exit code $semanticErrCode returned:\n$msg"
                    )
                    exit(semanticErrCode)
                }
              case Failure(msg) =>
                println(s"Errors detected during compilation!\n" ++
                  s"Exit code $syntaxErrCode returned:\n$msg")
                exit(syntaxErrCode)
            }
        }
      case None => println("Please enter a filepath")
    }
  }
}
