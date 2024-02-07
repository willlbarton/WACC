package src.main.wacc

import parsley.{Failure, Success}
import scala.sys.exit

object Main {
  def main(args: Array[String]): Unit = {
    args.headOption match {
      // Attempt to parse, then analyse and finally generate code
      case Some(filepath) => {
        val source = scala.io.Source.fromFile(filepath)
        val program =
          try source.getLines().mkString("\n")
          finally source.close()

        program match {
          case "" => println("Please enter a valid filepath!")
          case _ =>
            parser.parse(program) match {
              case Success(x) =>
                analyser.analyse(x) match {
                  case "" => println("No errors detected!") // generate code
                  case msg =>
                    println(
                      s"Semantic errors detected during compilation!\nExit code 200 returned:\n$msg"
                    )
                    exit(200)
                }
              case Failure(msg) =>
                println(s"Errors detected during compilation!\nExit code 100 returned:\n$msg")
                exit(100)
            }
        }

      }
      case None => println("please enter a filepath")
    }
  }
}
