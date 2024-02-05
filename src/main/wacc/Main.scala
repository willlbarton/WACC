package src.main.wacc

import parsley.{Failure, Success}
import scala.sys.exit

object Main {
    def main(args: Array[String]): Unit = {

        args.headOption match {
            case Some(expr) => parser.parse(expr) match {
                case Success(x) => analyser.analyse(x) match {
                    case ""  => println("No errors detected!") // generate code
                    case msg =>
                      println(
                        "Semantic errors detected during compilation, exiting with error code 100."
                      )
                      println(msg)
                      exit(200)
                }
                case Failure(msg) =>
                  println("Syntax errors detected during compilation, exiting with error code 100.")
                  println(msg)
                  exit(100)
            }
            case None => println("please enter an expression")
        }
    }
}
