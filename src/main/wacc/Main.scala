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
                      println(s"Semantic errors detected during compilation!\nExit code 200 returned:\n$msg")
                      exit(200)
                }
                case Failure(msg) =>
                  println(s"Errors detected during compilation!\nExit code 100 returned:\n$msg")
                  exit(100)
            }
            case None => println("please enter an expression")
        }
    }
}
