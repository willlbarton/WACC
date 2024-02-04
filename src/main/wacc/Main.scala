package src.main.wacc

import parsley.{Failure, Success}
import scala.sys.exit

object Main {
    def main(args: Array[String]): Unit = {

        args.headOption match {
            case Some(expr) => parser.parse(expr) match {
                case Success(x) => analyser.analyse(x) match {
                    case ""  => ??? // generate code
                    case msg =>
                      println(s"Errors detected during compilation! Exit code 200 returned: $msg")
                      exit(200)
                }
                case Failure(msg) =>
                  println(s"Errors detected during compilation! Exit code 100 returned: $msg")
                  exit(100)
            }
            case None => println("please enter an expression")
        }
    }
}
