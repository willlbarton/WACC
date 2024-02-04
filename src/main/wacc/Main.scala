package src.main.wacc

import parsley.{Failure, Success}

import scala.sys.exit

object Main {
    def main(args: Array[String]): Unit = {
        args.headOption match {
            case Some(expr) => parser.parse(expr) match {
                case Success(x) => println(s"$expr = $x")
                case Failure(msg) =>
                    println(s"Errors detected during compilation! Exit code 100 returned: $msg")
                    exit(100)
            }
            case None => println("please enter an expression")
        }
    }
}
