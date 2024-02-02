package src.main.wacc

import parsley.{Success, Failure}

object Main {
    def main(args: Array[String]): Unit = {

        args.headOption match {
            case Some(expr) => parser.parse(expr) match {
                case Success(x) => analyser.analyse(x) match {
                    case ""  => ??? // generate code
                    case msg => println(msg)
                }
                case Failure(msg) => println(msg)
            }
            case None => println("please enter an expression")
        }
    }
}
