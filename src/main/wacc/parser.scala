package src.main.wacc

import parsley.{Parsley, Result}
import parsley.syntax.zipped._
import parsley.Parsley._
import parsley.combinator._
import parsley.errors.ErrorBuilder

import lexer.implicits.implicitSymbol
import lexer.{fully, ident}

object parser {
  def parse[Err: ErrorBuilder](input: String): Result[Err, Program] = parser.parse(input)

  private lazy val parser = fully(program)

  private lazy val program: Parsley[Program] =
    ("begin" ~> many(function), statement <~ "end").zipped(Program)

  private lazy val function: Parsley[Func] =
    (typ, ident, "(" ~> sepBy(parameter, ",") <~ ")", "is" ~> statement <~ "end")
      .zipped(Func)
  private lazy val parameter = (typ, ident).zipped(Param)

  private lazy val statement: Parsley[Stmt] = ???

  private lazy val typ: Parsley[Type] = ???

  var rvalue: Parsley[RVal] = ???
}
