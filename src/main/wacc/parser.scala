package src.main.wacc

import parsley.{Parsley, Result}
import parsley.syntax.zipped._
import parsley.Parsley._
import parsley.combinator._
import parsley.errors.ErrorBuilder
import lexer.implicits.implicitSymbol
import lexer.{fully, ident}
import parsley.character.string

object parser {
  def parse[Err: ErrorBuilder](input: String): Result[Err, Program] = parser.parse(input)

  private lazy val parser = fully(program)

  private lazy val program: Parsley[Program] =
    Program("begin" ~> many(function), statement <~ "end")

  private lazy val function: Parsley[Func] =
    Func(typ, ident, "(" ~> sepBy(parameter, ",") <~ ")", "is" ~> statement <~ "end")
  private lazy val parameter = Param(typ, ident)

  private lazy val statement: Parsley[Stmt] = Skip from "skip" |
    Decl(typ, ident, "=" ~> rvalue) |
    Asgn(ident, "=" ~> rvalue) |
    Read("read" ~> lvalue)

  private lazy val typ: Parsley[Type] = ???

  private lazy val rvalue: Parsley[RVal] = ???

  private lazy val lvalue: Parsley[LVal] = ???
}
