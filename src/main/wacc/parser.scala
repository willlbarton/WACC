package src.main.wacc

import parsley.Parsley._
import parsley.combinator._
import parsley.errors.ErrorBuilder
import parsley.expr.chain
import parsley.{Parsley, Result}
import src.main.wacc.lexer.implicits.implicitSymbol
import src.main.wacc.lexer.{fully, ident}

object parser {
  def parse[Err: ErrorBuilder](input: String): Result[Err, Program] = parser.parse(input)

  private lazy val parser = fully(program)

  private lazy val program: Parsley[Program] =
    Program("begin" ~> many(function), statement <~ "end")

  private lazy val function: Parsley[Func] =
    Func(typ, ident, "(" ~> sepBy(parameter, ",") <~ ")", "is" ~> statement <~ "end")
  private lazy val parameter = Param(typ, ident)

  private lazy val statement: Parsley[Stmt] = "skip" #> Skip |
    Decl(typ, ident, "=" ~> rvalue) |
    Asgn(ident, "=" ~> rvalue) |
    Read("read" ~> lvalue) |
    Free("free" ~> expr) |
    Return("return" ~> expr) |
    Exit("exit" ~> expr) |
    Print("print" ~> expr) |
    PrintLn("println" ~> expr) |
    IfStmt("if" ~> expr <~ "then", statement, "else" ~> statement <~ "fi") |
    While("while" ~> expr <~ "do", statement <~ "done") |
    ScopedStmt("begin" ~> statement <~ "end") |
    chain.left1(statement)(";" as StmtChain)

  private lazy val typ: Parsley[Type] = baseType | arrayType |
    PairType("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")
  private lazy val baseType: Parsley[BaseType] = "int" #> IntType |
    "bool" #> BoolType |
    "char" #> CharType |
    "string" #> StringType
  private lazy val arrayType =  ArrayType(baseType <~ "[" <~ "]")
  private lazy val pairElemType: Parsley[PairElemType] = baseType | arrayType | "pair" #> Pair

  private lazy val rvalue: Parsley[RVal] = ???

  private lazy val lvalue: Parsley[LVal] = ???

  private lazy val expr: Parsley[Expr] = ???
}
