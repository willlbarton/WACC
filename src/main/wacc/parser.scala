package src.main.wacc

import parsley.Parsley._
import parsley.combinator._
import parsley.errors.ErrorBuilder
import parsley.expr._
import parsley.{Parsley, Result}
import src.main.wacc.lexer.implicits.implicitSymbol
import src.main.wacc.lexer.{character, fully, ident, nat, string}

object parser {
  def parse[Err: ErrorBuilder](input: String): Result[Err, Program] = parser.parse(input)

  private lazy val parser = fully(program)

  private lazy val program: Parsley[Program] =
    Program("begin" ~> many(function), statement <~ "end")

  private lazy val function: Parsley[Func] =
    Func(typ, ident, "(" ~> sepBy(parameter, ",") <~ ")", "is" ~> statement <~ "end")
  private lazy val parameter = Param(typ, ident)

  private val statements = chain.left1(statement)(";" #> StmtChain)
  private lazy val statement: Parsley[Stmt] =
    "skip" #> Skip |
    Decl(typ, ident, "=" ~> rvalue) |
    Asgn(ident, "=" ~> rvalue) |
    Read("read" ~> lvalue) |
    Free("free" ~> expr) |
    Return("return" ~> expr) |
    Exit("exit" ~> expr) |
    Print("print" ~> expr) |
    PrintLn("println" ~> expr) |
    IfStmt("if" ~> expr <~ "then", statements, "else" ~> statements <~ "fi") |
    While("while" ~> expr <~ "do", statements <~ "done") |
    ScopedStmt("begin" ~> statements <~ "end")

  private lazy val typ: Parsley[Type] = baseType | arrayType |
    PairType("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")
  private lazy val baseType: Parsley[BaseType] =
    "int" #> IntType |
    "bool" #> BoolType |
    "char" #> CharType |
    "string" #> StringType
  private lazy val arrayType =  ArrayType(baseType <~ "[" <~ "]")
  private lazy val pairElemType: Parsley[PairElemType] = baseType | arrayType | "pair" #> Pair

  private lazy val lvalue: Parsley[LVal] = atomic(ident) | pairElem | arrayElem
  private lazy val rvalue: Parsley[RVal] = expr |
    ArrayLiter("[" ~> sepBy(expr, ",") <~ "]") |
    NewPair("newpair" ~> "(" ~> expr, "," ~> expr <~ ")") | pairElem |
    Call("call" ~> ident, "(" ~> sepBy1(expr, ",") <~ ")")
  private lazy val pairElem = Fst("fst" ~> lvalue) | Snd("snd" ~> lvalue)

  private lazy val expr: Parsley[Expr] = precedence(
    Integer(nat),
    Bool("true" #> true | "false" #> false),
    Character(character),
    StringAtom(string),
    "null" #> Null,
    ident,
    arrayElem,
    BracketedExpr("(" ~> expr <~ ")")
  )(
    Ops(Prefix)(
      "!" #> (x => UnaryApp(Not, x)),
      "-" #> (x => UnaryApp(Neg, x)),
      "len" #> (x => UnaryApp(Len, x)),
      "ord" #> (x => UnaryApp(Ord, x)),
      "chr" #> (x => UnaryApp(Chr, x))),
    Ops(InfixL)(
      "*" #> ((x, y) => BinaryApp(Mul, x, y)),
      "%" #> ((x, y) => BinaryApp(Mod, x, y)),
      "/" #> ((x, y) => BinaryApp(Div, x, y))),
    Ops(InfixL)(
      "+" #> ((x, y) => BinaryApp(Add, x, y)),
      "-" #> ((x, y) => BinaryApp(Sub, x, y))),
    Ops(InfixN)(
      ">" #> ((x, y) => BinaryApp(Gt, x, y)),
      ">=" #> ((x, y) => BinaryApp(GtEq, x, y)),
      "<" #> ((x, y) => BinaryApp(Lt, x, y)),
      "<=" #> ((x, y) => BinaryApp(LtEq, x, y))),
    Ops(InfixN)(
      "==" #> ((x, y) => BinaryApp(Eq, x, y)),
      "!=" #> ((x, y) => BinaryApp(NotEq, x, y))),
    Ops(InfixR)(
      "&&" #> ((x, y) => BinaryApp(And, x, y))),
    Ops(InfixR)(
      "||" #> ((x, y) => BinaryApp(Or, x, y)))
  )
  private lazy val arrayElem = ArrayElem(ident, some("[" ~> expr <~ "]"))
}
