package src.main.wacc

import parsley.Parsley._
import parsley.combinator._
import parsley.errors.ErrorBuilder
import parsley.expr._
import parsley.{Parsley, Result}
import src.main.wacc.lexer.implicits.implicitSymbol
import src.main.wacc.lexer.{character, fully, ident, integer, string}

object parser {
  def parse[Err: ErrorBuilder](input: String): Result[Err, Program] = parser.parse(input)

  private lazy val parser = fully(program)

  private lazy val program: Parsley[Program] =
    Program("begin" ~> many(atomic(function)), statements <~ "end")

  private lazy val function: Parsley[Func] =
    Func(typ, ident, "(" ~> sepBy(parameter, ",") <~ ")", "is" ~> statements <~ "end")
  private lazy val parameter: Parsley[Param] = Param(typ, ident)

  private val statements = chain.right1(statement)(StmtChain <# ";")
  private lazy val statement: Parsley[Stmt] =
    "skip" #> Skip |
    Decl(typ, ident, "=" ~> rvalue) |
    Asgn(lvalue, "=" ~> rvalue) |
    Read("read" ~> lvalue) |
    Free("free" ~> expr) |
    Print("print" ~> expr) |
    PrintLn("println" ~> expr) |
    // for functions, the last statement in a chain MUST be one of the below
    Return("return" ~> expr) |
    Exit("exit" ~> expr) |
    IfStmt("if" ~> expr <~ "then", statements, "else" ~> statements <~ "fi") |
    While("while" ~> expr <~ "do", statements <~ "done") |
    ScopedStmt("begin" ~> statements <~ "end")

  private lazy val pairType: Parsley[PairType] =
    PairType("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")
  private lazy val typ: Parsley[Type] = atomic(arrayType) | baseType |
    pairType
  private lazy val baseType: Parsley[BaseType] =
    "int" #> IntType |
    "bool" #> BoolType |
    "char" #> CharType |
    "string" #> StringType
  private lazy val arrayType: Parsley[ArrayType] =
    chain.postfix1(atomic(baseType) | atomic(pairType))(("[" <~ "]") #> ArrayType)
  private lazy val pairElemType: Parsley[PairElemType] =
    atomic(arrayType) | baseType | "pair" #> Pair

  private lazy val lvalue: Parsley[LVal] = atomic(arrayElem) | atomic(ident) | pairElem
  private lazy val rvalue: Parsley[RVal] = expr |
    ArrayLiter("[" ~> sepBy(expr, ",") <~ "]") |
    NewPair("newpair" ~> "(" ~> expr, "," ~> expr <~ ")") | pairElem |
    Call("call" ~> ident, "(" ~> sepBy(expr, ",") <~ ")")
  private lazy val pairElem = Fst("fst" ~> lvalue) | Snd("snd" ~> lvalue)
  private lazy val arrayElem = ArrayElem(ident, some("[" ~> expr <~ "]"))

  private lazy val expr: Parsley[Expr] = precedence(
    Integer(integer),
    ("-" ~> expr).map(x => UnaryApp(Neg, x)),
    Bool("true" #> true | "false" #> false),
    Character(character),
    StringAtom(string),
    "null" #> Null,
    atomic(arrayElem),
    ident,
    BracketedExpr("(" ~> expr <~ ")")
  )(
    Ops(Prefix)(
      "!" #> (x => UnaryApp(Not, x)),
      "len" #> (x => UnaryApp(Len, x)),
      "ord" #> (x => UnaryApp(Ord, x)),
      "chr" #> (x => UnaryApp(Chr, x))
    ),
    Ops(InfixL)(
      "*" #> ((x, y) => BinaryApp(Mul, x, y)),
      "%" #> ((x, y) => BinaryApp(Mod, x, y)),
      "/" #> ((x, y) => BinaryApp(Div, x, y))
    ),
    Ops(InfixL)("+" #> ((x, y) => BinaryApp(Add, x, y)), "-" #> ((x, y) => BinaryApp(Sub, x, y))),
    Ops(InfixN)(
      ">=" #> ((x, y) => BinaryApp(GtEq, x, y)),
      "<=" #> ((x, y) => BinaryApp(LtEq, x, y)),
      ">" #> ((x, y) => BinaryApp(Gt, x, y)),
      "<" #> ((x, y) => BinaryApp(Lt, x, y))
    ),
    Ops(InfixN)(
      "!=" #> ((x, y) => BinaryApp(NotEq, x, y)),
      "==" #> ((x, y) => BinaryApp(Eq, x, y))
    ),
    Ops(InfixR)("&&" #> ((x, y) => BinaryApp(And, x, y))),
    Ops(InfixR)("||" #> ((x, y) => BinaryApp(Or, x, y)))
  )
}
