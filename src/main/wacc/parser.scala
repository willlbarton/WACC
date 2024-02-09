package src.main.wacc

import parsley.Parsley._
import parsley.combinator._
import parsley.errors.ErrorBuilder
import parsley.errors.combinator.ErrorMethods
import parsley.expr._
import parsley.{Parsley, Result}
import parsley.syntax.zipped._
import src.main.wacc.lexer.implicits.implicitSymbol
import src.main.wacc.lexer.{character, fully, ident, integer, string}

object parser {
  // Main parser function
  def parse[Err: ErrorBuilder](input: String): Result[Err, Program] = parser.parse(input)

  private lazy val parser = fully(program)

  private lazy val program: Parsley[Program] =
    Program("begin" ~> many(atomic(function)), statements <~ "end")

  // Parses a single function declaration
  private lazy val function: Parsley[Func] =
    Func(typ, ident, "(" ~> sepBy(parameter, ",") <~ ")", "is" ~> functionStatements <~ "end")
  private lazy val parameter: Parsley[Param] = Param(typ, ident)

  // Parses a sequence of statements, which must end with a return or exit
  private lazy val functionStatements: Parsley[Stmt] =
    (many(atomic(statement <~ ";")),
      functionReturn.explain("functions must end with a return or exit")
    ).zipped((stmts, ret) =>
        if (stmts.isEmpty) ret else StmtChain(stmts.reduce(StmtChain(_, _)), ret))

  // The statements a function may end with, if it has sub-statements it must end with one of these
  private lazy val functionReturn = Return("return" ~> expr) |
    Exit("exit" ~> expr) |
    IfStmt("if" ~> expr <~ "then".explain("if statements require \'then\'"),
      functionStatements <~ "else".explain("if statements require an \'else\' branch"),
      functionStatements <~ "fi".explain("if statements must end in \'fi\'")) |
    ScopedStmt("begin" ~> functionStatements <~ "end")

  // Parses a sequence of statements with no requirement to end with a return or exit
  private lazy val statements = chain.right1(statement)(StmtChain <# ";")

  // Parses a single statement
  private lazy val statement: Parsley[Stmt] =
    "skip" #> Skip |
      Decl(typ, ident, "=" ~> rvalue) |
      Asgn(lvalue, "=".explain("unknown statement treated as assignment") ~> rvalue) |
      Read("read" ~> lvalue) |
      Free("free" ~> expr) |
      Print("print" ~> expr) |
      PrintLn("println" ~> expr) |
      Return("return" ~> expr) |
      Exit("exit" ~> expr) |
      IfStmt("if" ~> expr <~ "then".explain("if statements require \'then\'"),
        statements <~ "else".explain("if statements require an \'else\' branch"),
        statements <~ "fi".explain("if statements must end in \'fi\'")) |
      While("while" ~> expr <~ "do", statements <~
        "done".explain("while loops must end with \'done\'")) |
      ScopedStmt("begin" ~> statements <~ "end")

  // Parses a type
  private lazy val typ: Parsley[Type] = atomic(arrayType) | baseType | pairType
  private lazy val pairType: Parsley[PairType] =
    PairType("pair" ~> "(" ~> pairElemType, "," ~> pairElemType <~ ")")
  private lazy val baseType: Parsley[BaseType] =
    "int" #> IntType |
      "bool" #> BoolType |
      "char" #> CharType |
      "string" #> StringType
  private lazy val arrayType: Parsley[ArrayType] =
    chain.postfix1(atomic(baseType) |
      atomic(pairType))(("[".label("array") <~ "]") #> ArrayType)
  private lazy val pairElemType: Parsley[PairElemType] =
    atomic(arrayType) | baseType | "pair" #> Pair

  // Parses an lvalue and rvalue
  private lazy val lvalue: Parsley[LVal] = atomic(arrayElem) | atomic(ident) | pairElem
  private lazy val rvalue: Parsley[RVal] = expr |
    ArrayLiter("[" ~> sepBy(expr, ",") <~ "]") |
    NewPair("newpair" ~> "(" ~> expr, "," ~> expr <~ ")") | pairElem |
    Call("call" ~> ident, "(" ~> sepBy(expr, ",") <~ ")")
  private lazy val pairElem = Fst("fst" ~> lvalue) | Snd("snd" ~> lvalue)
  private lazy val arrayElem = ArrayElem(ident, some("[".label("array index") ~> expr <~ "]"))

  // Parses an expression
  private lazy val expr: Parsley[Expr] = precedence(
    Integer(integer), // An integer may begin with '-', so it must be parsed first
    ("-".label("unary operator") ~> expr).map(x => UnaryApp(Neg, x)),
    Bool("true" #> true | "false" #> false),
    Character(character),
    StringAtom(string),
    "null" #> Null,
    atomic(arrayElem),
    ident,
    BracketedExpr("(" ~> expr <~ ")")
  )(
    Ops(Prefix)(
      Not <# "!",
      Len <# "len ",
      Ord <# "ord ",
      Chr <# "chr "
    ),
    Ops(InfixL)(
      Mul <# "*",
      Div <# "/",
      Mod <# "%"
    ),
    Ops(InfixL)(
      Add <# "+",
      Sub <# "-".label("binary operator")
    ),
    Ops(InfixN)(
      GtEq <# ">=",
      LtEq <# "<=",
      Gt <# ">",
      Lt <# "<"
    ),
    Ops(InfixN)(
      NotEq <# "!=",
      Eq <# "=="
    ),
    Ops(InfixR)(And <# "&&"),
    Ops(InfixR)(Or <# "||")
  )
}
