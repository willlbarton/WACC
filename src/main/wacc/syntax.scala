package src.main.wacc

case class Program(functions: List[Func], body: Stmt)
case class Func(t: Type, name: String, parameters: List[Param], body: Stmt)
case class Param(t: Type, name: String)

sealed trait Type
// ...

sealed trait Stmt
// ...

sealed trait Expr
// ...
