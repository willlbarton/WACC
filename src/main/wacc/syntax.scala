package src.main.wacc

case class Program(functions: List[Func], body: Stmt)
case class Func(t: Type, name: String, parameters: List[Param], body: Stmt)
case class Param(t: Type, name: String)

sealed trait Type
sealed trait PairElemType extends Type
sealed trait BaseType extends PairElemType

case object IntType extends BaseType
case object BoolType extends BaseType
case object CharType extends BaseType
case object StringType extends BaseType

case class ArrayType(t: Type) extends PairElemType
case object Pair extends PairElemType

case class PairType(fst: PairElemType, snd: PairElemType) extends Type

sealed trait Stmt
// ...

sealed trait Expr
// ...
