package src.main.wacc

import parsley.generic

case class Program(functions: List[Func], body: Stmt)

// An empty 'params' list should be the same as no param-list in the syntax
case class Func(t: Type, name: Var, params: List[Param], body: Stmt)
case class Param(t: Type, name: Var)

sealed trait Type
sealed trait PairElemType extends Type
sealed trait BaseType extends PairElemType

// <base-type>
case object IntType extends BaseType
case object BoolType extends BaseType
case object CharType extends BaseType
case object StringType extends BaseType

// <pair-elem-type)
case class ArrayType(t: Type) extends PairElemType
case object Pair extends PairElemType

// <pair-type>
case class PairType(fst: PairElemType, snd: PairElemType) extends Type

// <stmnt>
sealed trait Stmt
case object Skip extends Stmt
case class Decl(t: Type, name: Var, value: RVal) extends Stmt
case class Asgn(left: LVal, value: RVal) extends Stmt
case class Read(value: LVal) extends Stmt
case class Free(expr: Expr) extends Stmt
case class Return(expr: Expr) extends Stmt
case class Exit(expr: Expr) extends Stmt
case class Print(expr: Expr) extends Stmt
case class PrintLn(expr: Expr) extends Stmt
case class IfStmt(cond: Expr, body1: Stmt, body2: Stmt) extends Stmt
case class While(cond: Expr, body: Stmt) extends Stmt
case class ScopedStmt(stmt: Stmt) extends Stmt
case class StmtChain(stmt: Stmt, next: Stmt) extends Stmt

// <rvalue>
sealed trait RVal
case class ArrayLiter(elems: List[Expr]) extends RVal
case class NewPair(fst: Expr, snd: Expr) extends RVal
case class Call(name: Var, args: List[Expr]) extends RVal

// <lvalue>
sealed trait LVal
case class Fst(value: LVal) extends LVal with RVal // <pair-elem>
case class Snd(value: LVal) extends LVal with RVal // <pair-elem>
sealed trait Expr extends RVal
case class UnaryApp(op: UnaryOp, expr: Expr) extends Expr
case class BinaryApp(op: BinaryOp, left: Expr, right: Expr) extends Expr

// <atom>
case class Integer(i: Int) extends Expr
case class Bool(value: Boolean) extends Expr
case class Character(c: Char) extends Expr
case class StringAtom(s: String) extends Expr
case object Null extends Expr // <pair-liter>
case class Var(name: String) extends Expr with LVal
case class ArrayElem(name: Var, exprs: List[Expr]) extends Expr with LVal
case class BracketedExpr(expr: Expr) extends Expr

// <unary-oper>
trait UnaryOp
case object Not extends UnaryOp
case object Neg extends UnaryOp
case object Len extends UnaryOp
case object Ord extends UnaryOp
case object Chr extends UnaryOp

// <binary-oper>
trait BinaryOp
case object Mul extends BinaryOp
case object Div extends BinaryOp
case object Mod extends BinaryOp
case object Add extends BinaryOp
case object Sub extends BinaryOp
case object Gt extends BinaryOp
case object GtEq extends BinaryOp
case object Lt extends BinaryOp
case object LtEq extends BinaryOp
case object Eq extends BinaryOp
case object NotEq extends BinaryOp
case object And extends BinaryOp
case object Or extends BinaryOp

object Program extends generic.ParserBridge2[List[Func], Stmt, Program]
object Func extends generic.ParserBridge4[Type, Var, List[Param], Stmt, Func]
object Param extends generic.ParserBridge2[Type, Var, Param]

// object ArrayType extends generic.ParserBridge1[Type, ArrayType]
object PairType extends generic.ParserBridge2[PairElemType, PairElemType, PairType]

object StmtChain extends generic.ParserBridge2[Stmt, Stmt, StmtChain]
object Decl extends generic.ParserBridge3[Type, Var, RVal, Stmt]
object Asgn extends generic.ParserBridge2[LVal, RVal, Stmt]
object Read extends generic.ParserBridge1[LVal, Stmt]
object Free extends generic.ParserBridge1[Expr, Stmt]
object Return extends generic.ParserBridge1[Expr, Stmt]
object Exit extends generic.ParserBridge1[Expr, Stmt]
object Print extends generic.ParserBridge1[Expr, Stmt]
object PrintLn extends generic.ParserBridge1[Expr, Stmt]
object IfStmt extends generic.ParserBridge3[Expr, Stmt, Stmt, Stmt]
object While extends generic.ParserBridge2[Expr, Stmt, Stmt]
object ScopedStmt extends generic.ParserBridge1[Stmt, Stmt]

object Var extends generic.ParserBridge1[String, Var]
object Integer extends generic.ParserBridge1[Int, Integer]
object Bool extends generic.ParserBridge1[Boolean, Bool]
object Character extends generic.ParserBridge1[Char, Character]
object StringAtom extends generic.ParserBridge1[String, StringAtom]
object BracketedExpr extends generic.ParserBridge1[Expr, BracketedExpr]

object ArrayElem extends generic.ParserBridge2[Var, List[Expr], ArrayElem]
object Fst extends generic.ParserBridge1[LVal, Fst]
object Snd extends generic.ParserBridge1[LVal, Snd]

object ArrayLiter extends generic.ParserBridge1[List[Expr], ArrayLiter]
object NewPair extends generic.ParserBridge2[Expr, Expr, NewPair]
object Call extends generic.ParserBridge2[Var, List[Expr], Call]
