package src.main.wacc

import parsley.generic

sealed trait SymbolTableObj {
  var typ: Option[Type] = None
}

case class Program(functions: List[Func], body: Stmt)

// An empty 'params' list should be the same as no param-list in the syntax
case class Func(t: Type, ident: Ident, params: List[Param], body: Stmt) extends SymbolTableObj {
  typ = Some(t)
}
case class Param(t: Type, ident: Ident)

sealed trait Type
sealed trait PairElemType extends Type
sealed trait BaseType extends PairElemType

// <base-type>
case object IntType extends BaseType { override def toString = "int" }
case object BoolType extends BaseType { override def toString = "bool" }
case object CharType extends BaseType { override def toString = "char" }
case object StringType extends BaseType { override def toString = "string" }

// <pair-elem-type)
case class ArrayType(t: Type) extends PairElemType { override def toString = s"$t[]" }
case object Pair extends PairElemType { override def toString = "pair" }
case object NullType extends Type { override def toString = "unknown" }

// <pair-type>
case class PairType(fst: PairElemType, snd: PairElemType) extends Type {
  override def toString = s"pair($fst, $snd)"
}

// <stmnt>
sealed trait Stmt
case object Skip extends Stmt
case class Decl(t: Type, ident: Ident, value: RVal) extends Stmt
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
sealed trait RVal extends SymbolTableObj
case class ArrayLiter(elems: List[Expr]) extends RVal {
  override def toString: String = elems.mkString("[", ", ", "]")
}
case class NewPair(fst: Expr, snd: Expr) extends RVal {
  override def toString: String = s"newpair($fst, $snd)"
}
case class Call(ident: Ident, args: List[Expr]) extends RVal {
  override def toString: String = s"call $ident(${args.mkString(", ")})"
}

// <lvalue>
sealed trait LVal
case class Fst(value: LVal) extends LVal with RVal { override def toString: String = s"fst $value" }
case class Snd(value: LVal) extends LVal with RVal { override def toString: String = s"snd $value" }
sealed trait Expr extends RVal
case class UnaryApp(op: UnaryOp, expr: Expr) extends Expr {
  override def toString: String = s"$op $expr"
}
case class BinaryApp(op: BinaryOp, left: Expr, right: Expr) extends Expr {
  override def toString: String = s"$left $op $right"
}

// <atom>
case class Integer(i: Int) extends Expr {
  override def toString: String = i.toString
}
case class Bool(value: Boolean) extends Expr {
  override def toString: String = value.toString
}
case class Character(c: Char) extends Expr {
  override def toString: String = s"'$c'"
}

case class StringAtom(s: String) extends Expr {
  override def toString: String = s
}

case object Null extends Expr {
  override def toString = "null"
} // <pair-liter>
case class Ident(name: String) extends Expr with LVal { override def toString: String = name }
case class ArrayElem(ident: Ident, exprs: List[Expr]) extends Expr with LVal {
  override def toString: String = s"$ident[${exprs.mkString("][")}]"
}
case class BracketedExpr(expr: Expr) extends Expr { override def toString: String = s"($expr)" }

// <unary-oper>
trait UnaryOp
case object Not extends UnaryOp { override def toString = "!" }
case object Neg extends UnaryOp { override def toString = "-" }
case object Len extends UnaryOp { override def toString = "len" }
case object Ord extends UnaryOp { override def toString = "ord" }
case object Chr extends UnaryOp { override def toString = "chr" }

// <binary-oper>
trait BinaryOp
case object Mul extends BinaryOp { override def toString = "*" }
case object Div extends BinaryOp { override def toString = "/" }
case object Mod extends BinaryOp { override def toString = "%" }
case object Add extends BinaryOp { override def toString = "+" }
case object Sub extends BinaryOp { override def toString = "-" }
case object Gt extends BinaryOp { override def toString = ">" }
case object GtEq extends BinaryOp { override def toString = ">=" }
case object Lt extends BinaryOp { override def toString = "<" }
case object LtEq extends BinaryOp { override def toString = "<=" }
case object Eq extends BinaryOp { override def toString = "==" }
case object NotEq extends BinaryOp { override def toString = "!=" }
case object And extends BinaryOp { override def toString = "&&" }
case object Or extends BinaryOp { override def toString = "||" }

// parser bridges

object Program extends generic.ParserBridge2[List[Func], Stmt, Program]
object Func extends generic.ParserBridge4[Type, Ident, List[Param], Stmt, Func]
object Param extends generic.ParserBridge2[Type, Ident, Param]

// object ArrayType extends generic.ParserBridge1[Type, ArrayType]
object PairType extends generic.ParserBridge2[PairElemType, PairElemType, PairType]

object StmtChain extends generic.ParserBridge2[Stmt, Stmt, StmtChain]
object Decl extends generic.ParserBridge3[Type, Ident, RVal, Stmt]
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

object Ident extends generic.ParserBridge1[String, Ident]
object Integer extends generic.ParserBridge1[Int, Integer]
object Bool extends generic.ParserBridge1[Boolean, Bool]
object Character extends generic.ParserBridge1[Char, Character]
object StringAtom extends generic.ParserBridge1[String, StringAtom]
object BracketedExpr extends generic.ParserBridge1[Expr, BracketedExpr]

object ArrayElem extends generic.ParserBridge2[Ident, List[Expr], ArrayElem]
object Fst extends generic.ParserBridge1[LVal, Fst]
object Snd extends generic.ParserBridge1[LVal, Snd]

object ArrayLiter extends generic.ParserBridge1[List[Expr], ArrayLiter]
object NewPair extends generic.ParserBridge2[Expr, Expr, NewPair]
object Call extends generic.ParserBridge2[Ident, List[Expr], Call]
