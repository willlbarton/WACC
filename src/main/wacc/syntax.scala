package src.main.wacc

import parsley.generic

// AST nodes that can be stored in the symbol table
sealed trait SymbolTableObj {
  var typ: Option[Type] = None // The type of the object
}
sealed trait ScopedBody extends SymbolTableObj {
  var vars: List[SymbolTableObj] = List.empty
}

// Main program
final case class Program(functions: List[Func], body: List[Stmt]) extends ScopedBody

// <func>
final case class Func(t: Type, ident: Ident, params: List[Param], body: List[Stmt])
    extends SymbolTableObj
    with ScopedBody {
  typ = Some(t)
  override def toString: String =
    s"$ident(${(for (p <- params) yield { s"${p.t} ${p.ident}" }).mkString(", ")})"
}
final case class Param(t: Type, ident: Ident) extends SymbolTableObj { typ = Some(t) }

sealed trait Type
sealed trait PairElemType extends Type // Types that can be used inside a pair
sealed trait BaseType extends PairElemType

// <base-type>
case object IntType extends BaseType { override def toString = "int" }
case object BoolType extends BaseType { override def toString = "bool" }
case object CharType extends BaseType { override def toString = "char" }
case object StringType extends BaseType { override def toString = "string" }

// <pair-elem-type)
final case class ArrayType(t: Type) extends PairElemType { override def toString = s"$t[]" }
// Pairs inside other pairs have their types erased
case object Pair extends PairElemType { override def toString = "pair" }
case object NullType extends Type { override def toString = "unknown" }

// <pair-type>
final case class PairType(fst: PairElemType, snd: PairElemType) extends Type {
  override def toString = s"pair($fst, $snd)"
}

// <stmnt>
sealed trait Stmt
case object Skip extends Stmt { override def toString = "skip" }
final case class Decl(t: Type, ident: Ident, value: RVal) extends Stmt {
  override def toString: String = s"$t $ident = $value"
}
final case class Asgn(left: LVal, value: RVal) extends Stmt {
  override def toString: String = s"$left = $value"
}
final case class Read(value: LVal) extends Stmt { override def toString: String = s"read $value" }
final case class Free(expr: Expr) extends Stmt { override def toString: String = s"free $expr" }
final case class Return(expr: Expr) extends Stmt { override def toString: String = s"return $expr" }
final case class Exit(expr: Expr) extends Stmt { override def toString: String = s"exit $expr" }
final case class Print(expr: Expr) extends Stmt { override def toString: String = s"print $expr" }
final case class PrintLn(expr: Expr) extends Stmt {
  override def toString: String = s"println $expr"
}
final case class IfStmt(cond: Expr, body1: List[Stmt], body2: List[Stmt]) extends Stmt {
  override def toString: String = s"if $cond then $body1 else $body2 fi"
  var branch1Vars: List[SymbolTableObj] = List.empty
  var branch2Vars: List[SymbolTableObj] = List.empty
}
final case class While(cond: Expr, body: List[Stmt]) extends Stmt with ScopedBody {
  override def toString: String = s"while $cond do $body done"
}
final case class ScopedStmt(stmt: List[Stmt]) extends Stmt with ScopedBody {
  override def toString: String = s"begin $stmt end"
}

// <rvalue> Right hand side of declaration or assignment
sealed trait RVal extends SymbolTableObj
final case class ArrayLiter(elems: List[Expr]) extends RVal {
  override def toString: String = elems.mkString("[", ", ", "]")
}
final case class NewPair(fst: Expr, snd: Expr) extends RVal {
  override def toString: String = s"newpair($fst, $snd)"
}
final case class Call(ident: Ident, args: List[Expr]) extends RVal {
  override def toString: String = s"call $ident(${args.mkString(", ")})"
}

// <lvalue> Left hand side of declaration or assignment
sealed trait LVal extends RVal
final case class Fst(value: LVal) extends LVal {
  override def toString: String = s"fst $value"
}
final case class Snd(value: LVal) extends LVal {
  override def toString: String = s"snd $value"
}

sealed trait Expr extends RVal
final case class UnaryApp(op: UnaryOp, expr: Expr) extends Expr {
  override def toString: String = s"$op $expr"
}
final case class BinaryApp(op: BinaryOp, left: Expr, right: Expr) extends Expr {
  override def toString: String = s"$left $op $right"
}

// <atom>
final case class Integer(i: Int) extends Expr {
  override def toString: String = i.toString
}
final case class Bool(value: Boolean) extends Expr {
  override def toString: String = value.toString
}
final case class Character(c: Char) extends Expr {
  override def toString: String = s"'$c'"
}
final case class StringAtom(s: String) extends Expr {
  override def toString: String = s
}
case object Null extends Expr { override def toString = "null" } // Null pair
final case class Ident(name: String) extends Expr with LVal { override def toString: String = name }
final case class ArrayElem(ident: Ident, exprs: List[Expr]) extends Expr with LVal {
  override def toString: String = s"$ident[${exprs.mkString("][")}]"
}
final case class BracketedExpr(expr: Expr) extends Expr {
  override def toString: String = s"($expr)"
}

// <unary-oper> Unary operator types
trait UnaryOp extends generic.ParserBridge1[Expr, UnaryApp] {
  override def apply(e: Expr): UnaryApp = UnaryApp(this, e)
}
case object Not extends UnaryOp { override def toString = "!" }
case object Neg extends UnaryOp { override def toString = "-" }
case object Len extends UnaryOp { override def toString = "len" }
case object Ord extends UnaryOp { override def toString = "ord" }
case object Chr extends UnaryOp { override def toString = "chr" }

trait Comparison

// <binary-oper> Binary operator types
trait BinaryOp extends generic.ParserBridge2[Expr, Expr, BinaryApp] {
  override def apply(l: Expr, r: Expr): BinaryApp = BinaryApp(this, l, r)
}
case object Mul extends BinaryOp { override def toString = "*" }
case object Div extends BinaryOp { override def toString = "/" }
case object Mod extends BinaryOp { override def toString = "%" }
case object Add extends BinaryOp { override def toString = "+" }
case object Sub extends BinaryOp { override def toString = "-" }
case object Gt extends BinaryOp with Comparison { override def toString = ">" }
case object GtEq extends BinaryOp with Comparison { override def toString = ">=" }
case object Lt extends BinaryOp with Comparison { override def toString = "<" }
case object LtEq extends BinaryOp with Comparison { override def toString = "<=" }
case object Eq extends BinaryOp with Comparison { override def toString = "==" }
case object NotEq extends BinaryOp with Comparison { override def toString = "!=" }
case object And extends BinaryOp { override def toString = "&&" }
case object Or extends BinaryOp { override def toString = "||" }

// Parser bridges used in the parser

object Program extends generic.ParserBridge2[List[Func], List[Stmt], Program]
object Func extends generic.ParserBridge4[Type, Ident, List[Param], List[Stmt], Func]
object Param extends generic.ParserBridge2[Type, Ident, Param]

object PairType extends generic.ParserBridge2[PairElemType, PairElemType, PairType]

object Decl extends generic.ParserBridge3[Type, Ident, RVal, Stmt]
object Asgn extends generic.ParserBridge2[LVal, RVal, Stmt]
object Read extends generic.ParserBridge1[LVal, Stmt]
object Free extends generic.ParserBridge1[Expr, Stmt]
object Return extends generic.ParserBridge1[Expr, Stmt]
object Exit extends generic.ParserBridge1[Expr, Stmt]
object Print extends generic.ParserBridge1[Expr, Stmt]
object PrintLn extends generic.ParserBridge1[Expr, Stmt]
object IfStmt extends generic.ParserBridge3[Expr, List[Stmt], List[Stmt], Stmt]
object While extends generic.ParserBridge2[Expr, List[Stmt], Stmt]
object ScopedStmt extends generic.ParserBridge1[List[Stmt], Stmt]

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
