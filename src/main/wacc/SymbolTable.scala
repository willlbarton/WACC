package src.main.wacc

import scala.collection.mutable

class SymbolTable(parent: Option[SymbolTable]) {
  private val table: mutable.Map[Ident, Identifier] = mutable.Map()

  def update(ident: Ident, obj: Identifier): Unit = table += ident -> obj
  def apply(ident: Ident): Option[Identifier] =
    table.get(ident) orElse parent.flatMap(_.apply(ident))
  // Only checks within the current scope; apply may return a value even if contains returns false
  def contains(ident: Ident): Boolean = table.contains(ident)
}

object SymbolTable extends SymbolTable(None) {
  def apply(parent: SymbolTable) = new SymbolTable(Some(parent))
}

sealed trait Identifier
case class FuncI(t: Type, params:List[ParamI]) extends Identifier
case class ParamI(t: Type) extends Identifier
case class VarI(t: Type) extends Identifier
