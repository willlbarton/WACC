package src.main.wacc

import scala.collection.mutable

class SymbolTable(parent: Option[SymbolTable]) {
  private val table: mutable.Map[Ident, Identifier] = mutable.Map()

  def update(ident: Ident, obj: Identifier): Unit = table += ident -> obj
  def apply(ident: Ident): Option[Identifier] =
    table.get(ident) orElse parent.flatMap(_.apply(ident))
}

object SymbolTable extends SymbolTable(None) {
  def apply(parent: SymbolTable) = new SymbolTable(Some(parent))
}

sealed trait Identifier
case class FuncI(t: Type, params:List[ParamI]) extends Identifier
case class ParamI(t: Type) extends Identifier
