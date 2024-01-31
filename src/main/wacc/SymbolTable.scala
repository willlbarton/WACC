package src.main.wacc

import scala.collection.mutable

class SymbolTable(parent: Option[SymbolTable]) {
  private var table: mutable.Map[String, Identifier] = mutable.Map()

  def update(ident: String, obj: Identifier) = table += ident -> obj
  def apply(ident: String): Option[Identifier] = table.get(ident) orElse parent.flatMap(_.apply(ident))
}

object SymbolTable extends SymbolTable(None) {
  def apply(parent: SymbolTable) = new SymbolTable(Some(parent))
}

sealed trait Identifier
// identifier objects
