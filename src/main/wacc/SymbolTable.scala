package src.main.wacc

import scala.collection.mutable

case class SymbolTable(parent: Option[SymbolTable]) {
  private val table: mutable.Map[Ident, SymbolTableObj] = mutable.HashMap()

  def put(key: Ident, obj: SymbolTableObj): Unit = {
    table += key -> obj
  }

  // Checks if the identifier is in the current scope or any parent scopes
  def apply(key: Ident): Option[SymbolTableObj] =
    table.get(key) orElse parent.flatMap(_.apply(key))

  // Only checks within current scope
  // Apply may still return a value if contains is false, if the identifier is in a parent scope
  def inCurrentScope(key: Ident): Boolean = table.contains(key)

  def makeChild: SymbolTable = SymbolTable(Some(this))

  def clear(): Unit = table.clear()
}
