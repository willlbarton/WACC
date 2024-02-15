package src.main.wacc

import scala.collection.mutable

// Symbol table used in the semantic analyser
case class SymbolTable(parent: Option[SymbolTable]) {
  // The table is a map of identifiers to their corresponding AST node
  private val table: mutable.Map[Ident, SymbolTableObj] = mutable.HashMap()

  // Adds a new identifier to the current scope
  def put(key: Ident, obj: SymbolTableObj): Unit = table += key -> obj

  // Checks if the identifier is in the current scope or any parent scopes
  def apply(key: Ident): Option[SymbolTableObj] =
    table.get(key) orElse parent.flatMap(_.apply(key))

  // Only checks within current scope
  // Apply may still return a value if contains is false, if the identifier is in a parent scope
  def inCurrentScope(key: Ident): Boolean = table.contains(key)

  // Creates a new child scope
  def makeChild: SymbolTable = SymbolTable(Some(this))

  // Clears the current scope
  def clear(): Unit = table.clear()

  val vars: List[SymbolTableObj] = table.values.toList
}
