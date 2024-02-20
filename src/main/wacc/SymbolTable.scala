package src.main.wacc

import scala.collection.mutable

// Symbol table used in the semantic analyser
case class SymbolTable[T](parent: Option[SymbolTable[T]]) {
  // The table is a map of identifiers to their corresponding AST node
  val table: mutable.Map[Ident, T] = mutable.HashMap()

  // Adds a new identifier to the current scope
  def put(key: Ident, obj: T): Unit = table += key -> obj

  // Checks if the identifier is in the current scope or any parent scopes
  def apply(key: Ident): Option[T] =
    table.get(key) orElse parent.flatMap(_.apply(key))

  // Only checks within current scope
  // Apply may still return a value if contains is false, if the identifier is in a parent scope
  def inCurrentScope(key: Ident): Boolean = table.contains(key)

  // Creates a new child scope
  def makeChild: SymbolTable[T] = SymbolTable(Some(this))

  // Clears the current scope
  def clear(): Unit = table.clear()

  def vars: List[T] = table.values.toList
}
