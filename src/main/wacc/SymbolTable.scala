// package src.main.wacc

// import scala.collection.mutable

// case class SymbolTable(parent: Option[SymbolTable]) {
//   private val table: mutable.Map[String, SymbolTableValue] = mutable.HashMap()

//   def update(ident: Ident, obj: Identifier): Unit = table += ident -> obj

//   // Checks for the identifier in the current scope and all parent scopes
//   def apply(ident: Ident): Option[Identifier] =
//     table.get(ident) orElse parent.flatMap(_.apply(ident))

//   // Only checks within the current scope; apply may return a value even if contains returns false
//   def contains(ident: Ident): Boolean = table.contains(ident)

//   val makeChild: SymbolTable = SymbolTable(Some(this))
// }
