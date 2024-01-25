package src.main.wacc

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.descriptions._
import parsley.token.symbol.ImplicitSymbol

object lexer {
    private val desc = LexicalDesc.plain.copy(
      nameDesc = NameDesc.plain.copy(
        identifierStart = predicate.Basic(_.isLetter),
        identifierLetter = predicate.Basic(_.isLetterOrDigit),
      ),
      spaceDesc = SpaceDesc.plain,
    )
    private val lexer = new Lexer(desc)

    val ident: Parsley[Ident] = lexer.lexeme.names.identifier.map(Ident)
    val integer: Parsley[BigInt] = lexer.lexeme.integer.decimal
    val implicits: ImplicitSymbol = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
