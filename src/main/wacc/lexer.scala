package src.main.wacc

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.descriptions._
import parsley.token.descriptions.text.{EscapeDesc, TextDesc}
import parsley.token.predicate.Unicode
import parsley.token.symbol.ImplicitSymbol

object lexer {
    private val desc = LexicalDesc.plain.copy(
      nameDesc = NameDesc.plain.copy(
        identifierStart = predicate.Basic(_.isLetter),
        identifierLetter = predicate.Basic(_.isLetterOrDigit),
      ),
      spaceDesc = SpaceDesc.plain.copy(
        lineCommentStart = "#",
      ),
      textDesc = TextDesc.plain.copy(
        escapeSequences = EscapeDesc.plain.copy(
          literals = Set('\\', "\'", "\""),
          mapping = Map(
            '0' -> '\0',
            'b' -> '\b',
            't' -> '\t',
            'n' -> '\n',
            'f' -> '\f',
            'r' -> '\r'
          ),
        ),
        graphicCharacter = Unicode(c =>
          c >= ' '.toInt && c != '\\'.toInt && c != '\"'.toInt && c != '\''.toInt
        ),
      ),
    )
    private val lexer = new Lexer(desc)

    val ident: Parsley[Ident] = Ident(lexer.lexeme.names.identifier)
    val nat: Parsley[Int] = lexer.lexeme.natural.decimal.map(_.toInt)
    val character: Parsley[Char] = lexer.lexeme.character.latin1
    val string: Parsley[String] = lexer.lexeme.string.latin1
    val implicits: ImplicitSymbol = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
