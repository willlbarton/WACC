package src.main.wacc

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.descriptions._
import parsley.token.descriptions.text.{EscapeDesc, TextDesc}
import parsley.token.predicate.Basic
import parsley.token.symbol.ImplicitSymbol
import parsley.token.descriptions.numeric.PlusSignPresence
import parsley.token.descriptions.numeric.NumericDesc

object lexer {
  private val desc = LexicalDesc.plain.copy(
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic(x => x.isLetter || x == '_'),
      identifierLetter = predicate.Basic(x => x.isLetterOrDigit || x == '_')
    ),
    spaceDesc = SpaceDesc.plain.copy(
      lineCommentStart = "#"
    ),
    textDesc = TextDesc.plain.copy(
      escapeSequences = EscapeDesc.plain.copy(
        literals = Set('\\', '\'', '\"'),
        mapping = Map(
          "0" -> '\u0000'.toInt,
          "b" -> '\b'.toInt,
          "t" -> '\t'.toInt,
          "n" -> '\n'.toInt,
          "f" -> '\f'.toInt,
          "r" -> '\r'.toInt
        )
      ),
      graphicCharacter = Basic(c =>
        c >= ' '.toInt && c != '\\'.toInt && c != '\"'.toInt && c != '\''.toInt && c.toInt <= 127
      )
    ),
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = Set(
        "begin",
        "end",
        "is",
        "skip",
        "read",
        "free",
        "return",
        "exit",
        "print",
        "println",
        "if",
        "then",
        "else",
        "fi",
        "while",
        "do",
        "done",
        "newpair",
        "call",
        "fst",
        "snd",
        "int",
        "bool",
        "char",
        "string",
        "pair",
        "null"
      )
    )
  )
  private val lexer = new Lexer(desc)

  val ident: Parsley[Ident] = Ident(lexer.lexeme.names.identifier)
  val nat: Parsley[Int] = lexer.lexeme.integer.decimal.map(_.toInt)
  val character: Parsley[Char] = lexer.lexeme.character.latin1
  val string: Parsley[String] = lexer.lexeme.string.latin1
  val implicits: ImplicitSymbol = lexer.lexeme.symbol.implicits
  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
