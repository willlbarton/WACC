package src.main.wacc

import parsley.Parsley
import parsley.token.{Lexer, predicate}
import parsley.token.descriptions._
import parsley.token.descriptions.text.{EscapeDesc, TextDesc}
import parsley.token.predicate.Basic
import parsley.token.symbol.ImplicitSymbol

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

  val ident: Parsley[Var] = Var(lexer.lexeme.names.identifier)
  val integer: Parsley[Int] = lexer.lexeme.integer.decimal32
  val character: Parsley[Char] = lexer.lexeme.character.ascii
  val string: Parsley[String] = lexer.lexeme.string.ascii
  val implicits: ImplicitSymbol = lexer.lexeme.symbol.implicits
  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
