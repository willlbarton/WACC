package src.main.wacc

import parsley.Parsley
import parsley.errors.combinator.ErrorMethods
import parsley.token.{Lexer, predicate}
import parsley.token.descriptions._
import parsley.token.errors._
import parsley.token.descriptions.text.{EscapeDesc, TextDesc}
import parsley.token.predicate.Basic
import parsley.token.symbol.ImplicitSymbol

object lexer {
  // Specifies behaviour of the lexer
  private val desc = LexicalDesc.plain.copy(
    // Describes identifiers
    nameDesc = NameDesc.plain.copy(
      identifierStart = predicate.Basic(x => x.isLetter || x == '_'),
      identifierLetter = predicate.Basic(x => x.isLetterOrDigit || x == '_')
    ),
    // Describes whitespace and comments
    spaceDesc = SpaceDesc.plain.copy(
      lineCommentStart = "#"
    ),
    // Describes legal character and string literals
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
    // Keywords
    symbolDesc = SymbolDesc.plain.copy(
      hardKeywords = Set(
        "begin", "end", "is", "skip", "read", "free", "return", "exit", "print", "println", "if",
        "then", "else", "fi", "while", "do", "done", "newpair", "call", "fst", "snd", "int", "bool",
        "char", "string", "pair", "null"
      )
    )
  )

  private val errConfig = new ErrorConfig {
    override def labelNameIdentifier: String = "variable"
    override def unexpectedNameIllegalIdentifier(v: String): String = s"Unexpected keyword: '$v'"
    override def labelSymbol: Map[String, LabelWithExplainConfig] =
      Map (
        "!" -> Label("unary operator"),
        "len " -> Label("unary operator"),
        "ord " -> Label("unary operator"),
        "chr " -> Label("unary operator"),
        "*" -> Label("binary operator"),
        "/" -> Label("binary operator"),
        "%" -> Label("binary operator"),
        "+" -> Label("binary operator"),
        "&&" -> Label("logical operator"),
        "||" -> Label("logical operator"),
        ">" -> Label("comparison operator"),
        ">=" -> Label("comparison operator"),
        "<" -> Label("comparison operator"),
        "<=" -> Label("comparison operator"),
        "==" -> Label("comparison operator"),
        "!=" -> Label("comparison operator"),
        "false" -> Label("boolean"),
        "true" -> Label("boolean"),
        "call" -> Label("function call"),
        "fst" -> Label("pair element"),
        "snd" -> Label("pair element"),
      )
  }

  private val lexer = new Lexer(desc, errConfig)

  // Parsers generated by the lexer
  val ident: Parsley[Ident] = Ident(lexer.lexeme.names.identifier)
  val integer: Parsley[Int] = lexer.lexeme.integer.decimal32.label("integer")
  val character: Parsley[Char] = lexer.lexeme.character.ascii.label("character")
  val string: Parsley[String] = lexer.lexeme.string.ascii.label("string")
  val implicits: ImplicitSymbol = lexer.lexeme.symbol.implicits
  def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}
