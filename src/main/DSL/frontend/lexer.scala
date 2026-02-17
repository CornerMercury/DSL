package DSL.frontend

import parsley.Parsley
import parsley.character.string
import parsley.token.Lexer
import parsley.token.predicate.Basic 
import parsley.token.descriptions.numeric.{NumericDesc, ExponentDesc} // Added ExponentDesc
import parsley.token.descriptions.{LexicalDesc, NameDesc, SymbolDesc, SpaceDesc}
import parsley.token.descriptions.text.TextDesc

object lexer {
    private val desc = LexicalDesc(
        NameDesc.plain.copy(
            identifierStart = Basic(c => Character.isLetter(c) || c == '_'),
            identifierLetter = Basic(c => Character.isLetterOrDigit(c) || c == '_'),
        ),
        
        SymbolDesc.plain.copy(hardKeywords = Set("sum", "prod")),

        NumericDesc.plain.copy(
            integerNumbersCanBeHexadecimal = false,
            integerNumbersCanBeOctal = false,
        ),
        
        TextDesc.plain,
        SpaceDesc.plain
    )

    private val lexer = new Lexer(desc)

    val integer = lexer.lexeme.integer.decimal32
    
    val double: Parsley[Double] = lexer.lexeme.numeric.real.decimal.map(_.toDouble)

    val sumKeyword = lexer.lexeme(string("sum"))
    val prodKeyword = lexer.lexeme(string("prod"))

    val implicits = lexer.lexeme.symbol.implicits
    
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}