package DSL.frontend

import parsley.Parsley
import parsley.token.Lexer
import parsley.token.predicate.Basic 
import parsley.token.descriptions.numeric.NumericDesc
import parsley.token.descriptions.LexicalDesc
import parsley.token.descriptions.NameDesc
import parsley.token.descriptions.SymbolDesc
import parsley.token.descriptions.text.TextDesc
import parsley.token.descriptions.SpaceDesc

object lexer {
    // configure lexical description
    private val desc = LexicalDesc(
        NameDesc.plain.copy(
            identifierStart = Basic(c => Character.isLetter(c) || c == '_'),
            identifierLetter = Basic(c => Character.isLetterOrDigit(c) || c == '_'),
        ),

        SymbolDesc.plain,

        NumericDesc.plain.copy(
            integerNumbersCanBeHexadecimal = false,
            integerNumbersCanBeOctal = false,
        ),
        
        // Fix 3: You must include TextDesc (the 4th argument)
        TextDesc.plain,

        SpaceDesc.plain
    )

    // lexer instance
    private val lexer = new Lexer(desc)

    // basic token type parsers
    val integer = lexer.lexeme.integer.decimal32

    // symbols and whitespace parsers
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)
}