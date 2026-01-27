package DSL.frontend

import parsley.Parsley
import parsley.Parsley.notFollowedBy
import parsley.token.{Lexer, Basic}
import parsley.token.descriptions.*

object lexer {
    // configure lexical description
    private val desc = LexicalDesc(
        NameDesc.plain.copy(
            // start with letter/underscore
            identifierStart = Basic(c => Character.isLetter(c) || c == '_'),
            // filled with letters/digits/underscores
            identifierLetter = Basic(c => Character.isLetterOrDigit(c) || c == '_'),
        ),

        SymbolDesc.plain.copy(
            // reserved keywords
            hardKeywords = Set(
                // "int", "bool", "char", "string", "pair",
                // "begin", "end", "is", "skip", "return", "exit",
                // "read", "free", "print", "println",
                // "if", "then", "else", "fi", "while", "do", "done",
                // "newpair", "fst", "snd", "call",
                // "true", "false", "null",
                // "len", "ord", "chr",
            ),
            // operators
            hardOperators = Set(
                "+", "-", "*", "/", "%",
                "==", "!=", "<", "<=", ">", ">=",
                "&&", "||",
                "!",
            ),        
        ),

        // disable hex and oct numbers
        NumericDesc.plain.copy(
            integerNumbersCanBeHexadecimal = false,
            integerNumbersCanBeOctal = false,
        ),

        SpaceDesc.plain.copy(
            lineCommentStart = "#",
        ),
    )

    // lexer instance with error configuration
    private val lexer = Lexer(desc, errConfig)

    // basic token type parsers
    val digit = parsley.character.digit
    val minus = lexer.nonlexeme.symbol("-")
    val minusExpr = lexer.lexeme(minus <~ notFollowedBy(digit))
    
    val identifier = lexer.lexeme.names.identifier
    val integer = lexer.lexeme.integer.decimal32
    val character = lexer.lexeme.character.ascii
    val string = lexer.lexeme.string.ascii
    val boolean = lexer.lexeme.symbol("true").as(true)
                | lexer.lexeme.symbol("false").as(false)

    // higher-order parsers
    def brackets[A](p: => Parsley[A]): Parsley[A] = lexer.lexeme.brackets(p)
    def parens[A](p: => Parsley[A]): Parsley[A] = lexer.lexeme.parens(p)
    def commaSep[A](p: Parsley[A]): Parsley[List[A]] = lexer.lexeme.commaSep(p)
    def semiSep1[A](p: Parsley[A]): Parsley[List[A]] = lexer.lexeme.semiSep1(p)

    // symbols and whitespace parsers
    val implicits = lexer.lexeme.symbol.implicits
    def fully[A](p: Parsley[A]): Parsley[A] = lexer.fully(p)

    // information for the error builder
    def tokensList = Seq(
        lexer.nonlexeme.names.identifier.map(v => s"identifier $v"),
        lexer.nonlexeme.integer.decimal32.map(n => s"integer $n"),
        lexer.nonlexeme.character.ascii.map(c => s"character '$c'"),
        lexer.nonlexeme.string.ascii.map(s => s"string $s"),
        parsley.character.whitespace.map(_ => "whitespace"),
    ) ++ desc.symbolDesc.hardKeywords.map { k =>
        lexer.nonlexeme.symbol(k).as(s"keyword $k")
    }
}