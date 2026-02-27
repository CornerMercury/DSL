package DSL

import DSL.frontend.AST._

class LiteralsSpec extends ParserSpecHelper {

  "Integer literals" should "parse as root Sum(expr)" in {
    assertParse("42", Sum(IntLiteral(42)))
    assertParse("0", Sum(IntLiteral(0)))
  }
}
