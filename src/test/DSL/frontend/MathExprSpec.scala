package DSL

import DSL.frontend.AST._

class MathExprSpec extends ParserSpecHelper {

  "Basic Math" should "parse as root Sum(expr)" in {
    assertParse("1 + 2", Sum(Add(IntLiteral(1), IntLiteral(2))))
    assertParse("5 - 3", Sum(Sub(IntLiteral(5), IntLiteral(3))))
    assertParse("4 * 2", Sum(Mul(IntLiteral(4), IntLiteral(2))))
    assertParse("10 / 2", Sum(Div(IntLiteral(10), IntLiteral(2))))
  }

  "Precedence" should "respect BODMAS/PEMDAS logic" in {
    assertParse("1 + 2 * 3", Sum(Add(IntLiteral(1), Mul(IntLiteral(2), IntLiteral(3)))))
    assertParse("2 * 3 + 1", Sum(Add(Mul(IntLiteral(2), IntLiteral(3)), IntLiteral(1))))
  }

  "Parentheses" should "override precedence" in {
    assertParse("(1 + 2) * 3", Sum(Mul(Add(IntLiteral(1), IntLiteral(2)), IntLiteral(3))))
    assertParse("d(4 + 4)", Sum(Dice(IntLiteral(1), Add(IntLiteral(4), IntLiteral(4)))))
    assertParse("(1 + 1)d6", Sum(Dice(Add(IntLiteral(1), IntLiteral(1)), IntLiteral(6))))
  }

  "Complex Expressions" should "parse correctly" in {
    assertParse("(3d6 + 5) * 2", Sum(Mul(Add(Dice(IntLiteral(3), IntLiteral(6)), IntLiteral(5)), IntLiteral(2))))
  }
}
