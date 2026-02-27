package DSL

import DSL.frontend.AST._

class DiceExprSpec extends ParserSpecHelper {

  "Basic Dice (Prefix)" should "parse as Sum(Dice(1, N))" in {
    assertParse("d6", Sum(Dice(IntLiteral(1), IntLiteral(6))))
    assertParse("d20", Sum(Dice(IntLiteral(1), IntLiteral(20))))
  }

  "Dice Pools (Infix)" should "parse as Sum(Dice(N, S))" in {
    assertParse("3d6", Sum(Dice(IntLiteral(3), IntLiteral(6))))
    assertParse("10d100", Sum(Dice(IntLiteral(10), IntLiteral(100))))
  }

  "d6 + 7" should "parse as Sum(Add(Dice(1,6), 7))" in {
    assertParse("d6 + 7", Sum(Add(Dice(IntLiteral(1), IntLiteral(6)), IntLiteral(7))))
  }

  "Dice Precedence" should "bind tighter than math" in {
    assertParse("3d6 + 5", Sum(Add(Dice(IntLiteral(3), IntLiteral(6)), IntLiteral(5))))
    assertParse("2 * d20", Sum(Mul(IntLiteral(2), Dice(IntLiteral(1), IntLiteral(20)))))
  }
}
