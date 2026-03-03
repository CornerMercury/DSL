package DSL

import DSL.frontend.AST._

class DiceExprSpec extends ParserSpecHelper {
  "Basic Dice (Prefix)" should "parse as Sum(Dice(1, N))" in {
    assertParseExpr("d6", Sum(Dice(IntLiteral(1), IntLiteral(6))))
    assertParseExpr("d20", Sum(Dice(IntLiteral(1), IntLiteral(20))))
  }

  "Dice Pools (Infix)" should "parse as Sum(Dice(N, S))" in {
    assertParseExpr("3d6", Sum(Dice(IntLiteral(3), IntLiteral(6))))
    assertParseExpr("10d100", Sum(Dice(IntLiteral(10), IntLiteral(100))))
  }

  "d6 + 7" should "parse as Sum(Add(Dice(1,6), 7))" in {
    assertParseExpr("d6 + 7", Sum(Add(Dice(IntLiteral(1), IntLiteral(6)), IntLiteral(7))))
  }

  "Dice Precedence" should "bind tighter than math" in {
    assertParseExpr("3d6 + 5", Sum(Add(Dice(IntLiteral(3), IntLiteral(6)), IntLiteral(5))))
    assertParseExpr("2 * d20", Sum(Mul(IntLiteral(2), Dice(IntLiteral(1), IntLiteral(20)))))
  }

  "Zero Dice (0dx)" should "parse correctly with 0 count" in {
    assertParseExpr("0d6", Sum(Dice(IntLiteral(0), IntLiteral(6))))
    assertParseExpr("0d20", Sum(Dice(IntLiteral(0), IntLiteral(20))))
  }

  "Zero Sides (xd0)" should "parse correctly with 0 sides" in {
    assertParseExpr("6d0", Sum(Dice(IntLiteral(6), IntLiteral(0))))
    assertParseExpr("10d0", Sum(Dice(IntLiteral(10), IntLiteral(0))))
  }

  "Zero Count and Sides (0d0)" should "parse correctly" in {
    assertParseExpr("0d0", Sum(Dice(IntLiteral(0), IntLiteral(0))))
    assertParseExpr("d0", Sum(Dice(IntLiteral(1), IntLiteral(0)))) // Prefix d0 defaults to 1d0
  }
}