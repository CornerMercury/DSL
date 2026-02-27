package DSL

import DSL.frontend.AST._

class SumProdSpec extends ParserSpecHelper {

  "Sum" should "parse sum(expr) correctly (one Sum, bare Dice inside)" in {
    assertParseExpr("sum(2d6)", Sum(Dice(IntLiteral(2), IntLiteral(6))))
    assertParseExpr("sum(d20)", Sum(Dice(IntLiteral(1), IntLiteral(20))))
    assertParseExpr("sum(1 + 2)", Sum(Add(IntLiteral(1), IntLiteral(2))))
    assertParseExpr("sum(3d6 + 5)", Sum(Add(Dice(IntLiteral(3), IntLiteral(6)), IntLiteral(5))))
  }

  "Sum" should "parse sum expr without parens (Haskell-style)" in {
    assertParseExpr("sum 2d6", Sum(Dice(IntLiteral(2), IntLiteral(6))))
    assertParseExpr("sum d20", Sum(Dice(IntLiteral(1), IntLiteral(20))))
    assertParseExpr("sum 2d6 + 5", Sum(Add(Sum(Dice(IntLiteral(2), IntLiteral(6))), IntLiteral(5))))
  }

  "Prod" should "parse prod(expr) correctly" in {
    assertParseExpr("prod(2d6)", Prod(Dice(IntLiteral(2), IntLiteral(6))))
    assertParseExpr("prod(d20)", Prod(Dice(IntLiteral(1), IntLiteral(20))))
    assertParseExpr("prod(3d6 + 5)", Prod(Add(Dice(IntLiteral(3), IntLiteral(6)), IntLiteral(5))))
  }

  "Prod" should "parse prod expr without parens (Haskell-style)" in {
    assertParseExpr("prod 2d6", Prod(Dice(IntLiteral(2), IntLiteral(6))))
    assertParseExpr("prod d20", Prod(Dice(IntLiteral(1), IntLiteral(20))))
    assertParseExpr("prod 2d6 + 5", Sum(Add(Prod(Dice(IntLiteral(2), IntLiteral(6))), IntLiteral(5))))
  }
}
