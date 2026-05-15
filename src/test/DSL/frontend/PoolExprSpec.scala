package DSL

import DSL.frontend.AST._

class PoolExprSpec extends ParserSpecHelper {
  "Pool List Syntax" should "parse as Sum(Pool(...)) at top level" in {
    assertParseExpr(
      "[d6, d8]", 
      Sum(Pool(List(
        Dice(IntLiteral(1), IntLiteral(6)), 
        Dice(IntLiteral(1), IntLiteral(8))
      )))
    )
    
    assertParseExpr(
      "[3d6, 2d4, d10]", 
      Sum(Pool(List(
        Dice(IntLiteral(3), IntLiteral(6)), 
        Dice(IntLiteral(2), IntLiteral(4)), 
        Dice(IntLiteral(1), IntLiteral(10))
      )))
    )
  }

  "Pool Concatenation" should "parse as Sum(PoolConcat(...)) at top level" in {
    assertParseExpr(
      "3d6 ++ 2d5", 
      Sum(PoolConcat(
        Dice(IntLiteral(3), IntLiteral(6)), 
        Dice(IntLiteral(2), IntLiteral(5))
      ))
    )
    
    assertParseExpr(
      "d6 ++ d20", 
      Sum(PoolConcat(
        Dice(IntLiteral(1), IntLiteral(6)), 
        Dice(IntLiteral(1), IntLiteral(20))
      ))
    )
  }

  "Mixed Pool Syntax" should "parse correctly" in {
    assertParseExpr(
      "[d6] ++ d8", 
      Sum(PoolConcat(
        Pool(List(Dice(IntLiteral(1), IntLiteral(6)))), 
        Dice(IntLiteral(1), IntLiteral(8))
      ))
    )
    
    assertParseExpr(
      "d10 ++ [d4, d4]", 
      Sum(PoolConcat(
        Dice(IntLiteral(1), IntLiteral(10)), 
        Pool(List(
          Dice(IntLiteral(1), IntLiteral(4)), 
          Dice(IntLiteral(1), IntLiteral(4))
        ))
      ))
    )
  }

  "Pool Precedence" should "bind lower than multiplication" in {
    // d6 ++ 2 * d8  ->  d6 ++ (2 * d8)
    assertParseExpr(
      "d6 ++ 2 * d8", 
      Sum(PoolConcat(
        Dice(IntLiteral(1), IntLiteral(6)), 
        Mul(IntLiteral(2), Dice(IntLiteral(1), IntLiteral(8)))
      ))
    )
  }

  "Pool Precedence" should "associate left with addition/subtraction" in {
    // d6 ++ d8 + 10 -> (d6 ++ d8) + 10
    assertParseExpr(
      "d6 ++ d8 + 10", 
      Sum(Add(
        PoolConcat(Dice(IntLiteral(1), IntLiteral(6)), Dice(IntLiteral(1), IntLiteral(8))), 
        IntLiteral(10)
      ))
    )
  }

  "Pool with Custom Distributions" should "parse correctly" in {
    assertParseExpr(
      "[d{1:0.5, 2:0.5}]", 
      Sum(Pool(List(
        Dice(IntLiteral(1), CustomDist(Map(1 -> 0.5, 2 -> 0.5)))
      )))
    )
    
    assertParseExpr(
      "3d6 ++ [d{10:0.1, 20:0.9}]", 
      Sum(PoolConcat(
        Dice(IntLiteral(3), IntLiteral(6)), 
        Pool(List(Dice(IntLiteral(1), CustomDist(Map(10 -> 0.1, 20 -> 0.9)))))
      ))
    )
  }
  
  "Empty Pool" should "parse correctly" in {
    assertParseExpr(
      "[]", 
      Sum(Pool(List()))
    )
  }
}