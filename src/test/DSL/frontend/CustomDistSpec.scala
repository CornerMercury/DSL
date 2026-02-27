package DSL

import DSL.frontend.AST._

class CustomDistSpec extends ParserSpecHelper {

  "Custom Distributions (Literals)" should "parse a basic mapping" in {
    assertParseExpr("{1: 0.5, 2: 0.5}", Sum(CustomDist(Map(1 -> 0.5, 2 -> 0.5))))
  }

  it should "handle a single outcome" in {
    assertParseExpr("{20: 1.0}", Sum(CustomDist(Map(20 -> 1.0))))
  }

  it should "handle empty distributions (due to sepBy)" in {
    assertParseExpr("{}", Sum(CustomDist(Map.empty)))
  }

  it should "handle scientific notation if the lexer allows it" in {
    assertParseExpr("{0: 0.9, 1: 1e-1}", Sum(CustomDist(Map(0 -> 0.9, 1 -> 0.1))))
  }

  "Custom Distributions with Dice" should "work as the number of sides (Prefix)" in {
    val dist = CustomDist(Map(4 -> 0.5, 6 -> 0.5))
    assertParseExpr("d{4: 0.5, 6: 0.5}", Sum(Dice(IntLiteral(1), dist)))
  }

  it should "work as the pool size (Infix)" in {
    val dist = CustomDist(Map(1 -> 0.9, 2 -> 0.1))
    assertParseExpr("{1: 0.9, 2: 0.1}d6", Sum(Dice(dist, IntLiteral(6))))
  }

  it should "allow nested dice within the distribution syntax if atoms are allowed" in {
    assertParseExpr("{1: 0.1, 10: 0.9}", Sum(CustomDist(Map(1 -> 0.1, 10 -> 0.9))))
  }

  "Custom Distributions with Keywords" should "work with sum(...) and prod(...)" in {
    val dist = CustomDist(Map(1 -> 0.5, 2 -> 0.5))
    assertParseExpr("sum({1: 0.5, 2: 0.5})", Sum(dist))
    assertParseExpr("prod({1: 0.5, 2: 0.5})", Prod(dist))
  }

  it should "work with sum/prod without parentheses (Haskell-style)" in {
    val dist = CustomDist(Map(1 -> 0.5, 2 -> 0.5))
    assertParseExpr("sum {1: 0.5, 2: 0.5}", Sum(dist))
    assertParseExpr("prod {1: 0.5, 2: 0.5}", Prod(dist))
  }

  "Custom Distributions in Math" should "respect precedence" in {
    val dist = CustomDist(Map(1 -> 0.5, 6 -> 0.5))
    assertParseExpr("{1: 0.5, 6: 0.5} + 10", Sum(Add(dist, IntLiteral(10))))
    assertParseExpr("2 * {1: 0.5, 6: 0.5}", Sum(Mul(IntLiteral(2), dist)))
  }

  "Whitespace and Delimiters" should "be flexible" in {
    val expected = Sum(CustomDist(Map(1 -> 0.1, 2 -> 0.2)))
    assertParseExpr("{ 1 : 0.1 , 2 : 0.2 }", expected)
    assertParseExpr("{1:0.1,2:0.2}", expected)
  }
}
