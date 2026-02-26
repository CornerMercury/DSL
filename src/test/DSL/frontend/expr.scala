package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}
import DSL.frontend.AST._
import DSL.frontend.parser

class DiceParserSpec extends AnyFlatSpec {

  /**
   * Helper function to check parsing results.
   * 
   * 1. Runs the parser.
   * 2. If Successful: Checks if the resulting AST matches the expected AST.
   *    If not, prints "Expected X but got Y".
   * 3. If Failed: Fails the test and prints the Parsley error message.
   */
  def assertParse(input: String, expected: AstNode): Unit = {
    parser.parse(input) match {
      case Success(actual) => 
        // shouldBe checks equality and prints a nice diff on failure
        actual shouldBe expected 
      case Failure(msg) => 
        fail(s"Parser failed to parse input: '$input'\nParse Error:\n$msg")
    }
  }

  "Integer literals" should "parse as root Sum(expr)" in {
    assertParse("42", Sum(IntLiteral(42)))
    assertParse("0", Sum(IntLiteral(0)))
  }

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

  "Dice Precedence" should "bind tighter than math" in {
    assertParse("3d6 + 5", Sum(Add(Dice(IntLiteral(3), IntLiteral(6)), IntLiteral(5))))
    assertParse("2 * d20", Sum(Mul(IntLiteral(2), Dice(IntLiteral(1), IntLiteral(20)))))
  }

  "Sum" should "parse sum(expr) correctly (one Sum, bare Dice inside)" in {
    assertParse("sum(2d6)", Sum(Dice(IntLiteral(2), IntLiteral(6))))
    assertParse("sum(d20)", Sum(Dice(IntLiteral(1), IntLiteral(20))))
    assertParse("sum(1 + 2)", Sum(Add(IntLiteral(1), IntLiteral(2))))
    assertParse("sum(3d6 + 5)", Sum(Add(Dice(IntLiteral(3), IntLiteral(6)), IntLiteral(5))))
  }

  "Sum" should "parse sum expr without parens (Haskell-style)" in {
    assertParse("sum 2d6", Sum(Dice(IntLiteral(2), IntLiteral(6))))
    assertParse("sum d20", Sum(Dice(IntLiteral(1), IntLiteral(20))))
    assertParse("sum 2d6 + 5", Sum(Add(Sum(Dice(IntLiteral(2), IntLiteral(6))), IntLiteral(5))))
  }

  "Prod" should "parse prod(expr) correctly" in {
    assertParse("prod(2d6)", Prod(Dice(IntLiteral(2), IntLiteral(6))))
    assertParse("prod(d20)", Prod(Dice(IntLiteral(1), IntLiteral(20))))
    assertParse("prod(3d6 + 5)", Prod(Add(Dice(IntLiteral(3), IntLiteral(6)), IntLiteral(5))))
  }

  "Prod" should "parse prod expr without parens (Haskell-style)" in {
    assertParse("prod 2d6", Prod(Dice(IntLiteral(2), IntLiteral(6))))
    assertParse("prod d20", Prod(Dice(IntLiteral(1), IntLiteral(20))))
    assertParse("prod 2d6 + 5", Sum(Add(Prod(Dice(IntLiteral(2), IntLiteral(6))), IntLiteral(5))))
  }

  "Parentheses" should "override precedence" in {
    assertParse("(1 + 2) * 3", Sum(Mul(Add(IntLiteral(1), IntLiteral(2)), IntLiteral(3))))
    assertParse("d(4 + 4)", Sum(Dice(IntLiteral(1), Add(IntLiteral(4), IntLiteral(4)))))
    assertParse("(1 + 1)d6", Sum(Dice(Add(IntLiteral(1), IntLiteral(1)), IntLiteral(6))))
  }

  "Complex Expressions" should "parse correctly" in {
    assertParse("(3d6 + 5) * 2", Sum(Mul(Add(Dice(IntLiteral(3), IntLiteral(6)), IntLiteral(5)), IntLiteral(2))))
  }

  "Custom Distributions (Literals)" should "parse a basic mapping" in {
    assertParse("{1: 0.5, 2: 0.5}", Sum(CustomDist(Map(1 -> 0.5, 2 -> 0.5))))
  }

  it should "handle a single outcome" in {
    assertParse("{20: 1.0}", Sum(CustomDist(Map(20 -> 1.0))))
  }

  it should "handle empty distributions (due to sepBy)" in {
    assertParse("{}", Sum(CustomDist(Map.empty)))
  }

  it should "handle scientific notation if the lexer allows it" in {
    // Assuming 'double' in lexer handles 1e-1
    assertParse("{0: 0.9, 1: 1e-1}", Sum(CustomDist(Map(0 -> 0.9, 1 -> 0.1))))
  }

  "Custom Distributions with Dice" should "work as the number of sides (Prefix)" in {
    // d{4: 0.5, 6: 0.5} -> A die where sides are determined by a distribution
    val dist = CustomDist(Map(4 -> 0.5, 6 -> 0.5))
    assertParse("d{4: 0.5, 6: 0.5}", Sum(Dice(IntLiteral(1), dist)))
  }

  it should "work as the pool size (Infix)" in {
    // {1: 0.9, 2: 0.1}d6 -> Either 1d6 or 2d6 based on the distribution
    val dist = CustomDist(Map(1 -> 0.9, 2 -> 0.1))
    assertParse("{1: 0.9, 2: 0.1}d6", Sum(Dice(dist, IntLiteral(6))))
  }

  it should "allow nested dice within the distribution syntax if atoms are allowed" in {
    // Note: Your current 'entry' is 'integer <~> (":" ~> double)'. 
    // This test ensures the keys are specifically integers.
    assertParse("{1: 0.1, 10: 0.9}", Sum(CustomDist(Map(1 -> 0.1, 10 -> 0.9))))
  }

  "Custom Distributions with Keywords" should "work with sum(...) and prod(...)" in {
    val dist = CustomDist(Map(1 -> 0.5, 2 -> 0.5))
    assertParse("sum({1: 0.5, 2: 0.5})", Sum(dist))
    assertParse("prod({1: 0.5, 2: 0.5})", Prod(dist))
  }

  it should "work with sum/prod without parentheses (Haskell-style)" in {
    val dist = CustomDist(Map(1 -> 0.5, 2 -> 0.5))
    assertParse("sum {1: 0.5, 2: 0.5}", Sum(dist))
    assertParse("prod {1: 0.5, 2: 0.5}", Prod(dist))
  }

  "Custom Distributions in Math" should "respect precedence" in {
    val dist = CustomDist(Map(1 -> 0.5, 6 -> 0.5))
    // Math addition
    assertParse("{1: 0.5, 6: 0.5} + 10", Sum(Add(dist, IntLiteral(10))))
    // Multiplication
    assertParse("2 * {1: 0.5, 6: 0.5}", Sum(Mul(IntLiteral(2), dist)))
  }

  "Whitespace and Delimiters" should "be flexible" in {
    val expected = Sum(CustomDist(Map(1 -> 0.1, 2 -> 0.2)))
    assertParse("{ 1 : 0.1 , 2 : 0.2 }", expected)
    assertParse("{1:0.1,2:0.2}", expected)
  }

  "Identifiers" should "parse as identifier references" in {
    assertParse("x", Sum(Ident("x")))
    assertParse("foo_bar123", Sum(Ident("foo_bar123")))
  }

  "Assignments" should "parse a simple program with a final expression" in {
    val expected: AstNode = Program(
      List(
        Assign("x", IntLiteral(1)),
        ExprStmt(Sum(Add(Ident("x"), IntLiteral(2))))
      )
    )
    assertParse("x = 1; x + 2", expected)
  }

  it should "parse multiple assignments before the final expression" in {
    val expected: AstNode = Program(
      List(
        Assign("x", Dice(IntLiteral(1), IntLiteral(6))),
        Assign("y", IntLiteral(5)),
        ExprStmt(Sum(Add(Ident("x"), Ident("y"))))
      ),
    )
    assertParse("x = d6; y = 5; x + y", expected)
  }
}