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
  def assertParse(input: String, expected: Expr): Unit = {
    parser.parse(input) match {
      case Success(actual) => 
        // shouldBe checks equality and prints a nice diff on failure
        actual shouldBe expected 
      case Failure(msg) => 
        fail(s"Parser failed to parse input: '$input'\nParse Error:\n$msg")
    }
  }

  "Integer literals" should "be parsed correctly" in {
    assertParse("42", IntLiteral(42))
    assertParse("0", IntLiteral(0))
  }

  "Basic Dice (Prefix)" should "parse as 1 die of N sides" in {
    assertParse("d6", Dice(IntLiteral(1), IntLiteral(6)))
    assertParse("d20", Dice(IntLiteral(1), IntLiteral(20)))
  }

  "Dice Pools (Infix)" should "parse as N dice of S sides" in {
    assertParse("3d6", Dice(IntLiteral(3), IntLiteral(6)))
    assertParse("10d100", Dice(IntLiteral(10), IntLiteral(100)))
  }

  "Basic Math" should "parse basic arithmetic" in {
    assertParse("1 + 2", Add(IntLiteral(1), IntLiteral(2)))
    assertParse("5 - 3", Sub(IntLiteral(5), IntLiteral(3)))
    assertParse("4 * 2", Mul(IntLiteral(4), IntLiteral(2)))
    assertParse("10 / 2", Div(IntLiteral(10), IntLiteral(2)))
  }

  "Precedence" should "respect BODMAS/PEMDAS logic" in {
    // 1 + 2 * 3 -> 1 + (2 * 3)
    assertParse("1 + 2 * 3", 
      Add(IntLiteral(1), Mul(IntLiteral(2), IntLiteral(3)))
    )

    // 2 * 3 + 1 -> (2 * 3) + 1
    assertParse("2 * 3 + 1", 
      Add(Mul(IntLiteral(2), IntLiteral(3)), IntLiteral(1))
    )
  }

  "Dice Precedence" should "bind tighter than math" in {
    // 3d6 + 5 -> (3d6) + 5
    assertParse("3d6 + 5", 
      Add(Dice(IntLiteral(3), IntLiteral(6)), IntLiteral(5))
    )

    // 2 * d20 -> 2 * (1d20)
    assertParse("2 * d20", 
      Mul(IntLiteral(2), Dice(IntLiteral(1), IntLiteral(20)))
    )
  }

  "Parentheses" should "override precedence" in {
    // (1 + 2) * 3
    assertParse("(1 + 2) * 3", 
      Mul(Add(IntLiteral(1), IntLiteral(2)), IntLiteral(3))
    )

    // d(4 + 4) -> 1d8
    assertParse("d(4 + 4)", 
      Dice(IntLiteral(1), Add(IntLiteral(4), IntLiteral(4)))
    )

    // (1 + 1)d6
    assertParse("(1 + 1)d6", 
      Dice(Add(IntLiteral(1), IntLiteral(1)), IntLiteral(6))
    )
  }

  "Complex Expressions" should "parse correctly" in {
    // (3d6 + 5) * 2
    assertParse("(3d6 + 5) * 2",
      Mul(
        Add(Dice(IntLiteral(3), IntLiteral(6)), IntLiteral(5)), 
        IntLiteral(2)
      )
    )
  }
}