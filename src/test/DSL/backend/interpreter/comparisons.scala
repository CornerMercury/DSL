package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import DSL.frontend.AST._
import DSL.backend.typeChecker
import DSL.backend.interpreter

class ComparisonSpec extends AnyFlatSpec {

  def assertDist(expr: Expr)(expected: (Int, Double)*): Unit = {
    val prog = Program(List(Right(expr)))
    
    typeChecker.check(prog) match {
      case Left(errs) => fail(s"Type errors found: $errs")
      case Right(typedProg) =>
        val dists = interpreter.interpretProgram(typedProg)
        val dist = dists.head
        val expMap = expected.toMap

        dist.keySet shouldEqual expMap.keySet
        for ((k, p) <- expMap) {
          dist(k) shouldBe p +- 1e-9
        }
    }
  }

  "Scalar Comparisons" should "evaluate deterministic comparisons to 0 or 1" in {
    // 5 > 3 is true
    assertDist(Gt(IntLiteral(5), IntLiteral(3)))(1 -> 1.0)
    
    // 3 > 5 is false
    assertDist(Gt(IntLiteral(3), IntLiteral(5)))(0 -> 1.0)
    
    // 5 == 5 is true
    assertDist(Eq(IntLiteral(5), IntLiteral(5)))(1 -> 1.0)
  }

  "Distribution vs Scalar Comparisons" should "calculate Bernoulli probabilities correctly" in {
    // 1d6 > 3
    // Successes: 4, 5, 6 (3 outcomes). Total 6. P = 0.5
    assertDist(Gt(Dice(IntLiteral(1), IntLiteral(6)), IntLiteral(3)))(
      0 -> 0.5, 1 -> 0.5
    )

    // 1d6 >= 6
    // Successes: 6 (1 outcome). Total 6. P = 1/6
    assertDist(Ge(Dice(IntLiteral(1), IntLiteral(6)), IntLiteral(6)))(
      0 -> (5.0/6.0), 1 -> (1.0/6.0)
    )

    // 1d6 < 2
    // Successes: 1 (1 outcome). Total 6. P = 1/6
    assertDist(Lt(Dice(IntLiteral(1), IntLiteral(6)), IntLiteral(2)))(
      0 -> (5.0/6.0), 1 -> (1.0/6.0)
    )

    // 1d6 <= 1
    // Successes: 1 (1 outcome). Total 6. P = 1/6
    assertDist(Le(Dice(IntLiteral(1), IntLiteral(6)), IntLiteral(1)))(
      0 -> (5.0/6.0), 1 -> (1.0/6.0)
    )
  }

  "Distribution vs Distribution Comparisons" should "handle Uniform vs Uniform symmetry" in {
    // 1d6 > 1d6
    // Total outcomes: 36.
    // Ties: 6 (1,1 to 6,6).
    // Wins = (36 - 6) / 2 = 15.
    // P(Win) = 15/36 = 5/12 ≈ 0.416666
    // P(Lose/Tie) = 21/36 = 7/12 ≈ 0.583333
    assertDist(Gt(Dice(IntLiteral(1), IntLiteral(6)), Dice(IntLiteral(1), IntLiteral(6))))(
      0 -> (21.0/36.0), 1 -> (15.0/36.0)
    )
  }

  it should "calculate equality of two distributions" in {
    // 1d6 == 1d6
    // Ties: 6 outcomes. P = 6/36 = 1/6
    assertDist(Eq(Dice(IntLiteral(1), IntLiteral(6)), Dice(IntLiteral(1), IntLiteral(6))))(
      0 -> (30.0/36.0), 1 -> (6.0/36.0)
    )
  }

  "Complex Expressions" should "handle comparisons on convoluted distributions" in {
    // (1d6 + 1d6) > 10
    // 2d6 distribution: 2(1), 3(2), 4(3), 5(4), 6(5), 7(6), 8(5), 9(4), 10(3), 11(2), 12(1)
    // > 10 means 11 or 12.
    // Count(11) = 2, Count(12) = 1. Total 3/36 = 1/12
    assertDist(Gt(Add(Dice(IntLiteral(1), IntLiteral(6)), Dice(IntLiteral(1), IntLiteral(6))), IntLiteral(10)))(
      0 -> (33.0/36.0), 1 -> (3.0/36.0)
    )
  }

  it should "handle nested comparisons" in {
    // (1d6 > 3) == 1
    // 1d6 > 3 is Bernoulli(0.5).
    // Comparing Bernoulli(0.5) to scalar 1.
    // P(Bernoulli == 1) = P(Outcome is 1) = 0.5
    assertDist(Eq(Gt(Dice(IntLiteral(1), IntLiteral(6)), IntLiteral(3)), IntLiteral(1)))(
      0 -> 0.5, 1 -> 0.5
    )
  }

  it should "support comparisons with variables" in {
    val prog = Program(
      List(
        Left(Assign("x", Dice(IntLiteral(1), IntLiteral(6)))),
        Right(Gt(Ident("x"), IntLiteral(3)))
      )
    )

    typeChecker.check(prog) match {
      case Left(errs) => fail(errs.toString)
      case Right(typedProg) =>
        val dists = interpreter.interpretProgram(typedProg)
        val d = dists.head
        d shouldEqual Map(0 -> 0.5, 1 -> 0.5)
    }
  }
}