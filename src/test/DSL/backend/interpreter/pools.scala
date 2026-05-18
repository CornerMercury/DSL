package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import DSL.frontend.AST._
import DSL.backend.interpreter

class PoolSpec extends AnyFlatSpec {

  /**
   * Helper to wrap a single expression into a Program, interpret it,
   * and verify the resulting distribution.
   * 
   * Unlike the ComparisonSpec helper, this checks only the provided 
   * expected keys. This is necessary for Pools which produce ranges of outcomes,
   * allowing us to spot-check min, max, and specific probabilities without 
   * defining the entire map.
   */
  def assertDist(expr: Expr)(expected: (Int, Double)*): Unit = {
    val prog = Program(List(Right(expr)))
    val dists = interpreter.interpretProgram(prog)
    val dist = dists.head
    val expMap = expected.toMap

    // 1. Ensure all expected keys exist
    for (k <- expMap.keys) {
      if (!dist.contains(k)) {
        // Fix: keys returns Iterable, so we convert to Seq to use sorted
        fail(s"Expected key $k missing in distribution. Keys were: ${dist.keys.toSeq.sorted.mkString(", ")}")
      }
    }

    // 2. Check probabilities
    for ((k, p) <- expMap) {
      dist(k) shouldBe p +- 1e-9
    }
  }

  "Pool Basic List Syntax" should "sum a list of dice correctly" in {
    // [d6, d6] should be equivalent to 2d6
    val d6 = Dice(IntLiteral(1), IntLiteral(6))
    val poolExpr = Sum(Pool(List(d6, d6)))

    // 2d6 distribution check
    assertDist(poolExpr)(
      2  -> 1.0/36.0,
      3  -> 2.0/36.0,
      4  -> 3.0/36.0,
      5  -> 4.0/36.0,
      6  -> 5.0/36.0,
      7  -> 6.0/36.0,
      8  -> 5.0/36.0,
      9  -> 4.0/36.0,
      10 -> 3.0/36.0,
      11 -> 2.0/36.0,
      12 -> 1.0/36.0
    )
  }

  "Pool with Different Dice" should "convolve distributions correctly" in {
    // [d4, d6] 
    // Min: 2, Max: 10
    val d4 = Dice(IntLiteral(1), IntLiteral(4))
    val d6 = Dice(IntLiteral(1), IntLiteral(6))
    val poolExpr = Sum(Pool(List(d4, d6)))

    // Checking specific probabilities for 1d4 + 1d6
    assertDist(poolExpr)(
      2 -> 1.0/24.0, // (1,1)
      3 -> 2.0/24.0, // (1,2), (2,1)
      7 -> 4.0/24.0, // (1,6), (2,5), (3,4), (4,3)
      10 -> 1.0/24.0 // (4,6)
    )
  }

  "Pool Concatenation" should "sum the left and right operands" in {
    // d6 ++ d6 (parsed as Sum(PoolConcat(...)))
    val d6 = Dice(IntLiteral(1), IntLiteral(6))
    val concatExpr = Sum(PoolConcat(d6, d6))

    // Should be identical to 2d6
    assertDist(concatExpr)(
      2  -> 1.0/36.0,
      7  -> 6.0/36.0,
      12 -> 1.0/36.0
    )
  }

  "Mixed Pool Syntax" should "handle nested pools and concatenation" in {
    // [d6] ++ d6
    // Inner: Sum(Pool(d6)) -> d6
    // Outer: Sum(PoolConcat(d6, d6)) -> 2d6
    val d6 = Dice(IntLiteral(1), IntLiteral(6))
    val innerPool = Sum(Pool(List(d6)))
    val mixedExpr = Sum(PoolConcat(innerPool, d6))

    assertDist(mixedExpr)(
      2  -> 1.0/36.0,
      7  -> 6.0/36.0,
      12 -> 1.0/36.0
    )
  }

  "Pool with Scalar Addition" should "add scalars to the distribution" in {
    // d6 ++ 5
    // Equivalent to 1d6 + 5
    val d6 = Dice(IntLiteral(1), IntLiteral(6))
    val five = IntLiteral(5)
    val concatExpr = Sum(PoolConcat(d6, five))

    // Range 6 to 11, uniform 1/6
    assertDist(concatExpr)(
      6  -> 1.0/6.0,
      7  -> 1.0/6.0,
      8  -> 1.0/6.0,
      9  -> 1.0/6.0,
      10 -> 1.0/6.0,
      11 -> 1.0/6.0
    )
  }

  "Empty Pool" should "evaluate to 0" in {
    // [] is parsed as Sum(Pool(List()))
    val emptyPoolExpr = Sum(Pool(List()))

    assertDist(emptyPoolExpr)(
      0 -> 1.0
    )
  }

  "Complex Pool Expressions" should "evaluate multi-dice pools" in {
    // [3d6, 2d4]
    // 3d6: 3..18, 2d4: 2..8. Total: 5..26
    val d6 = Dice(IntLiteral(3), IntLiteral(6))
    val d4 = Dice(IntLiteral(2), IntLiteral(4))
    val complexPool = Sum(Pool(List(d6, d4)))

    // Spot checks
    // Min: 3 (from 3d6) + 2 (from 2d4) = 5
    // P(Min) = P(3d6=3) * P(2d4=2) = (1/216) * (1/16) = 1/3456
    assertDist(complexPool)(
      5  -> 1.0/3456.0,
      26 -> 1.0/3456.0 // Max: 18 + 8 = 26, P=1/216 * 1/16
    )
  }
}