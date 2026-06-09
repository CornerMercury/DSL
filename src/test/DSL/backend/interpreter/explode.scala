package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import DSL.frontend.AST._
import DSL.backend.typeChecker
import DSL.backend.interpreter

class ExplodeSpec extends AnyFlatSpec {

  /**
   * Helper to wrap a single expression into a Program, interpret it,
   * and verify the resulting distribution.
   */
  def assertDist(expr: Expr)(expected: (Int, Double)*): Unit = {
    val prog = Program(List(Right(expr)))
    typeChecker.check(prog) match {
      case Left(errs) => fail(s"Type errors: $errs")
      case Right(typedProg) =>
        val dists = interpreter.interpretProgram(typedProg)
        val dist = dists.head
        val expMap = expected.toMap

        for ((k, p) <- expMap) {
          dist.get(k) match {
            case Some(actual) => actual shouldBe p +- 1e-9
            case None => fail(s"Expected key $k not found in distribution. Keys were: ${dist.keySet.toSeq.sorted.mkString(", ")}")
          }
        }
        
        // Verify total probability is 1.0 (conservation of probability)
        val totalProb = dist.values.sum
        totalProb shouldBe 1.0 +- 1e-9
    }
  }

  "explodeN" should "be equivalent to identity when maxRolls is 1" in {
    val expr = Call("explodeN", List(IntLiteral(1), Dice(IntLiteral(1), IntLiteral(6))))
    // 1d6 is just a standard d6
    assertDist(expr)(
      1 -> (1.0/6.0),
      6 -> (1.0/6.0)
    )
  }

  it should "return 0 with probability 1.0 when maxRolls is 0" in {
    val expr = Call("explodeN", List(IntLiteral(0), Dice(IntLiteral(1), IntLiteral(6))))
    assertDist(expr)(
      0 -> 1.0
    )
  }

  it should "correctly calculate exploding d6 with maxRolls=2" in {
    val expr = Call("explodeN", List(IntLiteral(2), Dice(IntLiteral(1), IntLiteral(6))))
    assertDist(expr)(
      1 -> (1.0/6.0),
      7 -> (1.0/36.0),
      12 -> (1.0/36.0)
    )
  }

  it should "correctly calculate exploding d6 with maxRolls=3" in {
    val expr = Call("explodeN", List(IntLiteral(3), Dice(IntLiteral(1), IntLiteral(6))))
    assertDist(expr)(
      1 -> (1.0/6.0),
      13 -> (1.0/216.0),
      18 -> (1.0/216.0)
    )
  }

  it should "handle non-uniform distributions correctly" in {
    val customDie = CustomDist(Map(1 -> (1.0/3.0), 4 -> (2.0/3.0)))
    val expr = Call("explodeN", List(IntLiteral(3), customDie))
    
    assertDist(expr)(
      1 -> (1.0/3.0),
      5 -> (2.0/9.0),          // 2/3 * 1/3
      9 -> (4.0/27.0),         // (2/3)^2 * 1/3
      12 -> (8.0/27.0)         // (2/3)^3
    )
  }

  it should "handle a die that always rolls max" in {
    // Die: {6: 1.0}
    // Explode 3 times -> 6 + 6 + 6 = 18
    val alwaysMax = CustomDist(Map(6 -> 1.0))
    val expr = Call("explodeN", List(IntLiteral(3), alwaysMax))
    
    assertDist(expr)(
      18 -> 1.0
    )
  }

  it should "work with a pool of one die" in {
    // Testing that the asDist helper correctly unwraps a pool
    val expr = Call("explodeN", List(IntLiteral(2), Pool(List(Dice(IntLiteral(1), IntLiteral(4))))))
    // d4, explode twice
    // 1-3: 1/4
    // 5-7: 1/16
    // 8: 1/16
    assertDist(expr)(
      1 -> (1.0/4.0),
      5 -> (1.0/16.0),
      8 -> (1.0/16.0)
    )
  }
}