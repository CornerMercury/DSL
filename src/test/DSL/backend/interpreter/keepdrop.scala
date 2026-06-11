package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import DSL.frontend.AST._
import DSL.backend.typeChecker
import DSL.backend.interpreter

class KeepDropSpec extends AnyFlatSpec {

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

  "keepHighest" should "optimize k=1 to Max" in {
    val expr = Call("keepHighest", List(IntLiteral(1), Dice(IntLiteral(2), IntLiteral(6))))
    assertDist(expr)(
      1 -> (1.0/36.0),
      2 -> (3.0/36.0),
      6 -> (11.0/36.0)
    )
  }

  it should "handle the standard 4d6 drop lowest (keep 3)" in {
    val expr = Call("keepHighest", List(IntLiteral(3), Dice(IntLiteral(4), IntLiteral(6))))
    assertDist(expr)(
      3 -> (1.0/1296.0),
      17 -> (54.0/1296.0),
      18 -> (21.0/1296.0)
    )
  }

  it should "be equivalent to sum when k equals n" in {
    val expr = Call("keepHighest", List(IntLiteral(3), Dice(IntLiteral(3), IntLiteral(6))))
    assertDist(expr)(7 -> (15.0/216.0))
  }

  "keepLowest" should "optimize k=1 to Min" in {
    val expr = Call("keepLowest", List(IntLiteral(1), Dice(IntLiteral(2), IntLiteral(6))))
    assertDist(expr)(
      1 -> (11.0/36.0),
      2 -> (9.0/36.0),
      6 -> (1.0/36.0)
    )
  }

  it should "correctly keep the smallest dice" in {
    val expr = Call("keepLowest", List(IntLiteral(3), Dice(IntLiteral(4), IntLiteral(6))))
    assertDist(expr)(
      3 -> (21.0/1296.0),
      17 -> (4.0/1296.0),
      18 -> (1.0/1296.0)
    )
  }

  "dropHighest" should "be equivalent to keepLowest" in {
    // 4d6 drop 1 == 4d6 keep 3 (smallest)
    // We verify sum 17 matches the keepLowest calculation (4/1296)
    val expr = Call("dropHighest", List(IntLiteral(1), Dice(IntLiteral(4), IntLiteral(6))))
    assertDist(expr)(17 -> (4.0/1296.0))
  }

  it should "handle drop 1 on 2d6 (keep min 1)" in {
    // 2d6 drop 1 is equivalent to Min(2d6)
    // P(6) = 1/36
    val expr = Call("dropHighest", List(IntLiteral(1), Dice(IntLiteral(2), IntLiteral(6))))
    assertDist(expr)(6 -> (1.0/36.0))
  }

  "dropLowest" should "be equivalent to keepHighest" in {
    // 4d6 drop 1 == 4d6 keep 3 (largest)
    // We verify sum 17 matches the keepHighest calculation (54/1296)
    val expr = Call("dropLowest", List(IntLiteral(1), Dice(IntLiteral(4), IntLiteral(6))))
    assertDist(expr)(17 -> (54.0/1296.0))
  }

  it should "handle drop 1 on 2d6 (keep max 1)" in {
    // 2d6 drop 1 is equivalent to Max(2d6)
    // P(1) = 1/36
    val expr = Call("dropLowest", List(IntLiteral(1), Dice(IntLiteral(2), IntLiteral(6))))
    assertDist(expr)(1 -> (1.0/36.0))
  }

  "Inhomogeneous Pools" should "keep largest 1 of 1d4 and 1d6" in {
    val expr = Call("keepHighest", List(IntLiteral(1), Pool(List(Dice(IntLiteral(1), IntLiteral(4)), Dice(IntLiteral(1), IntLiteral(6))))))
    assertDist(expr)(
      4 -> (7.0/24.0),
      6 -> (4.0/24.0)
    )
  }

  it should "keep smallest 1 of 1d6 and 1d8" in {
    val expr = Call("keepLowest", List(IntLiteral(1), Pool(List(Dice(IntLiteral(1), IntLiteral(6)), Dice(IntLiteral(1), IntLiteral(8))))))
    assertDist(expr)(
      1 -> (13.0/48.0),
      6 -> (3.0/48.0)
    )
  }

  it should "sum 1d4 and 1d6 (keep 2)" in {
    val expr = Call("keepHighest", List(IntLiteral(2), Pool(List(Dice(IntLiteral(1), IntLiteral(4)), Dice(IntLiteral(1), IntLiteral(6))))))
    assertDist(expr)(
      5 -> (4.0/24.0),
      10 -> (1.0/24.0)
    )
  }
}