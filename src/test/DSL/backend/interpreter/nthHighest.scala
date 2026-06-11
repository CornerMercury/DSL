package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import DSL.frontend.AST._
import DSL.backend.typeChecker
import DSL.backend.interpreter

class NthSpec extends AnyFlatSpec {

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
        
        val totalProb = dist.values.sum
        totalProb shouldBe 1.0 +- 1e-9
    }
  }

  "nthHighest" should "calculate the maximum of 2d6 (k=1)" in {
    val expr = Call("nthHighest", List(IntLiteral(1), Dice(IntLiteral(2), IntLiteral(6))))
    assertDist(expr)(
      1 -> (1.0/36.0),
      6 -> (11.0/36.0)
    )
  }

  it should "calculate the minimum of 2d6 (k=2, effectively min)" in {
    val expr = Call("nthHighest", List(IntLiteral(2), Dice(IntLiteral(2), IntLiteral(6))))
    assertDist(expr)(1 -> (11.0/36.0))
  }
  
  it should "calculate the maximum of a heterogeneous pool (1d4, 1d6)" in {
    val expr = Call("nthHighest", List(IntLiteral(1), Pool(List(Dice(IntLiteral(1), IntLiteral(4)), Dice(IntLiteral(1), IntLiteral(6))))))
    assertDist(expr)(
      4 -> (7.0/24.0),
      6 -> (4.0/24.0)
    )
  }

  "nthLowest" should "calculate the minimum of 2d6 (k=1)" in {
    val expr = Call("nthLowest", List(IntLiteral(1), Dice(IntLiteral(2), IntLiteral(6))))
    assertDist(expr)(
      1 -> (11.0/36.0),
      6 -> (1.0/36.0)
    )
  }

  it should "calculate the maximum of 2d6 (k=2, effectively max)" in {
    val expr = Call("nthLowest", List(IntLiteral(2), Dice(IntLiteral(2), IntLiteral(6))))
    assertDist(expr)(6 -> (11.0/36.0))
  }
  
  it should "calculate the minimum of a heterogeneous pool (1d4, 1d6)" in {
    val expr = Call("nthLowest", List(IntLiteral(1), Pool(List(Dice(IntLiteral(1), IntLiteral(4)), Dice(IntLiteral(1), IntLiteral(6))))))
    assertDist(expr)(
      1 -> (9.0/24.0),
      4 -> (3.0/24.0)
    )
  }
}