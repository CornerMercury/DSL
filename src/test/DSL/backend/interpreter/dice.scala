package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import DSL.frontend.AST._
import DSL.backend.interpreter

class DiceInterpreterSpec extends AnyFlatSpec {

  /**
   * Helper to wrap a single expression into a Program, interpret it,
   * and verify the resulting distribution.
   */
  def assertDist(expr: Expr)(expected: (Int, Double)*): Unit = {
    val prog = Program(List(ExprStmt(expr)))
    val dists = interpreter.interpretProgram(prog)
    val dist = dists.head
    val expMap = expected.toMap
    
    dist.keySet shouldEqual expMap.keySet
    for ((k, p) <- expMap) {
      dist(k) shouldBe p +- 1e-9
    }
  }

  "Interpreter with variables" should "evaluate assignments and identifier references" in {
    val prog = Program(
      List(
        Assign("x", IntLiteral(1)),
        ExprStmt(Sum(Add(Ident("x"), IntLiteral(2))))
      )
    )

    val dists = interpreter.interpretProgram(prog)
    dists should have length 1
    dists.head shouldEqual Map(3 -> 1.0)
  }

  it should "support dice and multiple assignments" in {
    val prog = Program(
      List(
        Assign("x", Dice(IntLiteral(1), IntLiteral(6))),
        Assign("y", IntLiteral(5)),
        ExprStmt(Sum(Add(Ident("x"), Ident("y"))))
      )
    )

    val dists = interpreter.interpretProgram(prog)
    dists should have length 1
    val d = dists.head
    d.keySet shouldEqual (6 to 11).toSet
    d.values.sum shouldBe 1.0 +- 1e-9
  }

  it should "produce one distribution per expression statement" in {
    val prog = Program(
      List(
        ExprStmt(Sum(IntLiteral(1))),
        ExprStmt(Sum(IntLiteral(2)))
      )
    )

    val dists = interpreter.interpretProgram(prog)
    dists should have length 2
    dists(0) shouldEqual Map(1 -> 1.0)
    dists(1) shouldEqual Map(2 -> 1.0)
  }

  it should "fail on use of unbound identifiers" in {
    val prog = Program(
      List(
        ExprStmt(Sum(Ident("x")))
      )
    )

    val err = the[IllegalArgumentException] thrownBy interpreter.interpretProgram(prog)
    err.getMessage should include("Unbound identifier")
  }

  "Sum" should "interpret IntLiteral as point mass" in {
    assertDist(Sum(IntLiteral(5)))(5 -> 1.0)
  }

  it should "interpret 1d6 as uniform 1..6" in {
    assertDist(Sum(Dice(IntLiteral(1), IntLiteral(6))))(
      1 -> 1.0/6, 2 -> 1.0/6, 3 -> 1.0/6, 4 -> 1.0/6, 5 -> 1.0/6, 6 -> 1.0/6
    )
  }

  it should "interpret 2d6 with correct sum distribution" in {
    val prog = Program(List(ExprStmt(Sum(Dice(IntLiteral(2), IntLiteral(6))))))
    val d = interpreter.interpretProgram(prog).head
    d(2) shouldBe (1.0 / 36) +- 1e-9   // 1+1
    d(7) shouldBe (6.0 / 36) +- 1e-9   // peak
    d(12) shouldBe (1.0 / 36) +- 1e-9  // 6+6
    d.values.sum shouldBe 1.0 +- 1e-9
  }

  it should "interpret Add as convolution" in {
    assertDist(Sum(Add(IntLiteral(1), IntLiteral(2))))(3 -> 1.0)
    assertDist(Sum(Add(Dice(IntLiteral(1), IntLiteral(1)), Dice(IntLiteral(1), IntLiteral(1)))))(2 -> 1.0)
  }

  it should "interpret mixed expression 2d6 + 5" in {
    val prog = Program(List(ExprStmt(Sum(Add(Dice(IntLiteral(2), IntLiteral(6)), IntLiteral(5))))))
    val d = interpreter.interpretProgram(prog).head
    d.keySet shouldEqual (7 to 17).toSet  // 2+5 .. 12+5
    d(7) shouldBe (1.0 / 36) +- 1e-9
    d.values.sum shouldBe 1.0 +- 1e-9
  }

  "Prod" should "interpret 1d6 as uniform 1..6 (product of one die)" in {
    assertDist(Prod(Dice(IntLiteral(1), IntLiteral(6))))(
      1 -> 1.0/6, 2 -> 1.0/6, 3 -> 1.0/6, 4 -> 1.0/6, 5 -> 1.0/6, 6 -> 1.0/6
    )
  }

  it should "interpret 2d6 as product distribution (1*1..6*6)" in {
    val prog = Program(List(ExprStmt(Prod(Dice(IntLiteral(2), IntLiteral(6))))))
    val d = interpreter.interpretProgram(prog).head
    d.keySet shouldEqual Set(1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 30, 36)
    d(1) shouldBe (1.0 / 36) +- 1e-9   // 1*1
    d(36) shouldBe (1.0 / 36) +- 1e-9  // 6*6
    d(6) shouldBe (4.0 / 36) +- 1e-9   // 1*6, 2*3, 3*2, 6*1
    d.values.sum shouldBe 1.0 +- 1e-9
  }

  it should "interpret prod(2d6) * 2 as scaling product distribution" in {
    val prog = Program(List(ExprStmt(Prod(Mul(Dice(IntLiteral(2), IntLiteral(6)), IntLiteral(2))))))
    val d = interpreter.interpretProgram(prog).head
    d.values.sum shouldBe 1.0 +- 1e-9
    d.keySet shouldEqual Set(2, 4, 6, 8, 10, 12, 16, 18, 20, 24, 30, 32, 36, 40, 48, 50, 60, 72)
  }

  it should "interpret nested sum(prod(2d6)) as product then identity" in {
    val progProd = Program(List(ExprStmt(Prod(Dice(IntLiteral(2), IntLiteral(6))))))
    val progSumProd = Program(List(ExprStmt(Sum(Prod(Dice(IntLiteral(2), IntLiteral(6)))))))
    
    val prodOnly = interpreter.interpretProgram(progProd).head
    val viaSum = interpreter.interpretProgram(progSumProd).head
    viaSum shouldEqual prodOnly
  }
}