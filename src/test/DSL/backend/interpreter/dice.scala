package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import DSL.frontend.AST._
import DSL.backend.interpreter

class InterpreterSpec extends AnyFlatSpec {

  def assertDist(expr: Expr)(expected: (Int, Double)*): Unit = {
    val dist = interpreter.interpret(expr)
    val expMap = expected.toMap
    dist.keySet shouldEqual expMap.keySet
    for ((k, p) <- expMap) {
      dist(k) shouldBe p +- 1e-9
    }
  }

  "Interpreter" should "reject non-Sum/Prod root" in {
    val err = the[IllegalArgumentException] thrownBy interpreter.interpret(Dice(IntLiteral(1), IntLiteral(6)))
    err.getMessage should include("Sum or Prod")
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
    val d = interpreter.interpret(Sum(Dice(IntLiteral(1), IntLiteral(6))))
    d.size shouldBe 6
    d.values.foreach(_ shouldBe (1.0 / 6) +- 1e-9)
    d.keySet shouldEqual (1 to 6).toSet
  }

  it should "interpret 2d6 with correct sum distribution" in {
    val d = interpreter.interpret(Sum(Dice(IntLiteral(2), IntLiteral(6))))
    d(2) shouldBe (1.0 / 36) +- 1e-9   // 1+1
    d(7) shouldBe (6.0 / 36) +- 1e-9   // peak
    d(12) shouldBe (1.0 / 36) +- 1e-9  // 6+6
    d.values.sum shouldBe 1.0 +- 1e-9
  }

  it should "interpret Add as convolution" in {
    assertDist(Sum(Add(IntLiteral(1), IntLiteral(2))))(3 -> 1.0)
    val d = interpreter.interpret(Sum(Add(Dice(IntLiteral(1), IntLiteral(1)), Dice(IntLiteral(1), IntLiteral(1)))))
    d shouldEqual Map(2 -> 1.0)
  }

  it should "interpret mixed expression 2d6 + 5" in {
    val expr = Sum(Add(Dice(IntLiteral(2), IntLiteral(6)), IntLiteral(5)))
    val d = interpreter.interpret(expr)
    d.keySet shouldEqual (7 to 17).toSet  // 2+5 .. 12+5
    d(7) shouldBe (1.0 / 36) +- 1e-9
    d.values.sum shouldBe 1.0 +- 1e-9
  }

  "Prod" should "interpret 1d6 as uniform 1..6 (product of one die)" in {
    val d = interpreter.interpret(Prod(Dice(IntLiteral(1), IntLiteral(6))))
    d.size shouldBe 6
    d.values.foreach(_ shouldBe (1.0 / 6) +- 1e-9)
    d.keySet shouldEqual (1 to 6).toSet
  }

  it should "interpret 2d6 as product distribution (1*1..6*6)" in {
    val d = interpreter.interpret(Prod(Dice(IntLiteral(2), IntLiteral(6))))
    d.keySet shouldEqual Set(1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 30, 36)
    d(1) shouldBe (1.0 / 36) +- 1e-9   // 1*1
    d(36) shouldBe (1.0 / 36) +- 1e-9  // 6*6
    d(6) shouldBe (4.0 / 36) +- 1e-9   // 1*6, 2*3, 3*2, 6*1
    d.values.sum shouldBe 1.0 +- 1e-9
  }

  it should "interpret prod(2d6) * 2 as scaling product distribution" in {
    val d = interpreter.interpret(Prod(Mul(Dice(IntLiteral(2), IntLiteral(6)), IntLiteral(2))))
    d.values.sum shouldBe 1.0 +- 1e-9
    d.keySet shouldEqual Set(2, 4, 6, 8, 10, 12, 16, 18, 20, 24, 30, 32, 36, 40, 48, 50, 60, 72)
  }

  it should "interpret nested sum(prod(2d6)) as product then identity" in {
    val prodOnly = interpreter.interpret(Prod(Dice(IntLiteral(2), IntLiteral(6))))
    val viaSum = interpreter.interpret(Sum(Prod(Dice(IntLiteral(2), IntLiteral(6)))))
    viaSum shouldEqual prodOnly
  }
}
