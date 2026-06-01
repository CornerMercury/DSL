package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import DSL.frontend.AST._
import DSL.backend.typeChecker
import DSL.backend.interpreter

class DiceInterpreterSpec extends AnyFlatSpec {

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
        
        dist.keySet shouldEqual expMap.keySet
        for ((k, p) <- expMap) {
          dist(k) shouldBe p +- 1e-9
        }
    }
  }

  "Interpreter with variables" should "evaluate assignments and identifier references" in {
    val prog = Program(
      List(
        Left(Assign("x", IntLiteral(1))),
        Right(Sum(Add(Ident("x"), IntLiteral(2))))
      )
    )

    typeChecker.check(prog) match {
      case Left(errs) => fail(errs.toString)
      case Right(typedProg) =>
        val dists = interpreter.interpretProgram(typedProg)
        dists should have length 1
        dists.head shouldEqual Map(3 -> 1.0)
    }
  }

  it should "support dice and multiple assignments" in {
    val prog = Program(
      List(
        Left(Assign("x", Dice(IntLiteral(1), IntLiteral(6)))),
        Left(Assign("y", IntLiteral(5))),
        Right(Sum(Add(Ident("x"), Ident("y"))))
      )
    )

    typeChecker.check(prog) match {
      case Left(errs) => fail(errs.toString)
      case Right(typedProg) =>
        val dists = interpreter.interpretProgram(typedProg)
        dists should have length 1
        val d = dists.head
        d.keySet shouldEqual (6 to 11).toSet
        d.values.sum shouldBe 1.0 +- 1e-9
    }
  }

  it should "produce one distribution per expression statement" in {
    val prog = Program(
      List(
        Right(Sum(IntLiteral(1))),
        Right(Sum(IntLiteral(2)))
      )
    )

    typeChecker.check(prog) match {
      case Left(errs) => fail(errs.toString)
      case Right(typedProg) =>
        val dists = interpreter.interpretProgram(typedProg)
        dists should have length 2
        dists(0) shouldEqual Map(1 -> 1.0)
        dists(1) shouldEqual Map(2 -> 1.0)
    }
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
    val prog = Program(List(Right(Sum(Dice(IntLiteral(2), IntLiteral(6))))))
    typeChecker.check(prog) match {
      case Left(errs) => fail(errs.toString)
      case Right(typedProg) =>
        val d = interpreter.interpretProgram(typedProg).head
        d(2) shouldBe (1.0 / 36) +- 1e-9
        d(7) shouldBe (6.0 / 36) +- 1e-9
        d(12) shouldBe (1.0 / 36) +- 1e-9
        d.values.sum shouldBe 1.0 +- 1e-9
    }
  }

  it should "interpret Add as convolution" in {
    assertDist(Sum(Add(IntLiteral(1), IntLiteral(2))))(3 -> 1.0)
    assertDist(Sum(Add(Dice(IntLiteral(1), IntLiteral(1)), Dice(IntLiteral(1), IntLiteral(1)))))(2 -> 1.0)
  }

  it should "interpret mixed expression 2d6 + 5" in {
    val prog = Program(List(Right(Sum(Add(Dice(IntLiteral(2), IntLiteral(6)), IntLiteral(5))))))
    typeChecker.check(prog) match {
      case Left(errs) => fail(errs.toString)
      case Right(typedProg) =>
        val d = interpreter.interpretProgram(typedProg).head
        d.keySet shouldEqual (7 to 17).toSet
        d(7) shouldBe (1.0 / 36) +- 1e-9
        d.values.sum shouldBe 1.0 +- 1e-9
    }
  }

  "Prod" should "interpret 1d6 as uniform 1..6 (product of one die)" in {
    assertDist(Prod(Dice(IntLiteral(1), IntLiteral(6))))(
      1 -> 1.0/6, 2 -> 1.0/6, 3 -> 1.0/6, 4 -> 1.0/6, 5 -> 1.0/6, 6 -> 1.0/6
    )
  }

  it should "interpret 2d6 as product distribution (1*1..6*6)" in {
    val prog = Program(List(Right(Prod(Dice(IntLiteral(2), IntLiteral(6))))))
    typeChecker.check(prog) match {
      case Left(errs) => fail(errs.toString)
      case Right(typedProg) =>
        val d = interpreter.interpretProgram(typedProg).head
        d.keySet shouldEqual Set(1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 30, 36)
        d(1) shouldBe (1.0 / 36) +- 1e-9
        d(36) shouldBe (1.0 / 36) +- 1e-9
        d(6) shouldBe (4.0 / 36) +- 1e-9
        d.values.sum shouldBe 1.0 +- 1e-9
    }
  }

  it should "interpret prod(2d6) * 2 as scaling product distribution" in {
    val prog = Program(List(Right(Mul(Prod(Dice(IntLiteral(2), IntLiteral(6))), IntLiteral(2)))))
    typeChecker.check(prog) match {
      case Left(errs) => fail(errs.toString)
      case Right(typedProg) =>
        val d = interpreter.interpretProgram(typedProg).head
        d.values.sum shouldBe 1.0 +- 1e-9
        d.keySet shouldEqual Set(2, 4, 6, 8, 10, 12, 16, 18, 20, 24, 30, 32, 36, 40, 48, 50, 60, 72)
    }
  }

  it should "interpret nested sum(prod(2d6)) as product then identity" in {
    val progProd = Program(List(Right(Prod(Dice(IntLiteral(2), IntLiteral(6))))))
    val progSumProd = Program(List(Right(Sum(Prod(Dice(IntLiteral(2), IntLiteral(6)))))))
    
    val pTyped = typeChecker.check(progProd).getOrElse(fail("Type check failed"))
    val spTyped = typeChecker.check(progSumProd).getOrElse(fail("Type check failed"))

    val prodOnly = interpreter.interpretProgram(pTyped).head
    val viaSum = interpreter.interpretProgram(spTyped).head
    viaSum shouldEqual prodOnly
  }
}