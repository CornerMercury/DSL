package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import DSL.frontend.AST._
import DSL.backend.interpreter
import DSL.backend.typedAST
import DSL.backend.{ScalarTy, BinomialTy, UniformTy, GenericDistTy}

class TypedInterpreterSpec extends AnyFlatSpec {

  "interpretWithTypes" should "classify sum(1d2) as Binomial (two outcomes)" in {
    val expr = Sum(Dice(IntLiteral(1), IntLiteral(2)))
    val (dist, tyAst) = interpreter.interpretWithTypes(expr)
    tyAst.ty shouldBe BinomialTy
    dist.size shouldBe 2
    dist.keySet shouldEqual Set(1, 2)
    dist.values.foreach(_ shouldBe 0.5 +- 1e-9)
  }

  it should "classify sum(1d6) as Uniform (equal probs)" in {
    val expr = Sum(Dice(IntLiteral(1), IntLiteral(6)))
    val (dist, tyAst) = interpreter.interpretWithTypes(expr)
    tyAst.ty shouldBe UniformTy
    dist.size shouldBe 6
  }

  it should "classify sum(2d6) as Generic (unequal probs)" in {
    val expr = Sum(Dice(IntLiteral(2), IntLiteral(6)))
    val (_, tyAst) = interpreter.interpretWithTypes(expr)
    tyAst.ty shouldBe GenericDistTy
  }

  it should "classify sum(IntLiteral) as Scalar (single outcome)" in {
    val expr = Sum(IntLiteral(5))
    val (dist, tyAst) = interpreter.interpretWithTypes(expr)
    tyAst.ty shouldBe ScalarTy
    dist shouldEqual Map(5 -> 1.0)
  }

  it should "classify prod(1d2) as Binomial" in {
    val expr = Prod(Dice(IntLiteral(1), IntLiteral(2)))
    val (_, tyAst) = interpreter.interpretWithTypes(expr)
    tyAst.ty shouldBe BinomialTy
  }

  it should "classify prod(1d6) as Uniform" in {
    val expr = Prod(Dice(IntLiteral(1), IntLiteral(6)))
    val (_, tyAst) = interpreter.interpretWithTypes(expr)
    tyAst.ty shouldBe UniformTy
  }

  it should "classify prod(2d6) as Generic" in {
    val expr = Prod(Dice(IntLiteral(2), IntLiteral(6)))
    val (_, tyAst) = interpreter.interpretWithTypes(expr)
    tyAst.ty shouldBe GenericDistTy
  }

  it should "reject non-Sum/Prod root" in {
    val err = the[IllegalArgumentException] thrownBy interpreter.interpretWithTypes(Dice(IntLiteral(1), IntLiteral(6)))
    err.getMessage should include("Sum or Prod")
  }

  "interpretWithTypes distribution" should "match interpret for sum(2d6+5)" in {
    val expr = Sum(Add(Dice(IntLiteral(2), IntLiteral(6)), IntLiteral(5)))
    val (dist, _) = interpreter.interpretWithTypes(expr)
    val distViaInterpret = interpreter.interpret(expr)
    dist shouldEqual distViaInterpret
    dist.keySet shouldEqual (7 to 17).toSet
    dist.values.sum shouldBe 1.0 +- 1e-9
  }

  it should "match interpret for sum(2d6)" in {
    val expr = Sum(Dice(IntLiteral(2), IntLiteral(6)))
    val (distFromTyped, _) = interpreter.interpretWithTypes(expr)
    val distUntyped = interpreter.interpret(expr)
    distFromTyped shouldEqual distUntyped
  }

  it should "match interpret for prod(2d6)" in {
    val expr = Prod(Dice(IntLiteral(2), IntLiteral(6)))
    val (distFromTyped, _) = interpreter.interpretWithTypes(expr)
    val distUntyped = interpreter.interpret(expr)
    distFromTyped shouldEqual distUntyped
  }

  it should "match for sum(1d2 + 1d2) (binomial + binomial -> generic)" in {
    val expr = Sum(Add(Dice(IntLiteral(1), IntLiteral(2)), Dice(IntLiteral(1), IntLiteral(2))))
    val (dist, tyAst) = interpreter.interpretWithTypes(expr)
    tyAst.ty shouldBe GenericDistTy
    dist shouldEqual Map(2 -> 0.25, 3 -> 0.5, 4 -> 0.25)
  }

  it should "match for prod(2d6 * 2)" in {
    val expr = Prod(Mul(Dice(IntLiteral(2), IntLiteral(6)), IntLiteral(2)))
    val (dist, _) = interpreter.interpretWithTypes(expr)
    dist.values.sum shouldBe 1.0 +- 1e-9
  }
}
