package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import DSL.frontend.AST._
import DSL.backend.typer
import DSL.backend.typedAST._
import DSL.backend._

class TyperSpec extends AnyFlatSpec {

  "Typer" should "annotate IntLiteral as Scalar" in {
    val t = typer.annotate(IntLiteral(5))
    t match {
      case TyIntLiteral(v, ty) =>
        v shouldBe 5
        ty shouldBe ScalarTy
      case other =>
        fail(s"Expected TyIntLiteral, got $other")
    }
  }

  it should "annotate identifiers as UnknownTy" in {
    val t = typer.annotate(Ident("x"))
    t match {
      case TyIdent(name, ty) =>
        name shouldBe "x"
        ty shouldBe UnknownTy
      case other =>
        fail(s"Expected TyIdent with UnknownTy, got $other")
    }
  }

  it should "annotate CustomDist using semantic classification" in {
    val bernExpr = CustomDist(Map(0 -> 0.9, 1 -> 0.1))
    val bernTyped = typer.annotate(bernExpr)
    bernTyped match {
      case TyCustomDist(dist, BernoulliTy(p)) =>
        dist shouldBe bernExpr.dist
        p shouldBe 0.1 +- 1e-12
      case other =>
        fail(s"Expected BernoulliTy for {0,1} distribution, got $other")
    }

    val binomExpr = CustomDist(Map(1 -> 0.5, 2 -> 0.5))
    val binomTyped = typer.annotate(binomExpr)
    binomTyped match {
      case TyCustomDist(dist, BinomialTy) =>
        dist shouldBe binomExpr.dist
      case other =>
        fail(s"Expected BinomialTy for two-outcome non-Bernoulli distribution, got $other")
    }
  }

  it should "propagate types through Sum and Prod" in {
    val sumTyped = typer.annotate(Sum(IntLiteral(3)))
    sumTyped match {
      case TyUnary(UnaryOp.Sum, TyIntLiteral(_, ScalarTy), ty) =>
        ty shouldBe ScalarTy
      case other =>
        fail(s"Expected Sum over Scalar to remain Scalar, got $other")
    }

    val prodTyped = typer.annotate(Prod(IntLiteral(4)))
    prodTyped match {
      case TyUnary(UnaryOp.Prod, TyIntLiteral(_, ScalarTy), ty) =>
        ty shouldBe ScalarTy
      case other =>
        fail(s"Expected Prod over Scalar to remain Scalar, got $other")
    }
  }

  it should "type Dice conservatively as GenericDist" in {
    val typed = typer.annotate(Dice(IntLiteral(2), IntLiteral(6)))
    typed match {
      case TyBinary(BinaryOp.Dice, _, _, ty) =>
        ty shouldBe GenericDistTy
      case other =>
        fail(s"Expected Dice to have GenericDistTy, got $other")
    }
  }

  it should "type arithmetic over scalars as Scalar and otherwise Generic" in {
    val scalarAdd = typer.annotate(Add(IntLiteral(1), IntLiteral(2)))
    scalarAdd match {
      case TyBinary(BinaryOp.Add, TyIntLiteral(_, ScalarTy), TyIntLiteral(_, ScalarTy), ty) =>
        ty shouldBe ScalarTy
      case other =>
        fail(s"Expected Add of two scalars to be Scalar, got $other")
    }

    val mixedAdd = typer.annotate(Add(Dice(IntLiteral(1), IntLiteral(6)), IntLiteral(2)))
    mixedAdd match {
      case TyBinary(BinaryOp.Add, _, _, ty) =>
        ty shouldBe GenericDistTy
      case other =>
        fail(s"Expected Add of non-scalars to be GenericDistTy, got $other")
    }
  }
}

