package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import DSL.frontend.AST._
import DSL.backend.typer
import DSL.backend.typedAST._
import DSL.backend._
import DSL.backend.semanticTypes._

class TyperSpec extends AnyFlatSpec {

  "Typer" should "annotate IntLiteral as Scalar" in {
    val t = typer.annotate(IntLiteral(5))
    t match {
      case TyIntLiteral(v, ty) =>
        v shouldBe 5
        ty should equal (DistTy(ScalarTy))
      case _ => fail("Wrong type")
    }
  }

  it should "annotate CustomDist using semantic classification" in {
    val bernExpr = CustomDist(Map(0 -> 0.9, 1 -> 0.1))
    val bernTyped = typer.annotate(bernExpr)
    bernTyped match {
      // The type is now DistTy(BernoulliTy(p))
      case TyCustomDist(_, DistTy(BernoulliTy(p))) =>
        p shouldBe 0.1 +- 1e-12
      case _ => fail("Expected DistTy(BernoulliTy(p))")
    }
  }

  it should "propagate types through Sum and Prod" in {
    val sumTyped = typer.annotate(Sum(IntLiteral(3)))
    // Sum of a scalar remains a scalar, wrapped in DistTy
    sumTyped.ty should equal (DistTy(ScalarTy))
  }

  it should "type Dice conservatively as PoolTy" in {
    val typed = typer.annotate(Dice(IntLiteral(2), IntLiteral(6)))
    // Dice with count > 1 is PoolTy
    typed.ty shouldBe PoolTy
  }

  it should "type arithmetic over scalars as Scalar and otherwise Generic" in {
    val scalarAdd = typer.annotate(Add(IntLiteral(1), IntLiteral(2)))
    scalarAdd.ty should equal (DistTy(ScalarTy))

    // 1d6 is DistTy(UniformTy). Adding Scalar to DistTy yields DistTy(GenericTy)
    val mixedAdd = typer.annotate(Add(Dice(IntLiteral(1), IntLiteral(6)), IntLiteral(2)))
    mixedAdd.ty should equal (DistTy(GenericTy))
  }

  it should "type comparison as Bernoulli of unknown probability" in {
    val typed = typer.annotate(Eq(Dice(IntLiteral(1), IntLiteral(6)), IntLiteral(6)))
    // Comparisons return a distribution, specifically DistTy(BernoulliTy(...))
    typed.ty should equal (DistTy(BernoulliTy(0.0)))
  }
}