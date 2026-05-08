package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import DSL.frontend.AST._
import DSL.backend.optimiser

class OptimiserSpec extends AnyFlatSpec with Matchers {

  def optimize(items: Either[Stmt, Expr]*): List[Either[Stmt, Expr]] = {
    val prog = Program(items.toList)
    val Program(optimisedTopLevel) = optimiser.optimise(prog)
    optimisedTopLevel
  }

  "Optimiser" should "fold basic arithmetic constants" in {
    val ast = List(Right(Add(IntLiteral(1), IntLiteral(2))))
    optimize(ast: _*) shouldBe List(Right(IntLiteral(3)))
  }

  it should "eliminate dead stores" in {
    val ast = List(Left(Assign("x", IntLiteral(100))), Right(IntLiteral(50)))
    optimize(ast: _*) shouldBe List(Right(IntLiteral(50)))
  }

  it should "optimise inside function bodies" in {
    val ast = List(Left(Func("f", Nil, Block(Nil, Add(IntLiteral(5), IntLiteral(5))))))
    optimize(ast: _*) match {
      case List(Left(Func("f", Nil, Block(Nil, IntLiteral(10))))) => // success
      case _ => fail("Not optimised")
    }
  }

  it should "optimise inside If statement branches" in {
    val ast = List(
      Right(IfExpr(
        branches = List(
          IfBranch(Nil, IntLiteral(1), Block(Nil, Add(IntLiteral(1), IntLiteral(2))))
        ),
        elseBranch = Block(Nil, IntLiteral(0))
      ))
    )
    optimize(ast: _*) shouldBe List(
      Right(IfExpr(
        branches = List(
          IfBranch(Nil, IntLiteral(1), Block(Nil, IntLiteral(3)))
        ),
        elseBranch = Block(Nil, IntLiteral(0))
      ))
    )
  }

  it should "poison variables modified inside an if block" in {
    val ast = List(
      Left(Assign("x", IntLiteral(10))),
      Right(IfExpr(
        branches = List(
          IfBranch(Nil, Dice(IntLiteral(1), IntLiteral(6)), Block(List(Assign("x", IntLiteral(20))), IntLiteral(1)))
        ),
        elseBranch = Block(Nil, IntLiteral(0))
      )),
      Right(Ident("x"))
    )
    optimize(ast: _*).last shouldBe Right(Ident("x"))
  }

  it should "remove unused RollBindings in IfExpr branches" in {
    // v is used in the condition, w is never used
    val ast = List(
      Right(IfExpr(
        branches = List(
          IfBranch(
            bindings = List(
              RollBinding("v", Dice(IntLiteral(1), IntLiteral(6))),
              RollBinding("w", Dice(IntLiteral(1), IntLiteral(20))) // w is unused
            ),
            condition = Eq(Ident("v"), IntLiteral(6)),
            body = Block(Nil, Ident("v"))
          )
        ),
        elseBranch = Block(Nil, IntLiteral(0))
      ))
    )

    val expected = List(
      Right(IfExpr(
        branches = List(
          IfBranch(
            bindings = List(
              RollBinding("v", Dice(IntLiteral(1), IntLiteral(6))) // w is correctly removed
            ),
            condition = Eq(Ident("v"), IntLiteral(6)),
            body = Block(Nil, Ident("v"))
          )
        ),
        elseBranch = Block(Nil, IntLiteral(0))
      ))
    )

    optimize(ast: _*) shouldBe expected
  }
}