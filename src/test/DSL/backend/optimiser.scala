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

  it should "keep assignments that are used in the block's final expression" in {
    val block = Block(
      List(Assign("cond", Eq(Ident("v"), IntLiteral(1)))),
      Add(
        Mul(Ident("cond"), IntLiteral(100)),
        Mul(Sub(IntLiteral(1), Ident("cond")), IntLiteral(1))
      )
    )

    val result = optimize(Right(block)).headOption  
    result match {
      case Some(Right(Block(stmts, _))) =>
        stmts should contain(Assign("cond", Eq(Ident("v"), IntLiteral(1))))
      case _ => fail("Block not preserved or Assign was incorrectly removed")
    }
  }
}