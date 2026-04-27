package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import DSL.frontend.AST._
import DSL.backend.optimiser

class OptimiserSpec extends AnyFlatSpec with Matchers {

  def optimize(stmts: Stmt*): List[Stmt] = {
    val prog = Program(stmts.toList)
    val Program(optimizedStmts) = optimiser.optimise(prog)
    optimizedStmts
  }

  "Optimiser" should "fold basic arithmetic constants" in {
    val ast = List(ExprStmt(Add(IntLiteral(1), IntLiteral(2))))
    optimize(ast: _*) shouldBe List(ExprStmt(IntLiteral(3)))
  }

  it should "remove unreachable code after return" in {
    val ast = List(Return(IntLiteral(1)), Return(IntLiteral(0)))
    optimize(ast: _*) shouldBe List(Return(IntLiteral(1)))
  }

  it should "eliminate dead stores" in {
    val ast = List(Assign("x", IntLiteral(100)), Return(IntLiteral(50)))
    optimize(ast: _*) shouldBe List(Return(IntLiteral(50)))
  }

  it should "optimize inside function bodies" in {
    val ast = List(Func("f", Nil, List(Return(Add(IntLiteral(5), IntLiteral(5))))))
    optimize(ast: _*) match {
      case List(Func("f", Nil, List(Return(IntLiteral(10))))) => // success
      case _ => fail("Not optimized")
    }
  }

  it should "optimize inside If statement branches" in {
    val ast = List(
      If(List(Branch(Nil, IntLiteral(1), List(Return(Add(IntLiteral(1), IntLiteral(2)))))), None)
    )
    optimize(ast: _*) shouldBe List(
      If(List(Branch(Nil, IntLiteral(1), List(Return(IntLiteral(3))))), None)
    )
  }

  it should "poison variables modified inside an if block" in {
    val ast = List(
      Assign("x", IntLiteral(10)),
      If(List(Branch(Nil, Dice(IntLiteral(1), IntLiteral(6)), List(Assign("x", IntLiteral(20))))), None),
      Return(Ident("x"))
    )
    optimize(ast: _*).last shouldBe Return(Ident("x"))
  }
}