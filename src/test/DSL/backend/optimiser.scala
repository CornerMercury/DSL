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

  it should "eliminate dead stores" in {
    val ast = List(Assign("x", IntLiteral(100)), ExprStmt(IntLiteral(50)))
    optimize(ast: _*) shouldBe List(ExprStmt(IntLiteral(50)))
  }

  it should "optimize inside function bodies" in {
    val ast = List(Func("f", Nil, Block(Nil, Add(IntLiteral(5), IntLiteral(5)))))
    optimize(ast: _*) match {
      case List(Func("f", Nil, Block(Nil, IntLiteral(10)))) => // success
      case _ => fail("Not optimized")
    }
  }

  it should "optimize inside If statement branches" in {
    val ast = List(
      ExprStmt(IfExpr(Nil, IntLiteral(1), Block(Nil, Add(IntLiteral(1), IntLiteral(2))), Block(Nil, IntLiteral(0))))
    )
    optimize(ast: _*) shouldBe List(
      ExprStmt(IfExpr(Nil, IntLiteral(1), Block(Nil, IntLiteral(3)), Block(Nil, IntLiteral(0))))
    )
  }

  it should "poison variables modified inside an if block" in {
    val ast = List(
      Assign("x", IntLiteral(10)),
      ExprStmt(IfExpr(Nil, Dice(IntLiteral(1), IntLiteral(6)), Block(List(Assign("x", IntLiteral(20))), IntLiteral(1)), Block(Nil, IntLiteral(0)))),
      ExprStmt(Ident("x"))
    )
    optimize(ast: _*).last shouldBe ExprStmt(Ident("x"))
  }
}