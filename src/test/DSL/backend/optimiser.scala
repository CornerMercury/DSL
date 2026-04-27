package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import DSL.frontend.AST._
import DSL.backend.optimiser

class OptimiserSpec extends AnyFlatSpec with Matchers {

  // Helper to make test construction cleaner
  def optimize(stmts: Stmt*): List[Stmt] = {
    val prog = Program(stmts.toList)
    val Program(optimizedStmts) = optimiser.optimise(prog)
    optimizedStmts
  }

  "Optimiser" should "fold basic arithmetic constants" in {
    // 1 + 2 -> 3
    // 10 - 4 -> 6
    // 2 * 3 -> 6
    // 10 / 2 -> 5
    val ast = List(
      ExprStmt(Add(IntLiteral(1), IntLiteral(2))),
      ExprStmt(Sub(IntLiteral(10), IntLiteral(4))),
      ExprStmt(Mul(IntLiteral(2), IntLiteral(3))),
      ExprStmt(Div(IntLiteral(10), IntLiteral(2)))
    )

    val result = optimize(ast: _*)

    result shouldBe List(
      ExprStmt(IntLiteral(3)),
      ExprStmt(IntLiteral(6)),
      ExprStmt(IntLiteral(6)),
      ExprStmt(IntLiteral(5))
    )
  }

  it should "apply algebraic identities (x+0, x*1, etc)" in {
    // x + 0 -> x
    // x * 1 -> x
    // 0 * x -> 0
    val ast = List(
      Assign("x", IntLiteral(10)), 
      ExprStmt(Add(Ident("x"), IntLiteral(0))),
      ExprStmt(Mul(Ident("x"), IntLiteral(1))),
      ExprStmt(Mul(IntLiteral(0), Ident("x")))
    )

    val result = optimize(ast: _*)

    // Note: The assignment "x=10" is removed because "x" is propagated 
    // into the statements as literals, making the original assignment dead.
    result shouldBe List(
      ExprStmt(IntLiteral(10)), // 10 + 0 -> 10
      ExprStmt(IntLiteral(10)), // 10 * 1 -> 10
      ExprStmt(IntLiteral(0))   // 0 * 10 -> 0
    )
  }

  it should "remove unreachable code after return" in {
    // return 1
    // return 0   <-- Dead
    // x = 5      <-- Dead
    val ast = List(
      Return(IntLiteral(1)),
      Return(IntLiteral(0)),
      Assign("x", IntLiteral(5))
    )

    val result = optimize(ast: _*)

    result shouldBe List(
      Return(IntLiteral(1))
    )
  }

  it should "handle variable reassignment and propagation (The x=1; x=x+5 case)" in {
    // x = 1
    // x = x + 5
    // return x
    val ast = List(
      Assign("x", IntLiteral(1)),
      Assign("x", Add(Ident("x"), IntLiteral(5))),
      Return(Ident("x"))
    )

    val result = optimize(ast: _*)

    result shouldBe List(
      Return(IntLiteral(6))
    )
  }

  it should "handle overwrite reassignment (x=5; x=10)" in {
    // x = 5
    // x = 10
    // return x
    val ast = List(
      Assign("x", IntLiteral(5)),
      Assign("x", IntLiteral(10)),
      Return(Ident("x"))
    )

    val result = optimize(ast: _*)

    result shouldBe List(
      Return(IntLiteral(10))
    )
  }

  it should "eliminate dead stores (variables assigned but never used)" in {
    // x = 100  (Dead)
    // y = 50   (Used)
    // z = 20   (Dead)
    // return y
    val ast = List(
      Assign("x", IntLiteral(100)),
      Assign("y", IntLiteral(50)),
      Assign("z", IntLiteral(20)),
      Return(Ident("y"))
    )

    val result = optimize(ast: _*)

    // y is propagated to 50 in the return.
    // Since 'Return(50)' does not use identifier 'y', 'y=50' is also dead code.
    result shouldBe List(
      Return(IntLiteral(50))
    )
  }

  it should "NOT remove assignments if variables are unknown (runtime values)" in {
    // x = Dice(1, 6)  (Cannot propagate, must keep assignment)
    // y = x + 1       (Cannot fold completely, must keep)
    // return y
    val ast = List(
      Assign("x", Dice(IntLiteral(1), IntLiteral(6))),
      Assign("y", Add(Ident("x"), IntLiteral(1))),
      Return(Ident("y"))
    )

    val result = optimize(ast: _*)

    result shouldBe List(
      Assign("x", Dice(IntLiteral(1), IntLiteral(6))),
      Assign("y", Add(Ident("x"), IntLiteral(1))),
      Return(Ident("y"))
    )
  }

  it should "optimize inside function bodies" in {
    val ast = List(
      Func("myFunc", List("p"), List(
        Assign("a", IntLiteral(5)),
        Assign("b", Add(Ident("a"), IntLiteral(5))), // a is 5 -> b is 10
        Return(Ident("b")),
        Assign("dead", IntLiteral(99)) // Unreachable after return
      ))
    )

    val result = optimize(ast: _*)

    result match {
      case List(Func("myFunc", List("p"), body)) =>
        // Inside the function, 'a' and 'b' should be propagated and dead stores removed.
        // The code after Return should also be removed.
        body shouldBe List(Return(IntLiteral(10)))
      case _ => fail("Structure changed unexpectedly")
    }
  }

  it should "stop propagation when a variable depends on a non-constant" in {
    // a = 5
    // b = Dice(1, 6)
    // a = b + 1  <-- 'a' is no longer 5, it is unknown.
    // return a
    val ast = List(
      Assign("a", IntLiteral(5)),
      Assign("b", Dice(IntLiteral(1), IntLiteral(6))),
      Assign("a", Add(Ident("b"), IntLiteral(1))),
      Return(Ident("a"))
    )

    val result = optimize(ast: _*)

    // 1. a=5 is removed (overwritten later without read? No, strictly it's overwritten).
    // 2. b is unknown, kept.
    // 3. a becomes b+1 (unknown).
    // 4. return a (uses a).
    result shouldBe List(
      Assign("b", Dice(IntLiteral(1), IntLiteral(6))),
      Assign("a", Add(Ident("b"), IntLiteral(1))),
      Return(Ident("a"))
    )
  }

  it should "fold comparison constants" in {
    val ast = List(
      ExprStmt(Eq(IntLiteral(1), IntLiteral(1))),   // 1 == 1 -> 1
      ExprStmt(Eq(IntLiteral(1), IntLiteral(2))),   // 1 == 2 -> 0
      ExprStmt(IdenEq(IntLiteral(5), IntLiteral(5))) // 5 === 5 -> 1
    )

    val result = optimize(ast: _*)

    result shouldBe List(
      ExprStmt(IntLiteral(1)),
      ExprStmt(IntLiteral(0)),
      ExprStmt(IntLiteral(1))
    )
  }

  it should "optimize inside If statement branches" in {
    // if 1 { x = 1 + 2; return x }
    val ast = List(
      If(
        branches = List((IntLiteral(1), List(
          Assign("x", Add(IntLiteral(1), IntLiteral(2))),
          Return(Ident("x"))
        ))),
        elseBody = None
      )
    )

    val result = optimize(ast: _*)

    result shouldBe List(
      If(List((IntLiteral(1), List(Return(IntLiteral(3))))), None)
    )
  }

  it should "poison variables modified inside an if block" in {
    // x = 10
    // if d6 { x = 20 }
    // return x  <-- cannot be optimized to 10 or 20 because x is 'poisoned'
    val ast = List(
      Assign("x", IntLiteral(10)),
      If(List((Dice(IntLiteral(1), IntLiteral(6)), List(Assign("x", IntLiteral(20))))), None),
      Return(Ident("x"))
    )

    val result = optimize(ast: _*)

    // x=10 is kept because it's used in the If (potentially), 
    // and Return(x) is kept because x is no longer a known constant.
    result.last shouldBe Return(Ident("x"))
  }
}