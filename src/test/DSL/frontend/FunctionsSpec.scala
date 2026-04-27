package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import DSL.frontend.AST._
import DSL.backend.interpreter

class FunctionInterpreterSpec extends AnyFlatSpec {

  "Interpreter" should "handle probability fallthrough after a conditional return" in {
    /*
      func f(x) {
          if v = ~x; v == 6 {
              return v
          }
          return 0
      }
      f(d6)
    */
    val prog = Program(List(
      Func("f", List("x"), List(
        If(
          branches = List(
            Branch(
              bindings = List(RollBinding("v", Ident("x"))),
              condition = Eq(Ident("v"), IntLiteral(6)),
              body = List(Return(Ident("v")))
            )
          ),
          elseBody = None
        ),
        Return(IntLiteral(0))
      )),
      ExprStmt(Call("f", List(Dice(IntLiteral(1), IntLiteral(6)))))
    ))

    val result = interpreter.interpretProgram(prog).head
    
    // Result should be {6: 16.67%, 0: 83.33%}
    result(6) shouldBe (1.0 / 6.0) +- 1e-9
    result(0) shouldBe (5.0 / 6.0) +- 1e-9
    result.values.sum shouldBe 1.0 +- 1e-9
  }

  it should "evaluate nested conditional returns" in {
    // func f() { if d2 == 1 { return 10 }; return 20 }
    val prog = Program(List(
      Func("f", Nil, List(
        If(
          branches = List(
            Branch(Nil, Eq(Dice(IntLiteral(1), IntLiteral(2)), IntLiteral(1)), List(Return(IntLiteral(10))))
          ),
          elseBody = None
        ),
        Return(IntLiteral(20))
      )),
      ExprStmt(Call("f", Nil))
    ))

    val result = interpreter.interpretProgram(prog).head
    result(10) shouldBe 0.5 +- 1e-9
    result(20) shouldBe 0.5 +- 1e-9
  }
}