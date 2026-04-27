package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import DSL.frontend.AST._
import DSL.backend.interpreter

class IfInterpreterSpec extends AnyFlatSpec {

  "Interpreter with If statements" should "evaluate a simple deterministic if" in {
    // if 1 { 100 } else { 0 }
    val prog = Program(List(
      If(
        branches = List(
          Branch(
            bindings = Nil, 
            condition = IntLiteral(1), 
            body = List(ExprStmt(Sum(IntLiteral(100))))
          )
        ),
        elseBody = Some(List(ExprStmt(Sum(IntLiteral(0)))))
      )
    ))
    val dists = interpreter.interpretProgram(prog)
    // Result of the expression statement
    dists.head shouldEqual Map(100 -> 1.0)
  }

  it should "handle sampling and conditioning (AnyDice style)" in {
    // This test correctly uses a function to test return behavior
    // func f(x) { if v = ~x; v == 6 { return v } return 0 }
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
      ExprStmt(Sum(Call("f", List(Dice(IntLiteral(1), IntLiteral(6))))))
    ))
    
    val result = interpreter.interpretProgram(prog).head
    result(6) shouldBe (1.0 / 6.0) +- 1e-9
    result(0) shouldBe (5.0 / 6.0) +- 1e-9
  }

  it should "handle multiple sampled variables in the header" in {
    // if v = ~d6; w = ~d6; v == w { 1 } else { 0 }
    val prog = Program(List(
      If(
        branches = List(
          Branch(
            bindings = List(
              RollBinding("v", Dice(IntLiteral(1), IntLiteral(6))),
              RollBinding("w", Dice(IntLiteral(1), IntLiteral(6)))
            ),
            condition = Eq(Ident("v"), Ident("w")),
            body = List(ExprStmt(Sum(IntLiteral(1))))
          )
        ),
        elseBody = Some(List(ExprStmt(Sum(IntLiteral(0)))))
      )
    ))

    val result = interpreter.interpretProgram(prog).head
    result(1) shouldBe (6.0 / 36.0) +- 1e-9
    result(0) shouldBe (30.0 / 36.0) +- 1e-9
  }

  it should "handle multiple elif branches with sampling" in {
    // if v = ~d3; v == 1 { 10 } elif v == 2 { 20 } else { 30 }
    val prog = Program(List(
      If(
        branches = List(
          Branch(
            bindings = List(RollBinding("v", Dice(IntLiteral(1), IntLiteral(3)))),
            condition = Eq(Ident("v"), IntLiteral(1)),
            body = List(ExprStmt(Sum(IntLiteral(10))))
          ),
          Branch(
            bindings = Nil, 
            condition = Eq(Ident("v"), IntLiteral(2)),
            body = List(ExprStmt(Sum(IntLiteral(20))))
          )
        ),
        elseBody = Some(List(ExprStmt(Sum(IntLiteral(30)))))
      )
    ))

    val result = interpreter.interpretProgram(prog).head
    result(10) shouldBe 0.3333333333 +- 1e-5
    result(20) shouldBe 0.3333333333 +- 1e-5
    result(30) shouldBe 0.3333333333 +- 1e-5
  }

  it should "merge outcomes from branches with complex distributions" in {
    // if v = ~d2; v == 1 { d6 } else { 0 }
    val prog = Program(List(
      If(
        branches = List(
          Branch(
            bindings = List(RollBinding("v", Dice(IntLiteral(1), IntLiteral(2)))),
            condition = Eq(Ident("v"), IntLiteral(1)),
            body = List(ExprStmt(Sum(Dice(IntLiteral(1), IntLiteral(6)))))
          )
        ),
        elseBody = Some(List(ExprStmt(Sum(IntLiteral(0)))))
      )
    ))

    val result = interpreter.interpretProgram(prog).head
    // 50% chance of 0 (else). 50% chance of d6 (1/12 each)
    result(0) shouldBe 0.5 +- 1e-9
    result(1) shouldBe (0.5 / 6.0) +- 1e-9
    result(6) shouldBe (0.5 / 6.0) +- 1e-9
  }
}