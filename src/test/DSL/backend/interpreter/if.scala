package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import DSL.frontend.AST._
import DSL.backend.interpreter

class IfInterpreterSpec extends AnyFlatSpec {

  "Interpreter with If statements" should "evaluate a simple deterministic if" in {
    // if 1 { return 100 } else { return 0 }
    val prog = Program(List(
      If(
        branches = List(
          Branch(
            bindings = Nil, 
            condition = IntLiteral(1), 
            body = List(Return(IntLiteral(100)))
          )
        ),
        elseBody = Some(List(Return(IntLiteral(0))))
      )
    ))
    val dists = interpreter.interpretProgram(prog)
    dists.head shouldEqual Map(100 -> 1.0)
  }

  it should "handle sampling and conditioning (AnyDice style)" in {
    // func f(x) { if v = ~x; v == 6 { return v } }
    // f(d6)
    // Result should be {6: 0.1666...} because only the '6' outcome returns a value
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
        )
      )),
      ExprStmt(Call("f", List(Dice(IntLiteral(1), IntLiteral(6)))))
    ))
    
    val result = interpreter.interpretProgram(prog).head
    // The sum of probabilities will be 1/6 (0.1666) because the other 5/6 outcomes 
    // don't reach a return statement.
    result(6) shouldBe (1.0 / 6.0) +- 1e-9
    result.get(1) shouldBe None 
  }

  it should "handle multiple sampled variables in the header" in {
    // if v = ~d6; w = ~d6; v == w { return 1 } else { return 0 }
    // Probability of matching on 2d6 is 1/6
    val prog = Program(List(
      If(
        branches = List(
          Branch(
            bindings = List(
              RollBinding("v", Dice(IntLiteral(1), IntLiteral(6))),
              RollBinding("w", Dice(IntLiteral(1), IntLiteral(6)))
            ),
            condition = Eq(Ident("v"), Ident("w")),
            body = List(Return(IntLiteral(1)))
          )
        ),
        elseBody = Some(List(Return(IntLiteral(0))))
      )
    ))

    val result = interpreter.interpretProgram(prog).head
    result(1) shouldBe (6.0 / 36.0) +- 1e-9
    result(0) shouldBe (30.0 / 36.0) +- 1e-9
  }

  it should "handle multiple elif branches with sampling" in {
    // if v = ~d3; v == 1 { return 10 } elif v == 2 { return 20 } else { return 30 }
    val prog = Program(List(
      If(
        branches = List(
          Branch(
            bindings = List(RollBinding("v", Dice(IntLiteral(1), IntLiteral(3)))),
            condition = Eq(Ident("v"), IntLiteral(1)),
            body = List(Return(IntLiteral(10)))
          ),
          Branch(
            bindings = Nil, // v is already in scope from the first branch binding
            condition = Eq(Ident("v"), IntLiteral(2)),
            body = List(Return(IntLiteral(20)))
          )
        ),
        elseBody = Some(List(Return(IntLiteral(30))))
      )
    ))

    val result = interpreter.interpretProgram(prog).head
    result(10) shouldBe 0.3333333333 +- 1e-5
    result(20) shouldBe 0.3333333333 +- 1e-5
    result(30) shouldBe 0.3333333333 +- 1e-5
  }

  it should "merge outcomes from branches that return complex distributions" in {
    // if v = ~d2; v == 1 { return d6 } else { return 0 }
    val prog = Program(List(
      If(
        branches = List(
          Branch(
            bindings = List(RollBinding("v", Dice(IntLiteral(1), IntLiteral(2)))),
            condition = Eq(Ident("v"), IntLiteral(1)),
            body = List(Return(Dice(IntLiteral(1), IntLiteral(6))))
          )
        ),
        elseBody = Some(List(Return(IntLiteral(0))))
      )
    ))

    val result = interpreter.interpretProgram(prog).head
    // 50% chance of being 0 (else). 
    // 50% chance of being d6 (1/6 * 0.5 = 0.0833 per outcome)
    result(0) shouldBe 0.5 +- 1e-9
    result(1) shouldBe (0.5 / 6.0) +- 1e-9
    result(6) shouldBe (0.5 / 6.0) +- 1e-9
  }
}