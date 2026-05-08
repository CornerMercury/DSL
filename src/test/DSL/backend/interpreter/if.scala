package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import DSL.frontend.AST._
import DSL.backend.interpreter

class IfInterpreterSpec extends AnyFlatSpec {

  "Interpreter with If statements" should "evaluate a simple deterministic if" in {
    // if 1 { 100 } else { 0 }
    val prog = Program(List(
      Right(IfExpr(
        branches = List(
          IfBranch(Nil, IntLiteral(1), Block(Nil, IntLiteral(100)))
        ),
        elseBranch = Block(Nil, IntLiteral(0))
      ))
    ))
    val dists = interpreter.interpretProgram(prog)
    dists.head shouldEqual Map(100 -> 1.0)
  }

  it should "handle multiple sampled variables in the header" in {
    // if v = ~d6; w = ~d6; v == w { 1 } else { 0 }
    val prog = Program(List(
      Right(IfExpr(
        branches = List(
          IfBranch(
            bindings = List(
              RollBinding("v", Dice(IntLiteral(1), IntLiteral(6))),
              RollBinding("w", Dice(IntLiteral(1), IntLiteral(6)))
            ),
            condition = Eq(Ident("v"), Ident("w")),
            body = Block(Nil, IntLiteral(1))
          )
        ),
        elseBranch = Block(Nil, IntLiteral(0))
      ))
    ))

    val result = interpreter.interpretProgram(prog).head
    result(1) shouldBe (6.0 / 36.0) +- 1e-9
    result(0) shouldBe (30.0 / 36.0) +- 1e-9
  }

  it should "handle nested elif branches with sampling" in {
    // if v = ~d3; v == 1 { 10 } elif v == 2 { 20 } else { 30 }
    val prog = Program(List(
      Right(IfExpr(
        branches = List(
          IfBranch(
            bindings = List(RollBinding("v", Dice(IntLiteral(1), IntLiteral(3)))),
            condition = Eq(Ident("v"), IntLiteral(1)),
            body = Block(Nil, IntLiteral(10))
          ),
          IfBranch(
            bindings = Nil,
            condition = Eq(Ident("v"), IntLiteral(2)),
            body = Block(Nil, IntLiteral(20))
          )
        ),
        elseBranch = Block(Nil, IntLiteral(30))
      ))
    ))

    val result = interpreter.interpretProgram(prog).head
    result(10) shouldBe 0.3333333333 +- 1e-5
    result(20) shouldBe 0.3333333333 +- 1e-5
    result(30) shouldBe 0.3333333333 +- 1e-5
  }

  it should "merge outcomes from branches with complex distributions" in {
    // if v = ~d2; v == 1 { d6 } else { 0 }
    val prog = Program(List(
      Right(IfExpr(
        branches = List(
          IfBranch(
            bindings = List(RollBinding("v", Dice(IntLiteral(1), IntLiteral(2)))),
            condition = Eq(Ident("v"), IntLiteral(1)),
            body = Block(Nil, Dice(IntLiteral(1), IntLiteral(6)))
          )
        ),
        elseBranch = Block(Nil, IntLiteral(0))
      ))
    ))

    val result = interpreter.interpretProgram(prog).head
    // 50% chance of 0 (else). 50% chance of d6 (1/12 each)
    result(0) shouldBe 0.5 +- 1e-9
    result(1) shouldBe (0.5 / 6.0) +- 1e-9
    result(6) shouldBe (0.5 / 6.0) +- 1e-9
  }
}