package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import DSL.frontend.AST._
import DSL.backend.interpreter

class FunctionInterpreterSpec extends AnyFlatSpec {

  "Interpreter with functions" should "evaluate a parameterless function" in {
    val prog = Program(
      List(
        Func("five", Nil, List(
          Return(IntLiteral(5))
        )),
        // funcCall -> wrapped in Sum by parser
        ExprStmt(Sum(Call("five", Nil)))
      )
    )

    val dists = interpreter.interpretProgram(prog)
    dists should have length 1
    dists.head shouldEqual Map(5 -> 1.0)
  }

  it should "evaluate a function with arguments" in {
    val prog = Program(
      List(
        Func("addFunc", List("a", "b"), List(
          Return(Add(Ident("a"), Ident("b")))
        )),
        ExprStmt(Sum(Call("addFunc", List(IntLiteral(2), IntLiteral(3)))))
      )
    )

    val dists = interpreter.interpretProgram(prog)
    dists.head shouldEqual Map(5 -> 1.0)
  }

  it should "evaluate a function with local assignments and dice" in {
    val prog = Program(
      List(
        Func("rollPlus", List("bonus"), List(
          Assign("r", Dice(IntLiteral(1), IntLiteral(6))),
          Return(Add(Ident("r"), Ident("bonus")))
        )),
        ExprStmt(Sum(Call("rollPlus", List(IntLiteral(4)))))
      )
    )

    val dists = interpreter.interpretProgram(prog)
    val d = dists.head
    d.keySet shouldEqual (5 to 10).toSet
    d.values.sum shouldBe 1.0 +- 1e-9
  }

  it should "short-circuit execution upon hitting a Return statement" in {
    val prog = Program(
      List(
        Func("earlyRet", Nil, List(
          Return(IntLiteral(10)),
          Return(IntLiteral(20)) // Should not be reached/evaluated
        )),
        ExprStmt(Sum(Call("earlyRet", Nil)))
      )
    )

    val dists = interpreter.interpretProgram(prog)
    dists.head shouldEqual Map(10 -> 1.0)
  }

  it should "maintain separate scopes for functions (shadowing)" in {
    val prog = Program(
      List(
        Assign("x", IntLiteral(100)),
        Func("shadow", List("x"), List(
          // Modifies local parameter 'x', not global 'x'
          Assign("x", Add(Ident("x"), IntLiteral(1))),
          Return(Ident("x"))
        )),
        // Call shadow(5) -> returns 6
        ExprStmt(Sum(Call("shadow", List(IntLiteral(5))))),
        // Global 'x' should still be 100
        ExprStmt(Sum(Ident("x")))
      )
    )

    val dists = interpreter.interpretProgram(prog)
    dists should have length 2
    dists(0) shouldEqual Map(6 -> 1.0)
    dists(1) shouldEqual Map(100 -> 1.0)
  }

  it should "allow functions to call other functions" in {
    val prog = Program(
      List(
        Func("double", List("x"), List(
          Return(Mul(Ident("x"), IntLiteral(2)))
        )),
        Func("quadruple", List("x"), List(
          Return(Call("double", List(Call("double", List(Ident("x"))))))
        )),
        ExprStmt(Sum(Call("quadruple", List(IntLiteral(3)))))
      )
    )

    val dists = interpreter.interpretProgram(prog)
    dists.head shouldEqual Map(12 -> 1.0)
  }

  "Interpreter errors for functions" should "fail if a function does not return a value" in {
    val prog = Program(
      List(
        Func("noRet", Nil, List(
          Assign("x", IntLiteral(1))
        )),
        ExprStmt(Sum(Call("noRet", Nil)))
      )
    )

    val err = the[IllegalArgumentException] thrownBy interpreter.interpretProgram(prog)
    err.getMessage should include("reached the end of its body without returning a value")
  }

  it should "fail if calling an undefined function" in {
    val prog = Program(
      List(
        ExprStmt(Sum(Call("missing", Nil)))
      )
    )

    val err = the[IllegalArgumentException] thrownBy interpreter.interpretProgram(prog)
    err.getMessage should include("Undefined function: missing")
  }

  it should "fail if argument arity is mismatched" in {
    val prog = Program(
      List(
        Func("needsTwo", List("a", "b"), List(
          Return(IntLiteral(0))
        )),
        ExprStmt(Sum(Call("needsTwo", List(IntLiteral(1))))) // only 1 arg passed
      )
    )

    val err = the[IllegalArgumentException] thrownBy interpreter.interpretProgram(prog)
    err.getMessage should include("expects 2 arguments, got 1")
  }
}