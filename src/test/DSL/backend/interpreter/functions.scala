package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import DSL.frontend.AST._
import DSL.backend.typeChecker
import DSL.backend.interpreter

class FunctionInterpreterSpec extends AnyFlatSpec {

  "Interpreter with functions" should "evaluate a parameterless function" in {
    val prog = Program(
      List(
        Left(Func("five", Nil, Block(Nil, IntLiteral(5)))),
        Right(Sum(Call("five", Nil)))
      )
    )

    typeChecker.check(prog) match {
      case Left(errs) => fail(errs.toString)
      case Right(typedProg) =>
        val dists = interpreter.interpretProgram(typedProg)
        dists should have length 1
        dists.head shouldEqual Map(5 -> 1.0)
    }
  }

  it should "evaluate a function with arguments" in {
    val prog = Program(
      List(
        Left(Func("addFunc", List(Param("a", None), Param("b", None)), Block(Nil, Add(Ident("a"), Ident("b"))))),
        Right(Sum(Call("addFunc", List(IntLiteral(2), IntLiteral(3)))))
      )
    )

    typeChecker.check(prog) match {
      case Left(errs) => fail(errs.toString)
      case Right(typedProg) =>
        val dists = interpreter.interpretProgram(typedProg)
        dists.head shouldEqual Map(5 -> 1.0)
    }
  }

  it should "evaluate a function with local assignments and dice" in {
    val prog = Program(
      List(
        Left(Func("rollPlus", List(Param("bonus", None)), Block(
          List(Assign("r", Dice(IntLiteral(1), IntLiteral(6)))),
          Add(Ident("r"), Ident("bonus"))
        ))),
        Right(Sum(Call("rollPlus", List(IntLiteral(4)))))
      )
    )

    typeChecker.check(prog) match {
      case Left(errs) => fail(errs.toString)
      case Right(typedProg) =>
        val dists = interpreter.interpretProgram(typedProg)
        val d = dists.head
        d.keySet shouldEqual (5 to 10).toSet
        d.values.sum shouldBe 1.0 +- 1e-9
    }
  }

  it should "maintain separate scopes for functions (shadowing)" in {
    val prog = Program(
      List(
        Left(Assign("x", IntLiteral(100))),
        Left(Func("shadow", List(Param("x", None)), Block(
          List(Assign("x", Add(Ident("x"), IntLiteral(1)))),
          Ident("x")
        ))),
        Right(Sum(Call("shadow", List(IntLiteral(5))))),
        Right(Sum(Ident("x")))
      )
    )

    typeChecker.check(prog) match {
      case Left(errs) => fail(errs.toString)
      case Right(typedProg) =>
        val dists = interpreter.interpretProgram(typedProg)
        dists should have length 2
        dists(0) shouldEqual Map(6 -> 1.0)
        dists(1) shouldEqual Map(100 -> 1.0)
    }
  }

  it should "allow functions to call other functions" in {
    val prog = Program(
      List(
        Left(Func("double", List(Param("x", None)), Block(Nil, Mul(Ident("x"), IntLiteral(2))))),
        Left(Func("quadruple", List(Param("x", None)), Block(Nil, Call("double", List(Call("double", List(Ident("x")))))))),
        Right(Sum(Call("quadruple", List(IntLiteral(3)))))
      )
    )

    typeChecker.check(prog) match {
      case Left(errs) => fail(errs.toString)
      case Right(typedProg) =>
        val dists = interpreter.interpretProgram(typedProg)
        dists.head shouldEqual Map(12 -> 1.0)
    }
  }

  it should "fail if calling an undefined function" in {
    val prog = Program(
      List(
        Right(Sum(Call("missing", Nil)))
      )
    )

    // Type checking passes (assuming arity check is runtime or loose in AST), 
    // but interpreter should fail.
    typeChecker.check(prog) match {
      case Left(errs) => fail(errs.toString)
      case Right(typedProg) =>
        val err = the[IllegalArgumentException] thrownBy interpreter.interpretProgram(typedProg)
        err.getMessage should include("Undefined function: missing")
    }
  }
}