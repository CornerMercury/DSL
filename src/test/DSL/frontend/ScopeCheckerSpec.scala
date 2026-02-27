package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import DSL.frontend.AST._
import DSL.frontend.scopeChecker
import DSL.frontend.{ScopeError, UndeclaredVariable, DuplicateFunction, DuplicateParameter, ReturnOutsideFunction}

class ScopeCheckerSpec extends AnyFlatSpec {

  def assertNoErrors(program: Program): Unit = {
    val errs = scopeChecker.check(program)
    assert(errs.isEmpty, s"Expected no scope errors but got: $errs")
  }

  def assertErrors(program: Program)(expected: ScopeError*): Unit = {
    val errs = scopeChecker.check(program)
    errs should contain theSameElementsAs expected
  }

  "Scope checker" should "accept program with declared variable used in expression" in {
    assertNoErrors(Program(List(
      Assign("x", IntLiteral(1)),
      ExprStmt(Sum(Add(Ident("x"), IntLiteral(2))))
    )))
  }

  it should "report undeclared variable" in {
    assertErrors(Program(List(ExprStmt(Sum(Ident("x"))))))(
      UndeclaredVariable("x")
    )
  }

  it should "allow shadowing in function body" in {
    assertNoErrors(Program(List(
      Assign("x", IntLiteral(1)),
      Func("f", List("x"), List(Return(Ident("x")))),
      ExprStmt(Sum(Ident("x")))
    )))
  }

  it should "report undeclared variable in function body" in {
    assertErrors(Program(List(
      Func("f", List("a"), List(
        Assign("b", Add(Ident("a"), Ident("c"))),
        Return(Ident("b"))
      ))
    )))(
      UndeclaredVariable("c")
    )
  }

  it should "report return outside function" in {
    assertErrors(Program(List(Return(IntLiteral(1)))))(
      ReturnOutsideFunction
    )
  }

  it should "accept return inside function" in {
    assertNoErrors(Program(List(
      Func("id", List("x"), List(Return(Ident("x"))))
    )))
  }

  it should "report duplicate function name" in {
    assertErrors(Program(List(
      Func("f", List(), List(Return(IntLiteral(0)))),
      Func("f", List(), List(Return(IntLiteral(1))))
    )))(
      DuplicateFunction("f")
    )
  }

  it should "report duplicate parameter name" in {
    assertErrors(Program(List(
      Func("f", List("x", "x"), List(Return(IntLiteral(0))))
    )))(
      DuplicateParameter("x")
    )
  }

  it should "accept multiple functions and main statements" in {
    assertNoErrors(Program(List(
      Func("f", List("a", "b"), List(
        Assign("s", Add(Ident("a"), Ident("b"))),
        Return(Ident("s"))
      )),
      Assign("x", IntLiteral(10)),
      ExprStmt(Sum(Ident("x")))
    )))
  }
}
