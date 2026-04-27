package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import DSL.frontend.AST._
import DSL.frontend.scopeChecker
import DSL.frontend.{ScopeError, UndeclaredVariable, UndeclaredFunction, DuplicateFunction, DuplicateParameter, ReturnOutsideFunction, MissingReturnPath}

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

  it should "report an undeclared function call" in {
    assertErrors(Program(List(
      ExprStmt(Sum(Call("missing", List(IntLiteral(5)))))
    )))(
      UndeclaredFunction("missing")
    )
  }

  /** Exhaustive Return Tests */

  it should "report missing return path if an If statement has no Else" in {
    // func f(x) { if x == 1 { return 1 } }
    assertErrors(Program(List(
      Func("f", List("x"), List(
        If(List(Branch(Nil, Eq(Ident("x"), IntLiteral(1)), List(Return(IntLiteral(1))))), None)
      ))
    )))(
      MissingReturnPath("f")
    )
  }

  it should "accept If-Else if both paths return" in {
    assertNoErrors(Program(List(
      Func("f", List("x"), List(
        If(
          branches = List(Branch(Nil, Eq(Ident("x"), IntLiteral(1)), List(Return(IntLiteral(1))))), 
          elseBody = Some(List(Return(IntLiteral(0))))
        )
      ))
    )))
  }

  it should "accept function if return is guaranteed after an incomplete If" in {
    // func f(x) { if x == 1 { return 1 } return 0 }
    assertNoErrors(Program(List(
      Func("f", List("x"), List(
        If(List(Branch(Nil, Eq(Ident("x"), IntLiteral(1)), List(Return(IntLiteral(1))))), None),
        Return(IntLiteral(0))
      ))
    )))
  }

  /** RollBinding Tests */

  it should "register variables from RollBindings in the If header" in {
    // func f(x) { if v = ~x; v == 6 { return v } else { return 0 } }
    assertNoErrors(Program(List(
      Func("f", List("x"), List(
        If(
          branches = List(
            Branch(
              bindings = List(RollBinding("v", Ident("x"))),
              condition = Eq(Ident("v"), IntLiteral(6)),
              body = List(Return(Ident("v")))
            )
          ),
          elseBody = Some(List(Return(IntLiteral(0))))
        )
      ))
    )))
  }

  it should "report undeclared variable inside RollBinding expression" in {
    // if v = ~x; 1 { } where x is undeclared
    assertErrors(Program(List(
      If(
        branches = List(
          Branch(
            bindings = List(RollBinding("v", Ident("x"))),
            condition = IntLiteral(1),
            body = List(ExprStmt(IntLiteral(1)))
          )
        ),
        elseBody = None
      )
    )))(
      UndeclaredVariable("x")
    )
  }

  it should "allow variables from earlier branches to be used in later elifs" in {
    // if v = ~d6; v == 1 { return 1 } elif v == 2 { return 2 } else { return 3 }
    assertNoErrors(Program(List(
      Func("f", Nil, List(
        If(
          branches = List(
            Branch(List(RollBinding("v", Dice(IntLiteral(1), IntLiteral(6)))), Eq(Ident("v"), IntLiteral(1)), List(Return(IntLiteral(1)))),
            Branch(Nil, Eq(Ident("v"), IntLiteral(2)), List(Return(IntLiteral(2))))
          ),
          elseBody = Some(List(Return(IntLiteral(3))))
        )
      ))
    )))
  }
}