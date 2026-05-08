package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import DSL.frontend.AST._
import DSL.frontend.scopeChecker
import DSL.frontend.{ScopeError, UndeclaredVariable, UndeclaredFunction, DuplicateFunction, DuplicateParameter}

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
      Left(Assign("x", IntLiteral(1))),
      Right(Sum(Add(Ident("x"), IntLiteral(2))))
    )))
  }

  it should "report undeclared variable" in {
    assertErrors(Program(List(Right(Sum(Ident("x"))))))(
      UndeclaredVariable("x")
    )
  }

  it should "allow shadowing in function body" in {
    assertNoErrors(Program(List(
      Left(Assign("x", IntLiteral(1))),
      Left(Func("f", List("x"), Block(Nil, Ident("x")))),
      Right(Sum(Ident("x")))
    )))
  }

  it should "report undeclared variable in function body" in {
    assertErrors(Program(List(
      Left(Func("f", List("a"), Block(
        List(Assign("b", Add(Ident("a"), Ident("c")))),
        Ident("b")
      )))
    )))(
      UndeclaredVariable("c")
    )
  }

  it should "report duplicate function name" in {
    assertErrors(Program(List(
      Left(Func("f", List(), Block(Nil, IntLiteral(0)))),
      Left(Func("f", List(), Block(Nil, IntLiteral(1))))
    )))(
      DuplicateFunction("f")
    )
  }

  it should "report duplicate parameter name" in {
    assertErrors(Program(List(
      Left(Func("f", List("x", "x"), Block(Nil, IntLiteral(0))))
    )))(
      DuplicateParameter("x")
    )
  }

  it should "report an undeclared function call" in {
    assertErrors(Program(List(
      Right(Sum(Call("missing", List(IntLiteral(5)))))
    )))(
      UndeclaredFunction("missing")
    )
  }

  /** RollBinding Tests */

  it should "register variables from RollBindings in the If header" in {
    // func f(x) { if v = ~x; v == 6 { v } else { 0 } }
    assertNoErrors(Program(List(
      Left(Func("f", List("x"), Block(Nil, IfExpr(
        branches = List(
          IfBranch(
            bindings = List(RollBinding("v", Ident("x"))),
            condition = Eq(Ident("v"), IntLiteral(6)),
            body = Block(Nil, Ident("v"))
          )
        ),
        elseBranch = Block(Nil, IntLiteral(0))
      )))
    ))))
  }

  it should "report undeclared variable inside RollBinding expression" in {
    // if v = ~x; 1 { 1 } else { 0 } where x is undeclared
    assertErrors(Program(List(
      Right(IfExpr(
        branches = List(
          IfBranch(
            bindings = List(RollBinding("v", Ident("x"))),
            condition = IntLiteral(1),
            body = Block(Nil, IntLiteral(1))
          )
        ),
        elseBranch = Block(Nil, IntLiteral(0))
      ))
    )))(
      UndeclaredVariable("x")
    )
  }

  it should "allow variables from earlier branches to be used in later elifs" in {
    // func f() { if v = ~d6; v == 1 { 1 } elif v == 2 { 2 } else { 3 } }
    assertNoErrors(Program(List(
      Left(Func("f", Nil, Block(Nil, IfExpr(
        branches = List(
          IfBranch(
            bindings = List(RollBinding("v", Dice(IntLiteral(1), IntLiteral(6)))),
            condition = Eq(Ident("v"), IntLiteral(1)),
            body = Block(Nil, IntLiteral(1))
          ),
          IfBranch(
            bindings = Nil,
            condition = Eq(Ident("v"), IntLiteral(2)),
            body = Block(Nil, IntLiteral(2))
          )
        ),
        elseBranch = Block(Nil, IntLiteral(3))
      )))
    ))))
  }

  /** MapExpr Tests */

  it should "accept map with a declared function" in {
    // func f(x) { x == 1 }
    // map(f, d6)
    assertNoErrors(Program(List(
      Left(Func("f", List("x"), Block(Nil, Eq(Ident("x"), IntLiteral(1))))),
      Right(MapExpr("f", Dice(IntLiteral(1), IntLiteral(6))))
    )))
  }

  it should "report undeclared function in map" in {
    // map(missing, d6)
    assertErrors(Program(List(
      Right(MapExpr("missing", Dice(IntLiteral(1), IntLiteral(6))))
    )))(
      UndeclaredFunction("missing")
    )
  }

  it should "report undeclared variable in map inner expression" in {
    // func f(x) { x }
    // map(f, y) where y is undeclared
    assertErrors(Program(List(
      Left(Func("f", List("x"), Block(Nil, Ident("x")))),
      Right(MapExpr("f", Ident("y")))
    )))(
      UndeclaredVariable("y")
    )
  }
}