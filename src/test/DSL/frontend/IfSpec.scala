package DSL

import DSL.frontend.AST._

class IfSpec extends ParserSpecHelper {

  "If statements" should "parse a basic if" in {
    val expected = Program(List(
      If(
        branches = List((Ident("x"), List(Return(IntLiteral(1))))), 
        elseBody = None
      )
    ))
    assertParse("if x { return 1 }", expected)
  }

  it should "parse an if-else" in {
    val expected = Program(List(
      If(
        branches = List((Ident("x"), List(Return(IntLiteral(1))))), 
        elseBody = Some(List(Return(IntLiteral(0))))
      )
    ))
    assertParse("if x { return 1 } else { return 0 }", expected)
  }

  it should "parse an if-elif-else with comparison operators" in {
    val expected = Program(List(
      If(
        branches = List(
          (Eq(Ident("x"), IntLiteral(1)), List(Assign("a", IntLiteral(10)))), 
          (IdenEq(Ident("x"), IntLiteral(2)), List(Assign("a", IntLiteral(20))))
        ), 
        elseBody = Some(List(Assign("a", IntLiteral(0))))
      )
    ))
    assertParse(
      """
      if x == 1 { a = 10 } 
      elif x === 2 { a = 20 } 
      else { a = 0 }
      """, expected)
  }

  it should "handle multiple elif branches correctly" in {
    val expected = Program(List(
      If(
        branches = List(
          (IntLiteral(1), List(ExprStmt(Sum(IntLiteral(1))))),
          (IntLiteral(2), List(ExprStmt(Sum(IntLiteral(2))))),
          (IntLiteral(3), List(ExprStmt(Sum(IntLiteral(3)))))
        ),
        elseBody = None
      )
    ))
    assertParse("if 1 { 1 } elif 2 { 2 } elif 3 { 3 }", expected)
  }

  it should "parse nested if statements within a function" in {
    val expected = Program(List(
      Func("check", List("x"), List(
        If(
          branches = List(
            (Ident("x"), List(
              If(
                branches = List((Ident("y"), List(Return(IntLiteral(1))))),
                elseBody = None
              )
            ))
          ),
          elseBody = Some(List(Return(IntLiteral(0))))
        )
      ))
    ))
    assertParse(
      """
      func check(x) {
        if x {
          if y { return 1 }
        } else {
          return 0
        }
      }
      """, expected)
  }
}