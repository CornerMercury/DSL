package DSL

import DSL.frontend.AST._

class IfSpec extends ParserSpecHelper {

  "If statements" should "parse a basic if" in {
    val expected = Program(List(
      If(
        branches = List(Branch(Nil, Ident("x"), List(Return(IntLiteral(1))))), 
        elseBody = None
      )
    ))
    assertParse("if x { return 1 }", expected)
  }

  it should "parse an if-else" in {
    val expected = Program(List(
      If(
        branches = List(Branch(Nil, Ident("x"), List(Return(IntLiteral(1))))), 
        elseBody = Some(List(Return(IntLiteral(0))))
      )
    ))
    assertParse("if x { return 1 } else { return 0 }", expected)
  }

  it should "parse an if with roll bindings" in {
    val expected = Program(List(
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
    ))
    assertParse("if v = ~x; v == 6 { return v }", expected)
  }

  it should "parse an if-elif-else with multiple bindings" in {
    val expected = Program(List(
      If(
        branches = List(
          Branch(
            bindings = List(RollBinding("v", Ident("x")), RollBinding("w", Ident("y"))),
            condition = Eq(Ident("v"), Ident("w")),
            body = List(Assign("a", IntLiteral(10)))
          ), 
          Branch(
            bindings = Nil,
            condition = Eq(Ident("x"), IntLiteral(2)),
            body = List(Assign("a", IntLiteral(20)))
          )
        ), 
        elseBody = Some(List(Assign("a", IntLiteral(0))))
      )
    ))
    assertParse(
      """
      if v = ~x; w = ~y; v == w { a = 10 } 
      elif x == 2 { a = 20 } 
      else { a = 0 }
      """, expected)
  }
}