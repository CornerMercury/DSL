package DSL

import DSL.frontend.AST._

class IfSpec extends ParserSpecHelper {

  "If statements" should "parse a basic if else" in {
    val expected = Program(List(
      Right(IfExpr(
        branches = List(
          IfBranch(Nil, Ident("x"), Block(Nil, IntLiteral(1)))
        ),
        elseBranch = Block(Nil, IntLiteral(0))
      ))
    ))
    assertParse("if x { 1 } else { 0 }", expected)
  }

  it should "parse an if with roll bindings" in {
    val expected = Program(List(
      Right(IfExpr(
        branches = List(
          IfBranch(
            bindings = List(RollBinding("v", Ident("x"))),
            condition = Eq(Ident("v"), IntLiteral(6)),
            body = Block(Nil, Ident("v"))
          )
        ),
        elseBranch = Block(Nil, IntLiteral(0))
      ))
    ))
    assertParse("if v = ~x; v == 6 { v } else { 0 }", expected)
  }

  it should "parse an if-elif-else with multiple bindings" in {
    val expected = Program(List(
      Right(IfExpr(
        branches = List(
          IfBranch(
            bindings = List(RollBinding("v", Ident("x")), RollBinding("w", Ident("y"))),
            condition = Eq(Ident("v"), Ident("w")),
            body = Block(List(Assign("a", IntLiteral(10))), Ident("a"))
          ),
          IfBranch(
            bindings = Nil,
            condition = Eq(Ident("x"), IntLiteral(2)),
            body = Block(List(Assign("a", IntLiteral(20))), Ident("a"))
          )
        ),
        elseBranch = Block(List(Assign("a", IntLiteral(0))), Ident("a"))
      ))
    ))
    assertParse(
      """
      if v = ~x; w = ~y; v == w { a = 10; a } 
      elif x == 2 { a = 20; a } 
      else { a = 0; a }
      """, expected)
  }
}