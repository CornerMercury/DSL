package DSL

import DSL.frontend.AST._

class IdentifiersAssignmentsSpec extends ParserSpecHelper {
  "Identifiers" should "parse as identifier references" in {
    assertParseExpr("x", Sum(Ident("x")))
    assertParseExpr("foo_bar123", Sum(Ident("foo_bar123")))
  }

  "Assignments" should "parse a simple program with a final expression" in {
    val expected: AstNode = Program(
      List(
        Left(Assign("x", IntLiteral(1))),
        Right(Sum(Add(Ident("x"), IntLiteral(2))))
      )
    )
    assertParse("x = 1; x + 2", expected)
  }

  it should "parse multiple assignments before the final expression" in {
    val expected: AstNode = Program(
      List(
        Left(Assign("x", Dice(IntLiteral(1), IntLiteral(6)))),
        Left(Assign("y", IntLiteral(5))),
        Right(Sum(Add(Ident("x"), Ident("y"))))
      ),
    )
    assertParse("x = d6; y = 5; x + y", expected)
  }
}