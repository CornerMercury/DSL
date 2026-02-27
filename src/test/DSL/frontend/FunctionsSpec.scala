package DSL

import DSL.frontend.AST._

class FunctionsSpec extends ParserSpecHelper {

  "Functions" should "parse func name(arg1, arg2, ...) { stmt; ...; return ... }" in {
    val expected: AstNode = Program(
      List(
        Func("f", List("a", "b"), List(
          Assign("x", IntLiteral(1)),
          Return(Ident("x"))
        ))
      )
    )
    assertParse("func f(a, b) { x = 1; return x }", expected)
  }

  it should "parse function with no params" in {
    val expected: AstNode = Program(
      List(Func("id", List(), List(Return(IntLiteral(42)))))
    )
    assertParse("func id() { return 42 }", expected)
  }

  it should "parse function with one param" in {
    val expected: AstNode = Program(
      List(Func("one", List("x"), List(Return(Ident("x")))))
    )
    assertParse("func one(x) { return x }", expected)
  }

  it should "parse function body with multiple statements" in {
    val expected: AstNode = Program(
      List(Func("add", List("a", "b"), List(
        Assign("s", Add(Ident("a"), Ident("b"))),
        Return(Ident("s"))
      )))
    )
    assertParse("func add(a, b) { s = a + b; return s }", expected)
  }
}
