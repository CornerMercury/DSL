package DSL

import DSL.frontend.AST._

class FunctionsSpec extends ParserSpecHelper {

  "Functions" should "parse func name(arg1, arg2, ...) { stmt; ...; expr }" in {
    val expected: AstNode = Program(
      List(
        Func("f", List("a", "b"), Block(
          List(Assign("x", IntLiteral(1))),
          Ident("x")
        ))
      )
    )
    assertParse("func f(a, b) { x = 1; x }", expected)
  }

  it should "parse function with no params" in {
    val expected: AstNode = Program(
      List(Func("id", List(), Block(Nil, IntLiteral(42))))
    )
    assertParse("func id() { 42 }", expected)
  }

  it should "parse function with one param" in {
    val expected: AstNode = Program(
      List(Func("one", List("x"), Block(Nil, Ident("x"))))
    )
    assertParse("func one(x) { x }", expected)
  }

  it should "parse function body with multiple statements and a final expression" in {
    val expected: AstNode = Program(
      List(Func("add", List("a", "b"), Block(
        List(Assign("s", Add(Ident("a"), Ident("b")))),
        Ident("s")
      )))
    )
    assertParse("func add(a, b) { s = a + b; s }", expected)
  }
}