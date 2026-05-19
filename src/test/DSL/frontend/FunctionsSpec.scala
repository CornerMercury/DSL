package DSL

import DSL.frontend.AST._

class FunctionsSpec extends ParserSpecHelper {

  "Functions" should "parse func name(arg1, arg2, ...) { stmt; ...; expr }" in {
    val expected: AstNode = Program(
      List(
        Left(Func("f", List(Param("a", None), Param("b", None)), Block(
          List(Assign("x", IntLiteral(1))),
          Ident("x")
        )))
      )
    )
    assertParse("func f(a, b) { x = 1; x }", expected)
  }

  it should "parse function with no params" in {
    val expected: AstNode = Program(
      List(Left(Func("id", List(), Block(Nil, IntLiteral(42)))))
    )
    assertParse("func id() { 42 }", expected)
  }

  it should "parse function with one param" in {
    val expected: AstNode = Program(
      List(Left(Func("one", List(Param("x", None)), Block(Nil, Ident("x")))))
    )
    assertParse("func one(x) { x }", expected)
  }

  it should "parse function body with multiple statements and a final expression" in {
    val expected: AstNode = Program(
      List(Left(Func("add", List(Param("a", None), Param("b", None)), Block(
        List(Assign("s", Add(Ident("a"), Ident("b")))),
        Ident("s")
      ))))
    )
    assertParse("func add(a, b) { s = a + b; s }", expected)
  }

  it should "parse function with explicit dist type" in {
    val expected: AstNode = Program(
      List(Left(Func("roll", List(Param("d", Some(DistType))), Block(Nil, Ident("d")))))
    )
    assertParse("func roll(d: dist) { d }", expected)
  }

  it should "parse function with explicit pool type" in {
    val expected: AstNode = Program(
      List(Left(Func("hand", List(Param("cards", Some(PoolType))), Block(Nil, Ident("cards")))))
    )
    assertParse("func hand(cards: pool) { cards }", expected)
  }

  it should "parse function with mixed typed and untyped parameters" in {
    val expected: AstNode = Program(
      List(Left(Func("mixed", List(Param("x", None), Param("y", Some(PoolType))), Block(Nil, Ident("x")))))
    )
    assertParse("func mixed(x, y: pool) { x }", expected)
  }
}