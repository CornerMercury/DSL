package DSL

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}
import DSL.frontend.AST._
import DSL.frontend.parser

/** Shared helper for frontend parser specs. */
trait ParserSpecHelper extends AnyFlatSpec {

  def assertParseExpr(input: String, expected: AstNode): Unit = {
    parser.parse(input) match {
      case Success(actual) =>
        actual.topLevel.head shouldBe Right(expected)
      case Failure(msg) =>
        fail(s"Parser failed to parse input: '$input'\nParse Error:\n$msg")
    }
  }

  def assertParse(input: String, expected: AstNode): Unit = {
    parser.parse(input) match {
      case Success(actual) =>
        actual shouldBe expected
      case Failure(msg) =>
        fail(s"Parser failed to parse input: '$input'\nParse Error:\n$msg")
    }
  }
}