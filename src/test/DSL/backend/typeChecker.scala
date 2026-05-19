package DSL.backend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}
import DSL.frontend.parser
import DSL.frontend.scopeChecker
import DSL.frontend.AST.Program
import DSL.backend.{optimiser, typeChecker, TypeError}

class TypeCheckerSpec extends AnyFlatSpec {

  def checkTypes(input: String): List[TypeError] = {
    parser.parse(input) match {
      case Success(p: Program) =>
        val scopeErrors = scopeChecker.check(p)
        assert(scopeErrors.isEmpty, s"Scope errors: $scopeErrors")
        val optimised = optimiser.optimise(p)
        typeChecker.check(optimised)
      case Failure(err) =>
        fail(s"Parse error: $err")
    }
  }

  "typeChecker" should "allow scalar comparisons" in {
    val errors = checkTypes("1 < 2")
    errors shouldBe empty
  }

  it should "allow distribution vs scalar comparisons" in {
    val errors = checkTypes("d6 < 3")
    errors shouldBe empty
  }

  it should "allow scalar vs distribution comparisons" in {
    val errors = checkTypes("3 > d6")
    errors shouldBe empty
  }

  it should "allow distribution vs distribution comparisons" in {
    val errors = checkTypes("d6 <= d6")
    errors shouldBe empty
  }

  it should "allow generic distribution comparisons" in {
    val errors = checkTypes("2d6 < 3")
    errors shouldBe empty
  }

  it should "allow comparisons on assigned distributions" in {
    val errors = checkTypes(
      """x = d6
        |x < 5
        |""".stripMargin
    )
    errors shouldBe empty
  }

  it should "allow equality on distributions" in {
    val errors = checkTypes("d6 == d6")
    errors shouldBe empty
  }

  it should "allow map expressions over distributions" in {
    // map enumerates the distribution, passing scalar outcomes to the function
    val errors = checkTypes(
      """func f(x) { x >= 5 }
        |map(f, d6)
        |""".stripMargin
    )
    errors shouldBe empty
  }

  it should "allow passing distributions to functions that use comparisons" in {
    // Since comparisons are now defined for distributions, functions taking distributions and comparing them are valid
    val errors = checkTypes(
      """func f(x) {
        |  x >= 5
        |}
        |f(sum(d6))
        |""".stripMargin
    )
    errors shouldBe empty
  }
  
  it should "allow functions to return distributions based on comparisons" in {
    val errors = checkTypes(
      """func check(x, y) {
        |  x > y
        |}
        |check(d6, sum(2d6))
        |""".stripMargin
    )
    errors shouldBe empty
  }
}