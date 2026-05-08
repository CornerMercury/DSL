package DSL.backend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}
import DSL.frontend.parser
import DSL.frontend.scopeChecker
import DSL.frontend.AST.Program
import DSL.backend.{optimiser, typeChecker, TypeError, NonScalarComparison, ArgTypeMismatch, GenericDistTy, ScalarTy, UnknownTy, UniformTy}

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

  it should "report NonScalarComparison for uniform distribution < scalar" in {
    val errors = checkTypes("d6 < 3")
    errors should have size 1
    val err = errors.head.asInstanceOf[NonScalarComparison]
    err.op shouldBe "Lt"
    err.leftTy shouldBe UniformTy
    err.rightTy shouldBe ScalarTy
  }

  it should "report NonScalarComparison for scalar > uniform distribution" in {
    val errors = checkTypes("3 > d6")
    errors should have size 1
    val err = errors.head.asInstanceOf[NonScalarComparison]
    err.op shouldBe "Gt"
    err.leftTy shouldBe ScalarTy
    err.rightTy shouldBe UniformTy
  }

  it should "report NonScalarComparison for uniform distribution <= uniform distribution" in {
    val errors = checkTypes("d6 <= d6")
    errors should have size 1
    val err = errors.head.asInstanceOf[NonScalarComparison]
    err.op shouldBe "Le"
    err.leftTy shouldBe UniformTy
    err.rightTy shouldBe UniformTy
  }

  it should "report NonScalarComparison for generic distribution < scalar" in {
    val errors = checkTypes("2d6 < 3")
    errors should have size 1
    val err = errors.head.asInstanceOf[NonScalarComparison]
    err.op shouldBe "Lt"
    err.leftTy shouldBe GenericDistTy
    err.rightTy shouldBe ScalarTy
  }

  it should "report NonScalarComparison when comparing assigned distribution" in {
    val errors = checkTypes(
      """x = d6
        |x < 5
        |""".stripMargin
    )
    errors should have size 1
    val err = errors.head.asInstanceOf[NonScalarComparison]
    err.leftTy shouldBe UniformTy
    err.rightTy shouldBe ScalarTy
  }

  it should "allow equality on distributions" in {
    val errors = checkTypes("d6 == d6")
    errors shouldBe empty
  }

  it should "allow map expressions over distributions even if func requires scalar" in {
    // map enumerates the distribution, passing scalar outcomes to the function
    val errors = checkTypes(
      """func f(x) { x >= 5 }
        |map(f, d6)
        |""".stripMargin
    )
    errors shouldBe empty
  }

  it should "report ArgTypeMismatch when passing uniform distribution to function requiring scalar" in {
    val errors = checkTypes(
      """func f(x) {
        |  x >= 5
        |}
        |f(d6)
        |""".stripMargin
    )
    errors should have size 1
    errors.head shouldBe an[ArgTypeMismatch]
    val err = errors.head.asInstanceOf[ArgTypeMismatch]
    err.funcName shouldBe "f"
    err.paramName shouldBe "x"
    err.expected shouldBe ScalarTy
    err.actual shouldBe UniformTy
  }

  it should "allow passing scalar to function requiring scalar" in {
    val errors = checkTypes(
      """func f(x) {
        |  x >= 5
        |}
        |f(1)
        |""".stripMargin
    )
    errors shouldBe empty
  }

  it should "report ArgTypeMismatch when passing assigned distribution to function" in {
    val errors = checkTypes(
      """func f(x) {
        |  x >= 5
        |}
        |y = d6
        |f(y)
        |""".stripMargin
    )
    errors should have size 1
    errors.head shouldBe an[ArgTypeMismatch]
    val err = errors.head.asInstanceOf[ArgTypeMismatch]
    err.funcName shouldBe "f"
    err.paramName shouldBe "x"
    err.actual shouldBe UniformTy
  }

  it should "report ArgTypeMismatch only for the invalid call site" in {
    val errors = checkTypes(
      """func f(x) {
        |  x >= 5
        |}
        |f(1)
        |f(d6)
        |""".stripMargin
    )
    errors should have size 1
    errors.head shouldBe an[ArgTypeMismatch]
    val err = errors.head.asInstanceOf[ArgTypeMismatch]
    err.funcName shouldBe "f"
    err.actual shouldBe UniformTy
  }
}