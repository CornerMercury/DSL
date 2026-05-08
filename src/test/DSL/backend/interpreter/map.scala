package DSL.backend

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.*
import parsley.{Success, Failure}
import DSL.frontend.parser
import DSL.frontend.scopeChecker
import DSL.frontend.AST.Program
import DSL.backend.{interpreter, optimiser, Distribution}

class InterpreterMapSpec extends AnyFlatSpec {

  def runProgram(input: String): List[Distribution] = {
    parser.parse(input) match {
      case Success(p: Program) =>
        val scopeErrors = scopeChecker.check(p)
        assert(scopeErrors.isEmpty, s"Scope errors: $scopeErrors")
        val optimised = optimiser.optimise(p)
        interpreter.interpretProgram(optimised)
      case Failure(err) =>
        fail(s"Parse error: $err")
    }
  }

  "map" should "apply a boolean equality function to a distribution" in {
    val result = runProgram(
      """func f(x) {
        |  x == 1
        |}
        |map(f, d6)
        |""".stripMargin
    )
    val dist = result.head
    dist.size shouldBe 2
    dist(0) shouldBe 5.0 / 6.0 +- 1e-9
    dist(1) shouldBe 1.0 / 6.0 +- 1e-9
  }

  it should "apply an arithmetic transformation to a distribution" in {
    val result = runProgram(
      """func double(x) {
        |  x + x
        |}
        |map(double, d3)
        |""".stripMargin
    )
    val dist = result.head
    // d3 produces 1, 2, 3. Double produces 2, 4, 6.
    dist.size shouldBe 3
    dist(2) shouldBe 1.0 / 3.0 +- 1e-9
    dist(4) shouldBe 1.0 / 3.0 +- 1e-9
    dist(6) shouldBe 1.0 / 3.0 +- 1e-9
  }

  it should "allow the mapped function to use variables from the outer scope" in {
    val result = runProgram(
      """y = 10
        |func addY(x) {
        |  x + y
        |}
        |map(addY, {1:0.5, 2:0.5})
        |""".stripMargin
    )
    val dist = result.head
    // 1 + 10 = 11, 2 + 10 = 12
    dist.size shouldBe 2
    dist(11) shouldBe 0.5 +- 1e-9
    dist(12) shouldBe 0.5 +- 1e-9
  }

  it should "handle mapping over a scalar distribution" in {
    val result = runProgram(
      """func isEven(x) {
        |  x == 2
        |}
        |map(isEven, 2)
        |""".stripMargin
    )
    val dist = result.head
    dist.size shouldBe 1
    dist(1) shouldBe 1.0 +- 1e-9
  }

  it should "collapse multiple inputs into the same output correctly" in {
    val result = runProgram(
      """func mod2(x) {
        |  x == 0
        |}
        |map(mod2, {0:0.2, 1:0.3, 2:0.5})
        |""".stripMargin
    )
    val dist = result.head
    dist.size shouldBe 2
    dist(0) shouldBe 0.8 +- 1e-9
    dist(1) shouldBe 0.2 +- 1e-9
  }

  it should "throw an error if the mapped function expects more than 1 argument" in {
    val input =
      """func add(x, y) {
        |  x + y
        |}
        |map(add, d6)
        |""".stripMargin
    
    val ex = intercept[IllegalArgumentException] {
      runProgram(input)
    }
    ex.getMessage should include ("expects 1 argument for map")
  }
}