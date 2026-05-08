package DSL

import java.io.File
import scala.io.Source
import parsley.{Success, Failure}
import DSL.frontend.parser
import DSL.frontend.stdlib
import DSL.frontend.AST._
import DSL.frontend.scopeChecker
import DSL.backend.optimiser
import DSL.backend.interpreter
import DSL.backend.typedAST._
import DSL.backend.{DistTy, ScalarTy, BinomialTy, UniformTy, GenericDistTy}

object ExitCode {
  val Success = 0
  val FileErr = 1
  val SyntaxErr = 100
  val ScopeErr = 101
}

@main
def main(path: String, flags: String*): Unit = {
  val f = new File(path)
  if (!f.exists()) {
    println(s"Error: File '$path' not found.")
    System.exit(ExitCode.FileErr)
  }

  val (msg, code) = compile(f, flags)
  
  println(msg)
  System.exit(code)
}

private def showTy(e: TyExpr): String = {
  def tyName(t: DistTy): String = t match {
    case ScalarTy      => "Scalar"
    case BinomialTy    => "Binomial"
    case UniformTy     => "Uniform"
    case GenericDistTy => "Generic"
  }
  e match {
    case TyIntLiteral(n, t) => s"$n:${tyName(t)}"
    case TyUnary(UnaryOp.Sum, inner, t)  => s"sum(${showTy(inner)}):${tyName(t)}"
    case TyUnary(UnaryOp.Prod, inner, t) => s"prod(${showTy(inner)}):${tyName(t)}"
    case TyBinary(BinaryOp.Dice, c, s, t) => s"dice(${showTy(c)},${showTy(s)}):${tyName(t)}"
    case TyBinary(BinaryOp.Add, l, r, t)  => s"(${showTy(l)}+${showTy(r)}):${tyName(t)}"
    case TyBinary(BinaryOp.Sub, l, r, t)  => s"(${showTy(l)}-${showTy(r)}):${tyName(t)}"
    case TyBinary(BinaryOp.Mul, l, r, t)  => s"(${showTy(l)}*${showTy(r)}):${tyName(t)}"
    case TyBinary(BinaryOp.Div, l, r, t)  => s"(${showTy(l)}/${showTy(r)}):${tyName(t)}"
  }
}

def compile(file: File, flags: Seq[String] = Seq.empty): (String, Int) = {
  val input = try {
    Source.fromFile(file).mkString
  } catch {
    case e: Exception => return (s"File Read Error: ${e.getMessage}", ExitCode.FileErr)
  }

  // Prepend the standard library builtins
  val fullInput = stdlib.source + "\n" + input

  // Pipeline: parse -> scope check -> optimise -> typing & interpreting
  parser.parse(fullInput) match {
    case Success(p: Program) =>
      // 1. Static Analysis
      val scopeErrors = scopeChecker.check(p)
      if (scopeErrors.nonEmpty) {
        val errorMsg = scopeErrors.map(e => s"  - $e").mkString("\n")
        return (s"Scope Errors Found:\n$errorMsg", ExitCode.ScopeErr)
      }

      // 2. Optimisation
      val optimised = optimiser.optimise(p)

      // 3. Interpretation
      try {
        val dists = interpreter.interpretProgram(optimised)
        val blocks = dists.zipWithIndex.map { case (dist, idx) =>
          val distLines = dist.toSeq.sortBy(_._1).map { case (v, p) => f"  $v%6d  ${p * 100}%6.2f%%" }
          s"Result ${idx + 1}:\n" + distLines.mkString("\n")
        }
        val distBlock = "Distributions (value → probability):\n" + blocks.mkString("\n\n")
        (s"AST: $optimised\n$distBlock", ExitCode.Success)
      } catch {
        case e: Exception => (s"Runtime Error: ${e.getMessage}", ExitCode.SyntaxErr)
      }

    case Failure(err) =>
      (s"Syntax Error:\n$err", ExitCode.SyntaxErr)
  }
}