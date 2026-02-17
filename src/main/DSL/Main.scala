package DSL

import java.io.File
import scala.io.Source
import parsley.{Success, Failure}
import DSL.frontend.parser
import DSL.frontend.AST.Expr
import DSL.backend.optimiser
import DSL.backend.interpreter
import DSL.backend.typedAST._
import DSL.backend.{DistTy, ScalarTy, BinomialTy, UniformTy, GenericDistTy}

object ExitCode {
  val Success = 0
  val FileErr = 1
  val SyntaxErr = 100
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

  // Pipeline: parse -> optimise -> typing & interpreting (single pass)
  parser.parse(input) match {
    case Success(ast: Expr) =>
      val optimised = optimiser.optimise(ast).asInstanceOf[Expr]
      val dist = interpreter.interpret(optimised)
      val distLines = dist.toSeq.sortBy(_._1).map { case (v, p) => f"  $v%6d  ${p * 100}%6.2f%%" }
      val distBlock = "Distribution (value → probability):\n" + distLines.mkString("\n")
      (s"AST: $optimised\n$distBlock", ExitCode.Success)

    case Success(_) =>
      ("Parse succeeded but root is not an expression", ExitCode.SyntaxErr)

    case Failure(err) =>
      (s"Syntax Error:\n$err", ExitCode.SyntaxErr)
  }
}