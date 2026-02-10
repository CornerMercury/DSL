package DSL

import java.io.File
import scala.io.Source
import parsley.{Success, Failure}
import DSL.frontend.parser
import DSL.frontend.AST.Expr
import DSL.backend.optimiser
import DSL.backend.interpreter

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

def compile(file: File, flags: Seq[String] = Seq.empty): (String, Int) = {
  val input = try {
    Source.fromFile(file).mkString
  } catch {
    case e: Exception => return (s"File Read Error: ${e.getMessage}", ExitCode.FileErr)
  }

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