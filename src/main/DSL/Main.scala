package DSL

import java.io.File
import scala.io.Source
import parsley.{Success, Failure}
import DSL.frontend.parser
import DSL.frontend.stdlib
import DSL.frontend.AST._
import DSL.frontend.scopeChecker
import DSL.backend.optimiser
import DSL.backend.typeChecker
import DSL.backend.interpreter
import DSL.backend.typedAST._
import DSL.backend._

object ExitCode {
  val Success = 0
  val FileErr = 1
  val SyntaxErr = 100
  val ScopeErr = 101
  val TypeErr = 102
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

  val fullInput = stdlib.source + "\n" + input

  // Pipeline: parse -> scope check -> optimise -> type check (with typing) -> interpreting
  parser.parse(fullInput) match {
    case Success(p: Program) =>
      val scopeErrors = scopeChecker.check(p)
      if (scopeErrors.nonEmpty) {
        val errorMsg = scopeErrors.map(e => s"  - $e").mkString("\n")
        return (s"Scope Errors Found:\n$errorMsg", ExitCode.ScopeErr)
      }

      // Optimise the untyped AST
      val optimised = optimiser.optimise(p)

      // Type Check AND Convert to Typed AST
      typeChecker.check(optimised) match {
        case Left(typeErrors) =>
          val errorMsg = typeErrors.map(e => s"  - $e").mkString("\n")
          (s"Type Errors Found:\n$errorMsg", ExitCode.TypeErr)
        
        case Right(typedProgram) =>
          // Interpret the Typed AST
          try {
            val dists = interpreter.interpretProgram(typedProgram)
            val blocks = dists.zipWithIndex.map { case (dist, idx) =>
              val distLines = dist.toSeq.sortBy(_._1).map { case (v, p) => f"  $v%6d  ${p * 100}%6.2f%%" }
              s"Result ${idx + 1}:\n" + distLines.mkString("\n")
            }
            val distBlock = "Distributions (value → probability):\n" + blocks.mkString("\n\n")
            (s"Compilation Successful.\n$distBlock", ExitCode.Success)
          } catch {
            case e: Exception => (s"Runtime Error: ${e.getMessage}", ExitCode.SyntaxErr)
          }
      }

    case Failure(err) =>
      (s"Syntax Error:\n$err", ExitCode.SyntaxErr)
  }
}