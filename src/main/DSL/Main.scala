package DSL

import java.io.File
import scala.io.Source
import parsley.{Success, Failure}
import DSL.frontend.parser

// Define Exit codes to handle the status integers
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
    case Success(ast) => 
      (s"Parsed successfully:\n$ast", ExitCode.Success)
    case Failure(err) => 
      (s"Syntax Error:\n$err", ExitCode.SyntaxErr)
  }
}