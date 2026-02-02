package DSL.frontend

import parsley.Parsley
import parsley.Parsley.{eof}
import parsley.character.{digit, char, string, item}
import parsley.combinator.{many, some}
import parsley.errors.ErrorBuilder
import parsley.{Success, Failure, Result}

import DSL.frontend.lexer.implicits.implicitSymbol
import DSL.frontend.lexer.{integer, fully}
import DSL.frontend.ast._

object parser {
  def parse[Err: ErrorBuilder](input: String): Result[String, AstNode] = {
    parser.parse(input) match {
      case p @ Success(_) => p
      case Failure(msg)   => Failure(msg.toString)
    }
  }

  private lazy val parser: Parsley[AstNode] = fully(die)

  /********************************* 
    Dice Parsers
    ******************************** */

  private lazy val die: Parsley[Die] = {
    ("d" ~> integer).map(sides => Die(sides))
  }
}