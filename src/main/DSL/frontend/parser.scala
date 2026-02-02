package DSL.frontend

import parsley.Parsley
import parsley.errors.ErrorBuilder
import parsley.{Success, Failure, Result}

// 1. Import Expression parsing tools
import parsley.expr.{precedence, Ops, InfixL}

import DSL.frontend.lexer.implicits.implicitSymbol
import DSL.frontend.lexer.{integer, fully}
import DSL.frontend.AST._

object parser {
  def parse[Err: ErrorBuilder](input: String): Result[String, AstNode] = {
    parser.parse(input) match {
      case p @ Success(_) => p
      case Failure(msg)   => Failure(msg.toString)
    }
  }

  // The top-level parser is now an expression
  private lazy val parser: Parsley[AstNode] = fully(expr)

  /** ******************************* 
    * Expression Parser (Precedence)
    * ******************************* */
  
  private lazy val expr: Parsley[Expr] = precedence[Expr](atom)(
    Ops(InfixL)(
      "+" #> Add.apply,
      "-" #> Sub.apply 
    )
  )

  /** ******************************* 
    * Atoms (Basic Units)
    * ******************************* */

  private lazy val atom: Parsley[Expr] = die <|> literal

  private lazy val die: Parsley[Die] = {
    ("d" ~> integer).map(sides => Die(sides))
  }

  private lazy val literal: Parsley[IntLiteral] = {
    integer.map(n => IntLiteral(n))
  }
}