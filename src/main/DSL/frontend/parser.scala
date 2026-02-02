package DSL.frontend

import parsley.Parsley
import parsley.character.char
import parsley.errors.ErrorBuilder
import parsley.{Success, Failure, Result}
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

  private lazy val parser: Parsley[AstNode] = fully(expr)

  /** ******************************* 
    * Expression Parser
    * ******************************* */
  
  private val diceOp = char('d')

  // ORDER MATTERS HERE: Top = Tightest Binding
  private lazy val expr: Parsley[Expr] = precedence[Expr](atom)(
    Ops(InfixL)(
      diceOp #> Dice.apply 
    ),
    Ops(InfixL)(
      "*" #> Mul.apply,
      "/" #> Div.apply
    ),
    Ops(InfixL)(
      "+" #> Add.apply,
      "-" #> Sub.apply
    )
  )

  /** ******************************* 
    * Atoms
    * ******************************* */

  private lazy val atom: Parsley[Expr] = literal <|> prefixDice <|> parens

  private lazy val literal: Parsley[IntLiteral] = {
    integer.map(n => IntLiteral(n))
  }

  private lazy val prefixDice: Parsley[Dice] = {
    diceOp ~> atom.map(sides => Dice(IntLiteral(1), sides))
  }

  private lazy val parens: Parsley[Expr] = "(" ~> expr <~ ")"
}