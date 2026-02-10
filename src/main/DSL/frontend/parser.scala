package DSL.frontend

import parsley.Parsley
import parsley.character.char
import parsley.errors.ErrorBuilder
import parsley.{Success, Failure, Result}
import parsley.expr.{precedence, Ops, InfixL}

import DSL.frontend.lexer.implicits.implicitSymbol
import DSL.frontend.lexer.{integer, fully, sumKeyword, prodKeyword}
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

  // Term: dice (default to sum), *, / (no +, -). Used so sum/prod 2d6 bind one term.
  private lazy val term: Parsley[Expr] = precedence[Expr](atom)(
    Ops(InfixL)(diceOp #> ((c: Expr, s: Expr) => Sum(Dice(c, s)))),
    Ops(InfixL)("*" #> Mul.apply, "/" #> Div.apply)
  )

  // ORDER MATTERS HERE: Top = Tightest Binding
  private lazy val expr: Parsley[Expr] = precedence[Expr](term)(
    Ops(InfixL)(
      "+" #> Add.apply,
      "-" #> Sub.apply
    )
  )

  /** ******************************* 
    * Atoms
    * ******************************* */

  private lazy val atom: Parsley[Expr] = literal <|> sumCall <|> prodCall <|> prefixDice <|> parens

  private lazy val literal: Parsley[IntLiteral] = {
    integer.map(n => IntLiteral(n))
  }

  // sum(expr) or sum expr (Haskell-style: without parens, binds to one term)
  private lazy val sumCall: Parsley[Sum] = {
    (sumKeyword ~> (("(" ~> expr <~ ")") <|> term)).map(Sum.apply)
  }

  // prod(expr) or prod expr (product of dice)
  private lazy val prodCall: Parsley[Prod] = {
    (prodKeyword ~> (("(" ~> expr <~ ")") <|> term)).map(Prod.apply)
  }

  // prefix dN defaults to sum (1dN)
  private lazy val prefixDice: Parsley[Sum] = {
    diceOp ~> atom.map(sides => Sum(Dice(IntLiteral(1), sides)))
  }

  private lazy val parens: Parsley[Expr] = "(" ~> expr <~ ")"
}