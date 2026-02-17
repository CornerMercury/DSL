package DSL.frontend

import parsley.Parsley
import parsley.character.char
import parsley.errors.ErrorBuilder
import parsley.{Success, Failure, Result}
import parsley.expr.{precedence, Ops, InfixL}
import parsley.combinator.sepBy // Needed for comma separation

import DSL.frontend.lexer.implicits.implicitSymbol
import DSL.frontend.lexer.{integer, double, fully, sumKeyword, prodKeyword}
import DSL.frontend.AST._

object parser {
  def parse[Err: ErrorBuilder](input: String): Result[String, AstNode] = {
    parser.parse(input) match {
      case p @ Success(_) => p
      case Failure(msg)   => Failure(msg.toString)
    }
  }

  private def wrapInSumIfNeeded(e: Expr): Expr = e match {
    case _: Sum | _: Prod => e
    case _                => Sum(e)
  }

  private lazy val parser: Parsley[AstNode] = fully(expr).map(wrapInSumIfNeeded)

  /** Expressions */
  
  private val diceOp = char('d')

  private lazy val term: Parsley[Expr] = precedence[Expr](atom)(
    Ops(InfixL)(diceOp #> Dice.apply),
    Ops(InfixL)("*" #> Mul.apply, "/" #> Div.apply)
  )

  private lazy val expr: Parsley[Expr] = precedence[Expr](term)(
    Ops(InfixL)(
      "+" #> Add.apply,
      "-" #> Sub.apply
    )
  )

  /** Atoms */

  // Add customDistLiteral to the atoms
  private lazy val atom: Parsley[Expr] = 
    literal <|> customDistLiteral <|> sumCall <|> prodCall <|> prefixDice <|> parens

  private lazy val literal: Parsley[IntLiteral] = {
    integer.map(n => IntLiteral(n))
  }

  /** 
   * NEW: Parses { 1: 0.1, 2: 0.5, ... } 
   */
  private lazy val customDistLiteral: Parsley[CustomDist] = {
    // A single entry: "1 : 0.5"
    val entry = integer <~> (":" ~> double)
    
    // Comma separated entries inside braces
    ("{" ~> sepBy(entry, ",") <~ "}").map { entries =>
      CustomDist(entries.toMap)
    }
  }

  private lazy val sumCall: Parsley[Sum] = {
    (sumKeyword ~> (("(" ~> expr <~ ")") <|> term)).map(Sum.apply)
  }

  private lazy val prodCall: Parsley[Prod] = {
    (prodKeyword ~> (("(" ~> expr <~ ")") <|> term)).map(Prod.apply)
  }

  private lazy val prefixDice: Parsley[Dice] = {
    diceOp ~> atom.map(sides => Dice(IntLiteral(1), sides))
  }

  private lazy val parens: Parsley[Expr] = "(" ~> expr <~ ")"
}