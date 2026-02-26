package DSL.frontend

import parsley.Parsley
import parsley.character.char
import parsley.errors.ErrorBuilder
import parsley.{Success, Failure, Result}
import parsley.expr.{precedence, Ops, InfixL}
import parsley.combinator.{sepBy, sepEndBy1}
import parsley.Parsley.atomic

import DSL.frontend.lexer.implicits.implicitSymbol
import DSL.frontend.lexer.{integer, double, identifier, fully, sumKeyword, prodKeyword}
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

  private lazy val parser: Parsley[AstNode] = fully(program)

  /** Program: one or more statements. */
  private lazy val program: Parsley[AstNode] =
    sepEndBy1(stmt, ";").map {
      case List(ExprStmt(e)) => e // Backward compatibility for single expressions
      case ss                => Program(ss)
    }

  private lazy val stmt: Parsley[Stmt] =
    assignStmt <|> exprStmt

  // Use atomic on the left side to backtrack if it's not an assignment (e.g. "x + 1")
  // Commits to assignment once '=' is parsed.
  private lazy val assignStmt: Parsley[Assign] =
    (atomic(identifier <~ "=") <~> expr).map { case (id, e) => Assign(id, e) }

  private lazy val exprStmt: Parsley[ExprStmt] =
    expr.map(e => ExprStmt(wrapInSumIfNeeded(e)))

  /** Expressions */
  
  private val diceOp = char('d')

  // 'd' as infix operator has highest precedence in term (binds tighter than mul/div)
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

  // KEY FIX: atomic(prefixDice) must be checked BEFORE identRef.
  // 'd' is a valid start for an identifier, so identRef would greedily consume "d6".
  // atomic ensures that if prefixDice starts matching 'd' but fails on the following atom,
  // it backtracks to let identRef try.
  private lazy val atom: Parsley[Expr] = 
    literal <|> customDistLiteral <|> sumCall <|> prodCall <|> atomic(prefixDice) <|> identRef <|> parens

  private lazy val literal: Parsley[IntLiteral] = {
    integer.map(n => IntLiteral(n))
  }
  
  private lazy val identRef: Parsley[Ident] =
    identifier.map(Ident.apply)

  private lazy val customDistLiteral: Parsley[CustomDist] = {
    val entry = integer <~> (":" ~> double)
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