package DSL.backend

import DSL.frontend.AST._

object optimiser {
  def optimise(node: AstNode): AstNode = node match {
    case e: Expr => optimiseExpr(e)
    case other   => other
  }

  private def optimiseExpr(node: Expr): Expr = node match {
    case Add(l, r) => foldAdd(optimiseExpr(l), optimiseExpr(r))
    case Sub(l, r) => foldSub(optimiseExpr(l), optimiseExpr(r))
    case Mul(l, r) => foldMul(optimiseExpr(l), optimiseExpr(r))
    case Div(l, r) => foldDiv(optimiseExpr(l), optimiseExpr(r))
    
    case Dice(c, s) => Dice(optimiseExpr(c), optimiseExpr(s))

    case other => other
  }

  private def foldAdd(l: Expr, r: Expr): Expr = (l, r) match {
    case (IntLiteral(a), IntLiteral(b)) => IntLiteral(a + b)
    case (x, IntLiteral(0)) => x
    case (IntLiteral(0), x) => x
    case _ => Add(l, r)
  }

  private def foldSub(l: Expr, r: Expr): Expr = (l, r) match {
    case (IntLiteral(a), IntLiteral(b)) => IntLiteral(a - b)
    case (x, IntLiteral(0)) => x
    case _ => Sub(l, r)
  }

  private def foldMul(l: Expr, r: Expr): Expr = (l, r) match {
    case (IntLiteral(a), IntLiteral(b)) => IntLiteral(a * b)
    case (_, IntLiteral(0)) => IntLiteral(0)
    case (IntLiteral(0), _) => IntLiteral(0)
    case (x, IntLiteral(1)) => x
    case (IntLiteral(1), x) => x
    case _ => Mul(l, r)
  }

  private def foldDiv(l: Expr, r: Expr): Expr = (l, r) match {
    case (IntLiteral(a), IntLiteral(b)) if b != 0 => IntLiteral(a / b)
    case (x, IntLiteral(1)) => x
    case _ => Div(l, r)
  }
}