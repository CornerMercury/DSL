package DSL.backend

import DSL.frontend.AST._
import typedAST._

object interpreter {

  /** Backwards-compatible entry point: type-check, then interpret. */
  def interpret(expr: Expr): Distribution =
    interpret(expr, DefaultDistributionSemantics)

  def interpret(expr: Expr, sem: DistributionSemantics): Distribution = expr match {
    case Sum(_) | Prod(_) =>
      val typed = typer.annotate(expr)
      interpretTyped(typed, sem)
    case other =>
      throw new IllegalArgumentException(s"Root must be Sum or Prod. Got: $other")
  }

  /** Interpret an already-typed AST. */
  def interpretTyped(expr: TyExpr): Distribution =
    interpretTyped(expr, DefaultDistributionSemantics)

  def interpretTyped(expr: TyExpr, sem: DistributionSemantics): Distribution = expr match {
    case TyUnary(UnaryOp.Sum, inner, _)  => eval(inner, DiceMode.Sum, sem)
    case TyUnary(UnaryOp.Prod, inner, _) => eval(inner, DiceMode.Prod, sem)
    case other =>
      throw new IllegalArgumentException(s"Root must be sum or prod. Got: $other")
  }

  private def eval(expr: TyExpr, mode: DiceMode, sem: DistributionSemantics): Distribution = expr match {
    case TyIntLiteral(n, _) =>
      sem.scalar(n)

    case TyCustomDist(raw, _) =>
      sem.custom(raw)

    case TyUnary(UnaryOp.Sum, inner, _) =>
      eval(inner, DiceMode.Sum, sem)

    case TyUnary(UnaryOp.Prod, inner, _) =>
      eval(inner, DiceMode.Prod, sem)

    case TyBinary(BinaryOp.Dice, c, s, _) =>
      val dC = eval(c, mode, sem)
      val dS = eval(s, mode, sem)
      sem.dice(dC, dS, mode)

    case TyBinary(BinaryOp.Add, l, r, _) =>
      val dL = eval(l, mode, sem)
      val dR = eval(r, mode, sem)
      sem.add(dL, dR)

    case TyBinary(BinaryOp.Sub, l, r, _) =>
      val dL = eval(l, mode, sem)
      val dR = eval(r, mode, sem)
      sem.sub(dL, dR)

    case TyBinary(BinaryOp.Mul, l, r, _) =>
      val dL = eval(l, mode, sem)
      val dR = eval(r, mode, sem)
      sem.mul(dL, dR)

    case TyBinary(BinaryOp.Div, l, r, _) =>
      val dL = eval(l, mode, sem)
      val dR = eval(r, mode, sem)
      sem.div(dL, dR)
  }
}