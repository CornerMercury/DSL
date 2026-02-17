package DSL.backend

import typedAST._
import interpreter.{
  convolve,
  convolveSub,
  convolveDiv,
  combineDiceSum,
  combineDiceProd,
}

/** Interprets the typed AST into a distribution. Uses the attached DistTy for specialization. */
object typedInterpreter {

  def interpret(tyExpr: TyExpr): Distribution = tyExpr match {
    case TySum(inner, _)   => evalSum(inner)
    case TyProd(inner, _)  => evalProd(inner)
    case other            => throw new IllegalArgumentException(s"Typed interpreter expects root Sum or Prod, got: $other")
  }

  private def evalSum(expr: TyExpr): Distribution = expr match {
    case TyIntLiteral(n, _)     => Map(n -> 1.0)
    case TyDice(tyC, tyS, _)    => combineDiceSum(evalSum(tyC), evalSum(tyS))
    case TySum(inner, _)        => evalSum(inner)
    case TyProd(inner, _)       => evalProd(inner)
    case TyAdd(l, r, _)         => convolve(evalSum(l), evalSum(r), _ + _)
    case TySub(l, r, _)         => convolveSub(evalSum(l), evalSum(r))
    case TyMul(l, r, _)         => convolve(evalSum(l), evalSum(r), _ * _)
    case TyDiv(l, r, _)         => convolveDiv(evalSum(l), evalSum(r))
  }

  private def evalProd(expr: TyExpr): Distribution = expr match {
    case TyIntLiteral(n, _)     => Map(n -> 1.0)
    case TyDice(tyC, tyS, _)    => combineDiceProd(evalProd(tyC), evalProd(tyS))
    case TySum(inner, _)       => evalSum(inner)
    case TyProd(inner, _)      => evalProd(inner)
    case TyAdd(l, r, _)        => convolve(evalProd(l), evalProd(r), _ + _)
    case TySub(l, r, _)        => convolveSub(evalProd(l), evalProd(r))
    case TyMul(l, r, _)        => convolve(evalProd(l), evalProd(r), _ * _)
    case TyDiv(l, r, _)        => convolveDiv(evalProd(l), evalProd(r))
  }
}
