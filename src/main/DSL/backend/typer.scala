package DSL.backend

import DSL.frontend.AST._
import DSL.backend.typedAST._
import semanticTypes._

object typer {

  def annotate(expr: Expr): TyExpr =
    infer(expr)

  private def infer(expr: Expr): TyExpr = expr match {
    case Ident(name) =>
      TyIdent(name, UnknownTy)

    case IntLiteral(n) =>
      TyIntLiteral(n, ScalarTy)

    case CustomDist(dist) =>
      TyCustomDist(dist, semanticTypes.classify(dist))

    case Call(name, args) =>
      val tArgs = args.map(infer)
      TyCall(name, tArgs, GenericDistTy)

    case Sum(inner) =>
      val tInner = infer(inner)
      TyUnary(UnaryOp.Sum, tInner, unaryResultType(tInner.ty))

    case Prod(inner) =>
      val tInner = infer(inner)
      TyUnary(UnaryOp.Prod, tInner, unaryResultType(tInner.ty))

    case Dice(c, s) =>
      val tC = infer(c)
      val tS = infer(s)
      TyBinary(BinaryOp.Dice, tC, tS, diceResultType(tC.ty, tS.ty))

    case Add(l, r) =>
      val tL = infer(l)
      val tR = infer(r)
      TyBinary(BinaryOp.Add, tL, tR, combineNumeric(tL.ty, tR.ty))

    case Sub(l, r) =>
      val tL = infer(l)
      val tR = infer(r)
      TyBinary(BinaryOp.Sub, tL, tR, combineNumeric(tL.ty, tR.ty))

    case Mul(l, r) =>
      val tL = infer(l)
      val tR = infer(r)
      TyBinary(BinaryOp.Mul, tL, tR, combineNumeric(tL.ty, tR.ty))

    case Div(l, r) =>
      val tL = infer(l)
      val tR = infer(r)
      TyBinary(BinaryOp.Div, tL, tR, combineNumeric(tL.ty, tR.ty))
  }

  private def unaryResultType(inner: DistTy): DistTy = inner match {
    case UnknownTy => UnknownTy
    case other     => other
  }

  private def diceResultType(countTy: DistTy, sidesTy: DistTy): DistTy = (countTy, sidesTy) match {
    case (UnknownTy, _) | (_, UnknownTy) => UnknownTy
    case _                               => GenericDistTy
  }

  private def combineNumeric(t1: DistTy, t2: DistTy): DistTy = (t1, t2) match {
    case (UnknownTy, _) | (_, UnknownTy) => UnknownTy
    case (ScalarTy, ScalarTy)            => ScalarTy
    case _                               => GenericDistTy
  }
}