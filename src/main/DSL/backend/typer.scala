package DSL.backend

import DSL.frontend.AST._
import DSL.backend.typedAST._
import semanticTypes._

object typer {

  def annotate(expr: Expr): TyExpr = infer(expr)

  private def infer(expr: Expr): TyExpr = expr match {

    case Ident(name)      => TyIdent(name, UnknownTy)
    case IntLiteral(n)    => TyIntLiteral(n, ScalarTy)
    case CustomDist(dist) => TyCustomDist(dist, classify(dist))
    case Call(name, args) => TyCall(name, args.map(infer), GenericDistTy)

    case Block(stmts, finalExpr) =>
      val tFinal = infer(finalExpr)
      TyBlock(stmts, tFinal, tFinal.ty)

    case IfExpr(branches, elseB) =>
      val tBranches = branches.map { b =>
        TyIfBranch(b.bindings, infer(b.condition), infer(b.body).asInstanceOf[TyBlock])
      }
      val tElse = infer(elseB).asInstanceOf[TyBlock]
      TyIfExpr(tBranches, tElse, tElse.ty)

    case Sum(inner) =>
      val tInner = infer(inner)
      TyUnary(UnaryOp.Sum, tInner, tInner.ty)

    case Prod(inner) =>
      val tInner = infer(inner)
      TyUnary(UnaryOp.Prod, tInner, tInner.ty)

    case Dice(c, s) =>
      TyBinary(BinaryOp.Dice, infer(c), infer(s), GenericDistTy)

    case Add(l, r) => binary(l, r, BinaryOp.Add)
    case Sub(l, r) => binary(l, r, BinaryOp.Sub)
    case Mul(l, r) => binary(l, r, BinaryOp.Mul)
    case Div(l, r) => binary(l, r, BinaryOp.Div)
    case Eq(l, r)  => binary(l, r, BinaryOp.Eq)
  }

  private def binary(l: Expr, r: Expr, op: BinaryOp): TyBinary = {
    val tL = infer(l)
    val tR = infer(r)
    val resTy =
      if (tL.ty == ScalarTy && tR.ty == ScalarTy) ScalarTy
      else GenericDistTy
    TyBinary(op, tL, tR, resTy)
  }
}