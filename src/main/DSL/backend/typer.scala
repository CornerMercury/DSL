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

    case MapExpr(funcName, inner) =>
      val tInner = infer(inner)
      TyMapExpr(funcName, tInner, GenericDistTy)

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
      // If the inner expression is a scalar, Sum just returns that scalar.
      // Otherwise, it aggregates a pool or passes through a distribution -> GenericDistTy
      val resTy = if (tInner.ty == ScalarTy) ScalarTy else GenericDistTy
      TyUnary(UnaryOp.Sum, tInner, resTy)

    case Prod(inner) =>
      val tInner = infer(inner)
      // Similar to Sum: if input is scalar, output is scalar.
      val resTy = if (tInner.ty == ScalarTy) ScalarTy else GenericDistTy
      TyUnary(UnaryOp.Prod, tInner, resTy)

    case Max(inner) =>
      val tInner = infer(inner)
      TyUnary(UnaryOp.Max, tInner, ScalarTy)

    case Min(inner) =>
      val tInner = infer(inner)
      TyUnary(UnaryOp.Min, tInner, ScalarTy)

    case Dice(c, s) =>
      val tC = infer(c)
      // Dice always returns a PoolTy when count is scalar (enforced by checker)
      TyBinary(BinaryOp.Dice, tC, infer(s), PoolTy)

    case Add(l, r) => binary(l, r, BinaryOp.Add)
    case Sub(l, r) => binary(l, r, BinaryOp.Sub)
    case Mul(l, r) => binary(l, r, BinaryOp.Mul)
    case Div(l, r) => binary(l, r, BinaryOp.Div)
    
    case Eq(l, r)  => binaryComp(l, r, BinaryOp.Eq)
    case Lt(l, r)  => binaryComp(l, r, BinaryOp.Lt)
    case Le(l, r)  => binaryComp(l, r, BinaryOp.Le)
    case Gt(l, r)  => binaryComp(l, r, BinaryOp.Gt)
    case Ge(l, r)  => binaryComp(l, r, BinaryOp.Ge)

    case Pool(items) =>
      val tItems = items.map(infer)
      TyPool(tItems, PoolTy)

    case PoolConcat(left, right) =>
      val tLeft = infer(left)
      val tRight = infer(right)
      TyPoolConcat(tLeft, tRight, PoolTy)
  }

  private def binary(l: Expr, r: Expr, op: BinaryOp): TyBinary = {
    val tL = infer(l)
    val tR = infer(r)
    val resTy =
      if (tL.ty == ScalarTy && tR.ty == ScalarTy) ScalarTy
      else GenericDistTy
    TyBinary(op, tL, tR, resTy)
  }

  private def binaryComp(l: Expr, r: Expr, op: BinaryOp): TyBinary = {
    val tL = infer(l)
    val tR = infer(r)
    
    val resTy = 
      if (tL.ty == ScalarTy && tR.ty == ScalarTy) ScalarTy
      else BernoulliTy(0.0) 
      
    TyBinary(op, tL, tR, resTy)
  }
}