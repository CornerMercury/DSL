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
    
    // Pass generic type for calls (could be improved with function signatures)
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
      TyUnary(UnaryOp.Sum, tInner, tInner.ty)

    case Prod(inner) =>
      val tInner = infer(inner)
      TyUnary(UnaryOp.Prod, tInner, tInner.ty)

    case Max(inner) =>
      val tInner = infer(inner)
      TyUnary(UnaryOp.Max, tInner, ScalarTy)

    case Min(inner) =>
      val tInner = infer(inner)
      TyUnary(UnaryOp.Min, tInner, ScalarTy)

    case Dice(c, s) =>
      val tC = infer(c)
      val tS = infer(s)
      val resTy = (tC, tS) match {
        case (TyIntLiteral(1, _), TyIntLiteral(1, _))       => ScalarTy
        case (TyIntLiteral(1, _), TyIntLiteral(sides, _)) if sides > 1 => UniformTy
        case (TyIntLiteral(n, _), TyIntLiteral(_, _)) if n > 1 => GenericDistTy
        case _                                              => GenericDistTy
      }
      TyBinary(BinaryOp.Dice, tC, tS, resTy)

    case Add(l, r) => binary(l, r, BinaryOp.Add)
    case Sub(l, r) => binary(l, r, BinaryOp.Sub)
    case Mul(l, r) => binary(l, r, BinaryOp.Mul)
    case Div(l, r) => binary(l, r, BinaryOp.Div)
    
    // Comparisons always result in a boolean-like distribution (Bernoulli)
    case Eq(l, r)  => binaryComp(l, r, BinaryOp.Eq)
    case Lt(l, r)  => binaryComp(l, r, BinaryOp.Lt)
    case Le(l, r)  => binaryComp(l, r, BinaryOp.Le)
    case Gt(l, r)  => binaryComp(l, r, BinaryOp.Gt)
    case Ge(l, r)  => binaryComp(l, r, BinaryOp.Ge)

    // Pool and PoolConcat handling
    case Pool(items) =>
      val tItems = items.map(infer)
      // For now, we treat the pool type as GenericDistTy, 
      // as it aggregates potentially multiple distributions.
      TyPool(tItems, GenericDistTy)

    case PoolConcat(left, right) =>
      val tLeft = infer(left)
      val tRight = infer(right)
      TyPoolConcat(tLeft, tRight, GenericDistTy)
  }

  private def binary(l: Expr, r: Expr, op: BinaryOp): TyBinary = {
    val tL = infer(l)
    val tR = infer(r)
    val resTy =
      if (tL.ty == ScalarTy && tR.ty == ScalarTy) ScalarTy
      else GenericDistTy
    TyBinary(op, tL, tR, resTy)
  }

  // Helper for comparison operators which return BernoulliTy
  private def binaryComp(l: Expr, r: Expr, op: BinaryOp): TyBinary = {
    val tL = infer(l)
    val tR = infer(r)
    
    // If both are Scalar, the result is deterministic (Scalar 0 or 1).
    // Otherwise, the result is a Bernoulli distribution.
    // We use 0.0 as a placeholder probability since we don't calculate 'p' at compile time.
    val resTy = 
      if (tL.ty == ScalarTy && tR.ty == ScalarTy) ScalarTy
      else BernoulliTy(0.0) 
      
    TyBinary(op, tL, tR, resTy)
  }
}