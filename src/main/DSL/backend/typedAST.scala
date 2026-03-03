package DSL.backend

import DSL.backend.DistTy

object typedAST {
  sealed trait TyAstNode
  
  sealed abstract class TyExpr extends TyAstNode {
    def ty: DistTy
  }

  enum UnaryOp { case Sum, Prod }
  enum BinaryOp { case Dice, Add, Sub, Mul, Div }

  case class TyIntLiteral(value: Int, ty: DistTy) extends TyExpr
  case class TyIdent(name: String, ty: DistTy) extends TyExpr
  
  case class TyCustomDist(dist: Map[Int, Double], ty: DistTy) extends TyExpr

  case class TyCall(name: String, args: List[TyExpr], ty: DistTy) extends TyExpr

  case class TyUnary(op: UnaryOp, inner: TyExpr, ty: DistTy) extends TyExpr
  case class TyBinary(op: BinaryOp, left: TyExpr, right: TyExpr, ty: DistTy) extends TyExpr
}