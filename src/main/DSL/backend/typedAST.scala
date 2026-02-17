package DSL.backend

object typedAST {

  sealed trait TyAstNode

  sealed abstract class TyExpr extends TyAstNode {
    def ty: DistTy
  }

  case class TyIntLiteral(value: Int, ty: DistTy) extends TyExpr

  case class TyDice(count: TyExpr, sides: TyExpr, ty: DistTy) extends TyExpr

  case class TySum(inner: TyExpr, ty: DistTy) extends TyExpr

  case class TyProd(inner: TyExpr, ty: DistTy) extends TyExpr

  case class TyAdd(left: TyExpr, right: TyExpr, ty: DistTy) extends TyExpr

  case class TySub(left: TyExpr, right: TyExpr, ty: DistTy) extends TyExpr

  case class TyMul(left: TyExpr, right: TyExpr, ty: DistTy) extends TyExpr

  case class TyDiv(left: TyExpr, right: TyExpr, ty: DistTy) extends TyExpr
}
