package DSL.backend

object typedAST {

  sealed trait TyAstNode

  sealed abstract class TyExpr extends TyAstNode {
    def ty: DistTy
  }

  enum UnaryOp {
    case Sum, Prod
  }

  enum BinaryOp {
    case Dice, Add, Sub, Mul, Div
  }

  case class TyIntLiteral(value: Int, ty: DistTy) extends TyExpr

  /** Unary wrapper nodes like `sum(e)` or `prod(e)`. */
  case class TyUnary(op: UnaryOp, inner: TyExpr, ty: DistTy) extends TyExpr

  /** Binary nodes like `e1 + e2`, `e1 / e2`, or `dice(count, sides)`. */
  case class TyBinary(op: BinaryOp, left: TyExpr, right: TyExpr, ty: DistTy) extends TyExpr
}
