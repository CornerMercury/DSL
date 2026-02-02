package DSL.frontend

object AST {
  sealed trait AstNode
  sealed trait Expr extends AstNode

  case class IntLiteral(value: Int) extends Expr

  case class Dice(count: Expr, sides: Expr) extends Expr

  case class Add(left: Expr, right: Expr) extends Expr
  case class Sub(left: Expr, right: Expr) extends Expr
  
  case class Mul(left: Expr, right: Expr) extends Expr
  case class Div(left: Expr, right: Expr) extends Expr
}