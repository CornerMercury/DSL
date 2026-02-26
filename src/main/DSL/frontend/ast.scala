package DSL.frontend

object AST {
  sealed trait AstNode
  sealed trait Expr extends AstNode
  
  /** Identifier reference (variable usage). */
  case class Ident(name: String) extends Expr
  
  sealed trait Stmt extends AstNode

  /** Variable assignment statement. */
  case class Assign(name: String, expr: Expr) extends Stmt

  /** Expression statement: evaluate + (for now) print. */
  case class ExprStmt(expr: Expr) extends Stmt
  
  /** A program is a list of statements (assignments and/or expressions). */
  case class Program(stmts: List[Stmt]) extends AstNode

  case class IntLiteral(value: Int) extends Expr

  case class Dice(count: Expr, sides: Expr) extends Expr

  case class Sum(expr: Expr) extends Expr
  case class Prod(expr: Expr) extends Expr

  case class Add(left: Expr, right: Expr) extends Expr
  case class Sub(left: Expr, right: Expr) extends Expr
  
  case class Mul(left: Expr, right: Expr) extends Expr
  case class Div(left: Expr, right: Expr) extends Expr
  
  case class CustomDist(dist: Map[Int, Double]) extends Expr
}