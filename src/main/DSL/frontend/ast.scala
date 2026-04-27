package DSL.frontend

object AST {
  sealed trait AstNode
  
  sealed trait Expr extends AstNode
  case class Ident(name: String) extends Expr
  case class Call(name: String, args: List[Expr]) extends Expr
  case class IntLiteral(value: Int) extends Expr
  case class Dice(count: Expr, sides: Expr) extends Expr
  case class Sum(expr: Expr) extends Expr
  case class Prod(expr: Expr) extends Expr
  case class CustomDist(dist: Map[Int, Double]) extends Expr

  // Arithmetic
  case class Add(left: Expr, right: Expr) extends Expr
  case class Sub(left: Expr, right: Expr) extends Expr
  case class Mul(left: Expr, right: Expr) extends Expr
  case class Div(left: Expr, right: Expr) extends Expr

  // Comparisons
  case class Eq(left: Expr, right: Expr) extends Expr

  sealed trait Stmt extends AstNode
  case class Assign(name: String, expr: Expr) extends Stmt
  case class ExprStmt(expr: Expr) extends Stmt
  case class Return(expr: Expr) extends Stmt
  case class Func(name: String, params: List[String], body: List[Stmt]) extends Stmt
  
  // Binding in if-header: v = ~d6
  case class RollBinding(name: String, expr: Expr)

  case class Branch(
    bindings: List[RollBinding], 
    condition: Expr, 
    body: List[Stmt]
  )

  case class If(branches: List[Branch], elseBody: Option[List[Stmt]]) extends Stmt
  case class Program(stmts: List[Stmt]) extends AstNode
}