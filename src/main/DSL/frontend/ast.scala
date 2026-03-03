package DSL.frontend

object AST {
  sealed trait AstNode
  sealed trait Expr extends AstNode
  
  /** Identifier reference (variable usage). */
  case class Ident(name: String) extends Expr
  
  /** Function call: funcName(arg1, arg2) */
  case class Call(name: String, args: List[Expr]) extends Expr
  
  sealed trait Stmt extends AstNode

  /** Variable assignment statement. */
  case class Assign(name: String, expr: Expr) extends Stmt

  /** Expression statement: evaluate + (for now) print. */
  case class ExprStmt(expr: Expr) extends Stmt

  /** Return statement (valid inside function body). */
  case class Return(expr: Expr) extends Stmt

  /** Function declaration: func name(arg1, arg2, ...) { stmt; ...; return ... } */
  case class Func(name: String, params: List[String], body: List[Stmt]) extends Stmt
  
  /** A program is a list of statements (assignments, expressions, function declarations). */
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