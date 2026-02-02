package DSL.frontend

object ast {
  
  // Base trait for all nodes
  sealed trait AstNode

  // We define Expr as a trait to allow future expansion 
  // (e.g. if you want to add "d6 + 5" later)
  sealed trait Expr extends AstNode

  /**
    * Represents a Die roll.
    * Example: d6 -> Die(6)
    * Example: d20 -> Die(20)
    */
  case class Die(sides: Int) extends Expr
  
}