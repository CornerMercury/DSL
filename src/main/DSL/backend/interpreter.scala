package DSL.backend

import DSL.frontend.AST._
import typedAST._

object interpreter {

  private type Env = Map[String, Distribution]

  /** Backwards-compatible entry point: type-check, then interpret. */
  def interpret(expr: Expr): Distribution =
    interpret(expr, DefaultDistributionSemantics)

  def interpret(expr: Expr, sem: DistributionSemantics): Distribution = expr match {
    case Sum(_) | Prod(_) =>
      val typed = typer.annotate(expr)
      interpretTyped(typed, sem)
    case other =>
      throw new IllegalArgumentException(s"Root must be Sum or Prod. Got: $other")
  }

  /** Interpret a full program with assignments and expression statements.
    * Returns a list of distributions, one per expression statement in order.
    */
  def interpretProgram(program: Program): List[Distribution] =
    interpretProgram(program, DefaultDistributionSemantics)

  def interpretProgram(program: Program, sem: DistributionSemantics): List[Distribution] = {
    val (_, results) = program.stmts.foldLeft((Map.empty[String, Distribution]: Env, List.empty[Distribution])) {
      case ((envAcc, outs), Assign(name, expr)) =>
        val value = evalExprWithEnv(expr, DiceMode.Sum, sem, envAcc)
        (envAcc.updated(name, value), outs)

      case ((envAcc, outs), ExprStmt(expr)) =>
        val value = evalExprWithEnv(expr, DiceMode.Sum, sem, envAcc)
        (envAcc, outs :+ value)
    }

    if results.nonEmpty then results
    else throw new IllegalArgumentException("Program contains no expression statements to evaluate.")
  }

  /** Interpret an already-typed AST. */
  def interpretTyped(expr: TyExpr): Distribution =
    interpretTyped(expr, DefaultDistributionSemantics)

  def interpretTyped(expr: TyExpr, sem: DistributionSemantics): Distribution = expr match {
    case TyUnary(UnaryOp.Sum, inner, _)  => eval(inner, DiceMode.Sum, sem)
    case TyUnary(UnaryOp.Prod, inner, _) => eval(inner, DiceMode.Prod, sem)
    case other =>
      throw new IllegalArgumentException(s"Root must be sum or prod. Got: $other")
  }

  /** Helper to interpret an expression given an environment of variable bindings. */
  private def evalExprWithEnv(expr: Expr, defaultMode: DiceMode, sem: DistributionSemantics, env: Env): Distribution = {
    val typed = typer.annotate(expr)
    typed match {
      case TyUnary(UnaryOp.Sum, inner, _)  => eval(inner, DiceMode.Sum, sem, env)
      case TyUnary(UnaryOp.Prod, inner, _) => eval(inner, DiceMode.Prod, sem, env)
      case other                           => eval(other, defaultMode, sem, env)
    }
  }

  private def eval(expr: TyExpr, mode: DiceMode, sem: DistributionSemantics): Distribution =
    eval(expr, mode, sem, Map.empty)

  private def eval(expr: TyExpr, mode: DiceMode, sem: DistributionSemantics, env: Env): Distribution = expr match {
    case TyIntLiteral(n, _) =>
      sem.scalar(n)

    case TyIdent(name, _) =>
      env.getOrElse(name, throw new IllegalArgumentException(s"Unbound identifier: $name"))

    case TyCustomDist(raw, _) =>
      sem.custom(raw)

    case TyUnary(UnaryOp.Sum, inner, _) =>
      eval(inner, DiceMode.Sum, sem, env)

    case TyUnary(UnaryOp.Prod, inner, _) =>
      eval(inner, DiceMode.Prod, sem, env)

    case TyBinary(BinaryOp.Dice, c, s, _) =>
      val dC = eval(c, mode, sem, env)
      val dS = eval(s, mode, sem, env)
      sem.dice(dC, dS, mode)

    case TyBinary(BinaryOp.Add, l, r, _) =>
      val dL = eval(l, mode, sem, env)
      val dR = eval(r, mode, sem, env)
      sem.add(dL, dR)

    case TyBinary(BinaryOp.Sub, l, r, _) =>
      val dL = eval(l, mode, sem, env)
      val dR = eval(r, mode, sem, env)
      sem.sub(dL, dR)

    case TyBinary(BinaryOp.Mul, l, r, _) =>
      val dL = eval(l, mode, sem, env)
      val dR = eval(r, mode, sem, env)
      sem.mul(dL, dR)

    case TyBinary(BinaryOp.Div, l, r, _) =>
      val dL = eval(l, mode, sem, env)
      val dR = eval(r, mode, sem, env)
      sem.div(dL, dR)
  }
}