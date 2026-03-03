package DSL.backend

import DSL.frontend.AST._
import typedAST._

object interpreter {

  type Env = Map[String, Distribution]
  type FuncEnv = Map[String, Func]

  // Tracks execution context block by block to easily handle Return short-circuiting
  private case class EvalState(
    env: Env,
    funcEnv: FuncEnv,
    outs: List[Distribution],
    retVal: Option[Distribution]
  )

  def interpret(expr: Expr): Distribution =
    interpret(expr, DefaultDistributionSemantics)

  def interpret(expr: Expr, sem: DistributionSemantics): Distribution = expr match {
    case Sum(_) | Prod(_) =>
      val typed = typer.annotate(expr)
      interpretTyped(typed, sem)
    case other =>
      throw new IllegalArgumentException(s"Root must be Sum or Prod. Got: $other")
  }

  def interpretProgram(program: Program): List[Distribution] =
    interpretProgram(program, DefaultDistributionSemantics)

  def interpretProgram(program: Program, sem: DistributionSemantics): List[Distribution] = {
    val finalState = evalStmts(program.stmts, Map.empty, Map.empty, sem, DiceMode.Sum)
    
    if (finalState.outs.nonEmpty) finalState.outs
    else throw new IllegalArgumentException("Program contains no expression statements to evaluate.")
  }

  /** Evaluates a block of statements, allowing Return to short-circuit execution. */
  private def evalStmts(
    stmts: List[Stmt],
    initEnv: Env,
    initFuncEnv: FuncEnv,
    sem: DistributionSemantics,
    defaultMode: DiceMode
  ): EvalState = {
    stmts.foldLeft(EvalState(initEnv, initFuncEnv, List.empty, None)) { (state, stmt) =>
      // If we've already hit a Return statement, short-circuit and skip remaining
      if (state.retVal.isDefined) state
      else stmt match {
        case Assign(name, expr) =>
          val value = evalExprWithEnv(expr, defaultMode, sem, state.env, state.funcEnv)
          state.copy(env = state.env.updated(name, value))

        case ExprStmt(expr) =>
          val value = evalExprWithEnv(expr, defaultMode, sem, state.env, state.funcEnv)
          state.copy(outs = state.outs :+ value)

        case f @ Func(name, _, _) =>
          // Bind/update function in the function environment
          state.copy(funcEnv = state.funcEnv.updated(name, f))

        case Return(expr) =>
          val value = evalExprWithEnv(expr, defaultMode, sem, state.env, state.funcEnv)
          state.copy(retVal = Some(value))
      }
    }
  }

  def interpretTyped(expr: TyExpr): Distribution =
    interpretTyped(expr, DefaultDistributionSemantics)

  def interpretTyped(expr: TyExpr, sem: DistributionSemantics): Distribution = expr match {
    case TyUnary(UnaryOp.Sum, inner, _)  => eval(inner, DiceMode.Sum, sem)
    case TyUnary(UnaryOp.Prod, inner, _) => eval(inner, DiceMode.Prod, sem)
    case other =>
      throw new IllegalArgumentException(s"Root must be sum or prod. Got: $other")
  }

  private def evalExprWithEnv(expr: Expr, defaultMode: DiceMode, sem: DistributionSemantics, env: Env, funcEnv: FuncEnv): Distribution = {
    val typed = typer.annotate(expr)
    typed match {
      case TyUnary(UnaryOp.Sum, inner, _)  => eval(inner, DiceMode.Sum, sem, env, funcEnv)
      case TyUnary(UnaryOp.Prod, inner, _) => eval(inner, DiceMode.Prod, sem, env, funcEnv)
      case other                           => eval(other, defaultMode, sem, env, funcEnv)
    }
  }

  private def eval(expr: TyExpr, mode: DiceMode, sem: DistributionSemantics): Distribution =
    eval(expr, mode, sem, Map.empty, Map.empty)

  private def eval(expr: TyExpr, mode: DiceMode, sem: DistributionSemantics, env: Env, funcEnv: FuncEnv): Distribution = expr match {
    case TyIntLiteral(n, _) => sem.scalar(n)

    case TyIdent(name, _) =>
      env.getOrElse(name, throw new IllegalArgumentException(s"Unbound identifier: $name"))

    case TyCustomDist(raw, _) => sem.custom(raw)

    case TyCall(name, args, _) =>
      val func = funcEnv.getOrElse(name, throw new IllegalArgumentException(s"Undefined function: $name"))
      if (func.params.size != args.size) {
        throw new IllegalArgumentException(s"Function '$name' expects ${func.params.size} arguments, got ${args.size}")
      }
      val evaluatedArgs = args.map(arg => eval(arg, mode, sem, env, funcEnv))
      
      // Merge caller env & evaluated args (Args will shadow existing globals if identically named)
      val localEnv = env ++ func.params.zip(evaluatedArgs).toMap
      
      val funcState = evalStmts(func.body, localEnv, funcEnv, sem, mode)
      funcState.retVal.getOrElse(throw new IllegalArgumentException(s"Function '$name' reached the end of its body without returning a value."))

    case TyUnary(UnaryOp.Sum, inner, _) =>
      eval(inner, DiceMode.Sum, sem, env, funcEnv)

    case TyUnary(UnaryOp.Prod, inner, _) =>
      eval(inner, DiceMode.Prod, sem, env, funcEnv)

    case TyBinary(BinaryOp.Dice, c, s, _) =>
      val dC = eval(c, mode, sem, env, funcEnv)
      val dS = eval(s, mode, sem, env, funcEnv)
      sem.dice(dC, dS, mode)

    case TyBinary(BinaryOp.Add, l, r, _) =>
      sem.add(eval(l, mode, sem, env, funcEnv), eval(r, mode, sem, env, funcEnv))

    case TyBinary(BinaryOp.Sub, l, r, _) =>
      sem.sub(eval(l, mode, sem, env, funcEnv), eval(r, mode, sem, env, funcEnv))

    case TyBinary(BinaryOp.Mul, l, r, _) =>
      sem.mul(eval(l, mode, sem, env, funcEnv), eval(r, mode, sem, env, funcEnv))

    case TyBinary(BinaryOp.Div, l, r, _) =>
      sem.div(eval(l, mode, sem, env, funcEnv), eval(r, mode, sem, env, funcEnv))
  }
}