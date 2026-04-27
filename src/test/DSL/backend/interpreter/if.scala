package DSL.backend

import DSL.frontend.AST._
import typedAST._

object interpreter {

  type Env = Map[String, Distribution]
  type FuncEnv = Map[String, Func]

  private case class EvalState(
    env: Env,
    funcEnv: FuncEnv,
    outs: List[Distribution],
    retVal: Option[Distribution]
  )

  def interpretProgram(program: Program, sem: DistributionSemantics = DefaultDistributionSemantics): List[Distribution] = {
    val finalState = evalStmts(program.stmts, Map.empty, Map.empty, sem, DiceMode.Sum)
    val finalOuts = if (finalState.retVal.isDefined) finalState.outs :+ finalState.retVal.get else finalState.outs
    if (finalOuts.nonEmpty) finalOuts
    else throw new IllegalArgumentException("Program contains no expression statements to evaluate.")
  }

  private def evalStmts(
    stmts: List[Stmt],
    initEnv: Env,
    initFuncEnv: FuncEnv,
    sem: DistributionSemantics,
    defaultMode: DiceMode
  ): EvalState = {
    stmts.foldLeft(EvalState(initEnv, initFuncEnv, List.empty, None)) { (state, stmt) =>
      if (state.retVal.isDefined) state
      else stmt match {
        case Assign(name, expr) =>
          val value = evalExprWithEnv(expr, defaultMode, sem, state.env, state.funcEnv)
          state.copy(env = state.env.updated(name, value))

        case ExprStmt(expr) =>
          val value = evalExprWithEnv(expr, defaultMode, sem, state.env, state.funcEnv)
          state.copy(outs = state.outs :+ value)

        case f @ Func(name, _, _) =>
          state.copy(funcEnv = state.funcEnv.updated(name, f))

        case Return(expr) =>
          val value = evalExprWithEnv(expr, defaultMode, sem, state.env, state.funcEnv)
          state.copy(retVal = Some(value))

        case If(branches, elseBody) =>
          // 1. Find all variables used in conditions to handle dependencies correctly
          val condVars = branches.flatMap(b => getUsedVars(b._1)).toSet.toList
          val varDists = condVars.map(v => v -> state.env.getOrElse(v, Map.empty))

          // 2. Expand the world: Iterate over every possible combination of values for those variables
          val worlds = generateWorlds(varDists)

          var combinedRet: Option[Distribution] = None
          var combinedOuts: List[Distribution] = List.empty

          for ((worldEnvUpdate, worldProb) <- worlds) {
            val worldEnv = state.env ++ worldEnvUpdate.mapValues(v => Map(v -> 1.0))
            
            // 3. Evaluate the If-structure deterministically in this specific world
            val branchResult = evalDeterministicIf(branches, elseBody, worldEnv, state.funcEnv, sem, defaultMode)
            
            // 4. Merge results weighted by the probability of this world
            branchResult.retVal.foreach { rv =>
              val weighted = MathOps.scale(rv, worldProb)
              combinedRet = Some(combinedRet.map(MathOps.merge(_, weighted)).getOrElse(weighted))
            }
            
            if (branchResult.outs.nonEmpty) {
              if (combinedOuts.isEmpty) combinedOuts = branchResult.outs.map(MathOps.scale(_, worldProb))
              else {
                combinedOuts = combinedOuts.zipAll(branchResult.outs, Map.empty, Map.empty).map {
                  case (oldD, newD) => MathOps.merge(oldD, MathOps.scale(newD, worldProb))
                }
              }
            }
          }
          state.copy(outs = state.outs ++ combinedOuts, retVal = combinedRet)
      }
    }
  }

  /** Evaluates the If-Elif-Else chain where variables are fixed, so conditions are either 100% true or false. */
  private def evalDeterministicIf(
    branches: List[(Expr, List[Stmt])],
    elseBody: Option[List[Stmt]],
    env: Env,
    funcEnv: FuncEnv,
    sem: DistributionSemantics,
    mode: DiceMode
  ): EvalState = {
    for ((condExpr, body) <- branches) {
      val condDist = evalExprWithEnv(condExpr, mode, sem, env, funcEnv)
      val isTrue = condDist.exists { case (v, p) => v != 0 && p > 0 }
      if (isTrue) return evalStmts(body, env, funcEnv, sem, mode)
    }
    elseBody.map(body => evalStmts(body, env, funcEnv, sem, mode))
      .getOrElse(EvalState(env, funcEnv, Nil, None))
  }

  /** Generates Cartesian product of all possible outcomes for the given variables. */
  private def generateWorlds(vars: List[(String, Distribution)]): List[(Map[String, Int], Double)] = {
    vars.foldLeft(List((Map.empty[String, Int], 1.0))) { case (acc, (name, dist)) =>
      for {
        (currentMap, currentProb) <- acc
        (value, prob) <- dist
      } yield (currentMap + (name -> value), currentProb * prob)
    }
  }

  private def getUsedVars(expr: Expr): Set[String] = expr match {
    case Ident(name)   => Set(name)
    case Call(_, args) => args.flatMap(getUsedVars).toSet
    case Add(l, r)     => getUsedVars(l) ++ getUsedVars(r)
    case Sub(l, r)     => getUsedVars(l) ++ getUsedVars(r)
    case Mul(l, r)     => getUsedVars(l) ++ getUsedVars(r)
    case Div(l, r)     => getUsedVars(l) ++ getUsedVars(r)
    case Eq(l, r)      => getUsedVars(l) ++ getUsedVars(r)
    case IdenEq(l, r)  => getUsedVars(l) ++ getUsedVars(r)
    case Dice(c, s)    => getUsedVars(c) ++ getUsedVars(s)
    case Sum(e)        => getUsedVars(e)
    case Prod(e)       => getUsedVars(e)
    case _             => Set.empty
  }

  private def evalExprWithEnv(expr: Expr, defaultMode: DiceMode, sem: DistributionSemantics, env: Env, funcEnv: FuncEnv): Distribution = {
    val typed = typer.annotate(expr)
    eval(typed, defaultMode, sem, env, funcEnv)
  }

  private def eval(expr: TyExpr, mode: DiceMode, sem: DistributionSemantics, env: Env, funcEnv: FuncEnv): Distribution = expr match {
    case TyIntLiteral(n, _) => sem.scalar(n)
    case TyIdent(name, _)   => env.getOrElse(name, throw new IllegalArgumentException(s"Unbound identifier: $name"))
    case TyCustomDist(raw, _) => sem.custom(raw)
    case TyCall(name, args, _) =>
      val func = funcEnv.getOrElse(name, throw new IllegalArgumentException(s"Undefined function: $name"))
      if (func.params.size != args.size) throw new IllegalArgumentException(s"Function '$name' expects ${func.params.size} arguments, got ${args.size}")
      val evaluatedArgs = args.map(arg => eval(arg, mode, sem, env, funcEnv))
      val localEnv = env ++ func.params.zip(evaluatedArgs).toMap
      val funcState = evalStmts(func.body, localEnv, funcEnv, sem, mode)
      funcState.retVal.getOrElse(throw new IllegalArgumentException(s"Function '$name' reached the end of its body without returning a value."))
    case TyUnary(UnaryOp.Sum, inner, _)  => eval(inner, DiceMode.Sum, sem, env, funcEnv)
    case TyUnary(UnaryOp.Prod, inner, _) => eval(inner, DiceMode.Prod, sem, env, funcEnv)
    case TyBinary(op, l, r, _) =>
      val dL = eval(l, mode, sem, env, funcEnv)
      val dR = eval(r, mode, sem, env, funcEnv)
      op match {
        case BinaryOp.Dice   => sem.dice(dL, dR, mode)
        case BinaryOp.Add    => sem.add(dL, dR)
        case BinaryOp.Sub    => sem.sub(dL, dR)
        case BinaryOp.Mul    => sem.mul(dL, dR)
        case BinaryOp.Div    => sem.div(dL, dR)
        case BinaryOp.Eq     => sem.eq(dL, dR)
        case BinaryOp.IdenEq => sem.idenEq(dL, dR)
      }
  }
}