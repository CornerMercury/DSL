package DSL.backend

import DSL.frontend.AST._
import typedAST._
import scala.collection.mutable

object interpreter {

  type Env = Map[String, Distribution]
  type FuncEnv = Map[String, Func]

  private case class EvalState(
    env: Env,
    funcEnv: FuncEnv,
    outs: List[Distribution],
    retVal: Distribution,
    returnedProb: Double
  )

  def interpretProgram(program: Program, sem: DistributionSemantics = DefaultDistributionSemantics): List[Distribution] = {
    val finalState = evalStmts(program.stmts, Map.empty, Map.empty, sem, DiceMode.Sum)
    
    // Append the return value to outputs if the program ended via a return
    val finalOuts = if (finalState.returnedProb > 0) finalState.outs :+ finalState.retVal else finalState.outs
    
    if (finalOuts.nonEmpty) finalOuts
    else throw new IllegalArgumentException("Program produced no output.")
  }

  private def evalStmts(
    stmts: List[Stmt],
    initEnv: Env,
    initFuncEnv: FuncEnv,
    sem: DistributionSemantics,
    defaultMode: DiceMode
  ): EvalState = {
    stmts.foldLeft(EvalState(initEnv, initFuncEnv, List.empty, Map.empty, 0.0)) { (state, stmt) =>
      if (state.returnedProb >= 1.0 - 1e-11) state
      else stmt match {
        case Assign(name, expr) =>
          val value = evalExprWithEnv(expr, defaultMode, sem, state.env, state.funcEnv)
          state.copy(env = state.env.updated(name, value))

        case ExprStmt(expr) =>
          val value = evalExprWithEnv(expr, defaultMode, sem, state.env, state.funcEnv)
          // Add this output to the current list
          state.copy(outs = state.outs :+ value)

        case f @ Func(name, _, _) =>
          state.copy(funcEnv = state.funcEnv.updated(name, f))

        case Return(expr) =>
          val value = evalExprWithEnv(expr, defaultMode, sem, state.env, state.funcEnv)
          val weight = 1.0 - state.returnedProb
          state.copy(
            retVal = MathOps.merge(state.retVal, MathOps.scale(value, weight)),
            returnedProb = 1.0
          )

        case If(branches, elseBody) =>
          var currentEnv = state.env
          var remainingProbInIf = 1.0 - state.returnedProb
          var combinedRet = state.retVal
          var totalReturnedProb = state.returnedProb
          
          // We need to track outputs generated specifically within this If block
          var newlyGeneratedOuts: List[Distribution] = Nil

          for (branch <- branches if remainingProbInIf > 0) {
            branch.bindings.foreach { b =>
              val d = evalExprWithEnv(b.expr, defaultMode, sem, currentEnv, state.funcEnv)
              currentEnv = currentEnv.updated(b.name, d)
            }

            val condDist = evalExprWithEnv(branch.condition, defaultMode, sem, currentEnv, state.funcEnv)
            val pTrue = condDist.filter((v, _) => v != 0).values.sum
            
            if (pTrue > 0) {
              val branchWeight = pTrue * remainingProbInIf
              
              val branchEnv = currentEnv.map { case (name, dist) =>
                name -> conditionDist(name, dist, branch.condition, true, currentEnv, state.funcEnv, sem, defaultMode)
              }

              val branchState = evalStmts(branch.body, branchEnv, state.funcEnv, sem, defaultMode)
              
              // 1. Merge Return Values
              combinedRet = MathOps.merge(combinedRet, MathOps.scale(branchState.retVal, branchWeight))
              totalReturnedProb += branchState.returnedProb * branchWeight
              
              // 2. Merge expression statement outputs
              val weightedBranchOuts = branchState.outs.map(MathOps.scale(_, branchWeight))
              if (newlyGeneratedOuts.isEmpty) {
                newlyGeneratedOuts = weightedBranchOuts
              } else {
                newlyGeneratedOuts = newlyGeneratedOuts.zipAll(weightedBranchOuts, Map.empty, Map.empty).map {
                  case (oldD, newD) => MathOps.merge(oldD, newD)
                }
              }

              currentEnv = currentEnv.map { case (name, dist) =>
                name -> conditionDist(name, dist, branch.condition, false, currentEnv, state.funcEnv, sem, defaultMode)
              }
              remainingProbInIf -= branchWeight
            }
          }

          if (remainingProbInIf > 0 && elseBody.isDefined) {
            val elseState = evalStmts(elseBody.get, currentEnv, state.funcEnv, sem, defaultMode)
            
            combinedRet = MathOps.merge(combinedRet, MathOps.scale(elseState.retVal, remainingProbInIf))
            totalReturnedProb += elseState.returnedProb * remainingProbInIf

            val weightedElseOuts = elseState.outs.map(MathOps.scale(_, remainingProbInIf))
            if (newlyGeneratedOuts.isEmpty) {
              newlyGeneratedOuts = weightedElseOuts
            } else {
              newlyGeneratedOuts = newlyGeneratedOuts.zipAll(weightedElseOuts, Map.empty, Map.empty).map {
                case (oldD, newD) => MathOps.merge(oldD, newD)
              }
            }
          }

          // Return state with the merged outputs from the branches appended to existing outputs
          state.copy(
            retVal = combinedRet, 
            returnedProb = totalReturnedProb, 
            outs = state.outs ++ newlyGeneratedOuts
          )
      }
    }
  }

  private def conditionDist(
    varName: String, 
    dist: Distribution, 
    cond: Expr, 
    isTrue: Boolean,
    env: Env,
    funcEnv: FuncEnv,
    sem: DistributionSemantics,
    mode: DiceMode
  ): Distribution = {
    if (!getUsedVars(cond).contains(varName)) return dist

    val filtered = dist.flatMap { case (v, p) =>
      val testEnv = env.updated(varName, Map(v -> 1.0))
      val res = evalExprWithEnv(cond, mode, sem, testEnv, funcEnv)
      val satisfied = if (isTrue) res.exists(_._1 != 0) else res.forall(_._1 == 0)
      if (satisfied) Some(v -> p) else None
    }
    
    val total = filtered.values.sum
    if (total == 0) Map.empty else filtered.map { case (v, p) => v -> p / total }
  }

  private def getUsedVars(expr: Expr): Set[String] = expr match {
    case Ident(n)      => Set(n)
    case Call(_, args) => args.flatMap(getUsedVars).toSet
    case Add(l, r)     => getUsedVars(l) ++ getUsedVars(r)
    case Sub(l, r)     => getUsedVars(l) ++ getUsedVars(r)
    case Mul(l, r)     => getUsedVars(l) ++ getUsedVars(r)
    case Div(l, r)     => getUsedVars(l) ++ getUsedVars(r)
    case Eq(l, r)      => getUsedVars(l) ++ getUsedVars(r)
    case Dice(c, s)    => getUsedVars(c) ++ getUsedVars(s)
    case Sum(e)        => getUsedVars(e)
    case Prod(e)       => getUsedVars(e)
    case _             => Set.empty
  }

  private def evalExprWithEnv(expr: Expr, mode: DiceMode, sem: DistributionSemantics, env: Env, funcEnv: FuncEnv): Distribution = {
    eval(typer.annotate(expr), mode, sem, env, funcEnv)
  }

  private def eval(expr: TyExpr, mode: DiceMode, sem: DistributionSemantics, env: Env, funcEnv: FuncEnv): Distribution = expr match {
    case TyIntLiteral(n, _) => sem.scalar(n)
    case TyIdent(name, _)   => env.getOrElse(name, throw new IllegalArgumentException(s"Unbound identifier: $name"))
    case TyCustomDist(d, _) => sem.custom(d)
    case TyCall(name, args, _) =>
      val func = funcEnv.getOrElse(name, throw new IllegalArgumentException(s"Undefined function: $name"))
      if (func.params.size != args.size) {
        throw new IllegalArgumentException(s"Function '$name' expects ${func.params.size} arguments, got ${args.size}")
      }
      val evaluatedArgs = args.map(eval(_, mode, sem, env, funcEnv))
      val res = evalStmts(func.body, env ++ func.params.zip(evaluatedArgs), funcEnv, sem, mode)
      if (res.returnedProb < 1.0 - 1e-11) {
        throw new IllegalArgumentException(s"Function '$name' reached the end of its body without returning a value.")
      }
      res.retVal
    case TyUnary(UnaryOp.Sum, i, _)  => eval(i, DiceMode.Sum, sem, env, funcEnv)
    case TyUnary(UnaryOp.Prod, i, _) => eval(i, DiceMode.Prod, sem, env, funcEnv)
    case TyBinary(op, l, r, _) =>
      val dL = eval(l, mode, sem, env, funcEnv)
      val dR = eval(r, mode, sem, env, funcEnv)
      op match {
        case BinaryOp.Dice => sem.dice(dL, dR, mode)
        case BinaryOp.Add  => sem.add(dL, dR)
        case BinaryOp.Sub  => sem.sub(dL, dR)
        case BinaryOp.Mul  => sem.mul(dL, dR)
        case BinaryOp.Div  => sem.div(dL, dR)
        case BinaryOp.Eq   => sem.eq(dL, dR)
      }
  }
}