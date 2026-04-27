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
    retVal: Distribution, // Changed from Option to a Map
    returnedProb: Double  // Tracks how much of the probability space has hit a 'return'
  )

  def interpretProgram(program: Program, sem: DistributionSemantics = DefaultDistributionSemantics): List[Distribution] = {
    val finalState = evalStmts(program.stmts, Map.empty, Map.empty, sem, DiceMode.Sum)
    
    // If there's a return value from the top level, include it in outputs
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
      // Only skip if the probability space is fully exhausted (100% returned)
      if (state.returnedProb >= 1.0 - 1e-11) state
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
          // Scale the return by whatever probability hasn't returned yet in this block
          val weight = 1.0 - state.returnedProb
          val weightedVal = MathOps.scale(value, weight)
          state.copy(
            retVal = MathOps.merge(state.retVal, weightedVal),
            returnedProb = 1.0 // This path is now fully exhausted
          )

        case If(branches, elseBody) =>
          val sampledVarsMap = branches.flatMap(_.bindings).map { b =>
            b.name -> evalExprWithEnv(b.expr, defaultMode, sem, state.env, state.funcEnv)
          }.toMap

          val varNames = sampledVarsMap.keys.toList
          val varDists = varNames.map(sampledVarsMap)

          def getJoint(names: List[String], dists: List[Distribution]): List[(Map[String, Int], Double)] = {
            names match {
              case Nil => List((Map.empty[String, Int], 1.0))
              case name :: ns =>
                for {
                  (value, prob) <- dists.head.toList
                  (restMap, restProb) <- getJoint(ns, dists.tail)
                } yield (restMap + (name -> value), prob * restProb)
            }
          }

          val joint = getJoint(varNames, varDists)
          val branchOutcomes = Array.fill(branches.size)(mutable.ListBuffer[(Map[String, Int], Double)]())
          val elseOutcomes = mutable.ListBuffer[(Map[String, Int], Double)]()

          for ((combo, p) <- joint) {
            var triggered = false
            for ((branch, idx) <- branches.zipWithIndex if !triggered) {
              val tempEnv = state.env ++ combo.map { case (k, v) => k -> sem.scalar(v) }
              val condDist = evalExprWithEnv(branch.condition, defaultMode, sem, tempEnv, state.funcEnv)
              if (condDist.exists(_._1 != 0)) {
                branchOutcomes(idx) += ((combo, p))
                triggered = true
              }
            }
            if (!triggered) elseOutcomes += ((combo, p))
          }

          var combinedRet = state.retVal
          var totalNewlyReturnedProb = 0.0
          val weightMultiplier = 1.0 - state.returnedProb

          def processBranch(outcomes: List[(Map[String, Int], Double)], body: List[Stmt]): Unit = {
            val branchWeight = outcomes.map(_._2).sum
            if (branchWeight <= 0) return

            val conditionalEnv = varNames.map { name =>
              val marginal = outcomes.groupBy(_._1(name)).map { case (v, group) =>
                v -> group.map(_._2).sum / branchWeight
              }
              name -> marginal
            }.toMap

            val branchState = evalStmts(body, state.env ++ conditionalEnv, state.funcEnv, sem, defaultMode)
            
            // Map return values back to the global probability space
            val globalBranchWeight = branchWeight * weightMultiplier
            val weightedBranchRet = MathOps.scale(branchState.retVal, globalBranchWeight)
            
            combinedRet = MathOps.merge(combinedRet, weightedBranchRet)
            totalNewlyReturnedProb += branchState.returnedProb * globalBranchWeight
          }

          branches.zipWithIndex.foreach { case (b, i) => processBranch(branchOutcomes(i).toList, b.body) }
          elseBody.foreach(processBranch(elseOutcomes.toList, _))

          state.copy(
            retVal = combinedRet,
            returnedProb = state.returnedProb + totalNewlyReturnedProb
          )
      }
    }
  }

  private def evalExprWithEnv(expr: Expr, mode: DiceMode, sem: DistributionSemantics, env: Env, funcEnv: FuncEnv): Distribution = {
    eval(typer.annotate(expr), mode, sem, env, funcEnv)
  }

  private def eval(expr: TyExpr, mode: DiceMode, sem: DistributionSemantics, env: Env, funcEnv: FuncEnv): Distribution = expr match {
    case TyIntLiteral(n, _) => sem.scalar(n)
    case TyIdent(name, _)   => env.getOrElse(name, throw new IllegalArgumentException(s"Unbound: $name"))
    case TyCustomDist(d, _) => sem.custom(d)
    case TyCall(name, args, _) =>
      val func = funcEnv.getOrElse(name, throw new IllegalArgumentException(s"Undefined function: $name"))
      if (func.params.size != args.size) throw new IllegalArgumentException(s"Arity mismatch for $name")
      
      val evaluatedArgs = args.map(eval(_, mode, sem, env, funcEnv))
      val localEnv = env ++ func.params.zip(evaluatedArgs).toMap
      val res = evalStmts(func.body, localEnv, funcEnv, sem, mode)
      
      // scopeChecker ensures returnedProb is 1.0
      res.retVal

    case TyUnary(UnaryOp.Sum, inner, _)  => eval(inner, DiceMode.Sum, sem, env, funcEnv)
    case TyUnary(UnaryOp.Prod, inner, _) => eval(inner, DiceMode.Prod, sem, env, funcEnv)
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