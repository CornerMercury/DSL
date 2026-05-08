package DSL.backend

import DSL.frontend.AST._
import typedAST._

object interpreter {

  type Env = Map[String, Distribution]
  type FuncEnv = Map[String, Func]

  def interpretProgram(
    program: Program,
    sem: DistributionSemantics = DefaultDistributionSemantics
  ): List[Distribution] = {

    val funcEnv =
      program.topLevel.collect { case Left(f: Func) => f.name -> f }.toMap

    val (_, results) = program.topLevel.foldLeft((Map.empty[String, Distribution], List.empty[Distribution])) {
      case ((env, acc), Left(Assign(name, expr))) =>
        val value = eval(typer.annotate(expr), env, funcEnv, sem, DiceMode.Sum)
        (env.updated(name, value), acc)
        
      case ((env, acc), Left(Func(_, _, _))) =>
        (env, acc) // Functions are already registered in funcEnv

      case ((env, acc), Right(expr)) =>
        val value = eval(typer.annotate(expr), env, funcEnv, sem, DiceMode.Sum)
        (env, acc :+ value)
    }

    results
  }

  private def eval(
    expr: TyExpr,
    env: Env,
    funcEnv: FuncEnv,
    sem: DistributionSemantics,
    mode: DiceMode
  ): Distribution = expr match {

    case TyIntLiteral(n, _) => sem.scalar(n)

    case TyIdent(name, _) =>
      env.getOrElse(name,
        throw new IllegalArgumentException(s"Unbound identifier: $name"))

    case TyCustomDist(d, _) => sem.custom(d)

    case TyCall(name, args, _) =>
      val func = funcEnv.getOrElse(name,
        throw new IllegalArgumentException(s"Undefined function: $name"))
      
      if (func.params.size != args.size) {
        throw new IllegalArgumentException(s"Function ${func.name} expects ${func.params.size} arguments, got ${args.size}")
      }

      val evaluatedArgs = args.map(eval(_, env, funcEnv, sem, mode))
      val newEnv = env ++ func.params.zip(evaluatedArgs)
      eval(typer.annotate(func.body), newEnv, funcEnv, sem, mode)

    case TyBlock(stmts, finalExpr, _) =>
      var currentEnv = env
      stmts.foreach {
        case Assign(name, e) =>
          val value = eval(typer.annotate(e), currentEnv, funcEnv, sem, mode)
          currentEnv = currentEnv.updated(name, value)
        case Func(_, _, _) => () // Nested functions not supported in blocks for now, ignore
      }
      eval(finalExpr, currentEnv, funcEnv, sem, mode)

    case TyIfExpr(branches, elseB, _) =>
      val allBindings = branches.flatMap(_.bindings)
      val enumerated = enumerateBindings(allBindings, env, funcEnv, sem, mode)

      var result: Distribution = Map.empty

      for ((bindEnv, weight) <- enumerated) {
        var branchTaken = false

        for (branch <- branches if !branchTaken) {
          val condDist = eval(branch.condition, bindEnv, funcEnv, sem, mode)
          val pTrue = condDist.getOrElse(1, 0.0)
          val pFalse = condDist.getOrElse(0, 0.0)

          if (pTrue > 0.0) {
            val res = eval(branch.body, bindEnv, funcEnv, sem, mode)
            result = MathOps.merge(result, MathOps.scale(res, pTrue * weight))
          }

          if (pFalse > 0.0 && pTrue < 1.0) {
            // Condition evaluates to false with some probability -> move on to the next elif/else
          } else if (pTrue > 0.0) {
            // Condition is absolutely true -> short-circuit the remaining branches
            branchTaken = true
          }
        }

        if (!branchTaken) {
          val elseResult = eval(elseB, bindEnv, funcEnv, sem, mode)
          result = MathOps.merge(result, MathOps.scale(elseResult, weight))
        }
      }

      result

    case TyUnary(UnaryOp.Sum, i, _) =>
      eval(i, env, funcEnv, sem, DiceMode.Sum)

    case TyUnary(UnaryOp.Prod, i, _) =>
      eval(i, env, funcEnv, sem, DiceMode.Prod)

    case TyUnary(UnaryOp.Max, i, _) =>
      val d = eval(i, env, funcEnv, sem, mode)
      sem.max(d)

    case TyUnary(UnaryOp.Min, i, _) =>
      val d = eval(i, env, funcEnv, sem, mode)
      sem.min(d)

    case TyBinary(op, l, r, _) =>
      val dL = eval(l, env, funcEnv, sem, mode)
      val dR = eval(r, env, funcEnv, sem, mode)
      op match {
        case BinaryOp.Dice => sem.dice(dL, dR, mode)
        case BinaryOp.Add  => sem.add(dL, dR)
        case BinaryOp.Sub  => sem.sub(dL, dR)
        case BinaryOp.Mul  => sem.mul(dL, dR)
        case BinaryOp.Div  => sem.div(dL, dR)
        case BinaryOp.Eq   => sem.eq(dL, dR)
      }
  }

  private def enumerateBindings(
    bindings: List[RollBinding],
    baseEnv: Env,
    funcEnv: FuncEnv,
    sem: DistributionSemantics,
    mode: DiceMode
  ): List[(Env, Double)] = {

    bindings.foldLeft(List((baseEnv, 1.0))) {
      case (acc, RollBinding(name, expr)) =>
        acc.flatMap { case (env, weight) =>
          val dist = eval(typer.annotate(expr), env, funcEnv, sem, mode)
          dist.map { case (value, p) =>
            (env.updated(name, Map(value -> 1.0)), weight * p)
          }
        }
    }
  }
}