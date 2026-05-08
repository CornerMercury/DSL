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
      program.stmts.collect { case f: Func => f.name -> f }.toMap

    // We must fold over the statements to correctly thread the environment
    val (_, results) = program.stmts.foldLeft((Map.empty[String, Distribution], List.empty[Distribution])) {
      case ((env, acc), Assign(name, expr)) =>
        val value = eval(typer.annotate(expr), env, funcEnv, sem, DiceMode.Sum)
        (env.updated(name, value), acc)
      case ((env, acc), ExprStmt(expr)) =>
        val value = eval(typer.annotate(expr), env, funcEnv, sem, DiceMode.Sum)
        (env, acc :+ value)
      case ((env, acc), Func(_, _, _)) =>
        (env, acc)
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

      val evaluatedArgs =
        args.map(eval(_, env, funcEnv, sem, mode))
      val newEnv = env ++ func.params.zip(evaluatedArgs)
      eval(typer.annotate(func.body), newEnv, funcEnv, sem, mode)

    case TyBlock(stmts, finalExpr, _) =>
      var currentEnv = env
      stmts.foreach {
        case Assign(name, e) =>
          val value = eval(typer.annotate(e), currentEnv, funcEnv, sem, mode)
          currentEnv = currentEnv.updated(name, value)
        case _ => ()
      }
      eval(finalExpr, currentEnv, funcEnv, sem, mode)

    case TyIfExpr(bindings, cond, thenB, elseB, _) =>
      val enumerated =
        enumerateBindings(bindings, env, funcEnv, sem, mode)

      var result: Distribution = Map.empty

      for ((bindEnv, weight) <- enumerated) {
        val condDist = eval(cond, bindEnv, funcEnv, sem, mode)
        val isTrue = condDist.getOrElse(1, 0.0) > 0.999999

        val branch =
          if (isTrue)
            eval(thenB, bindEnv, funcEnv, sem, mode)
          else
            eval(elseB, bindEnv, funcEnv, sem, mode)

        val scaled = MathOps.scale(branch, weight)
        result =
          if (result.isEmpty) scaled
          else MathOps.merge(result, scaled)
      }

      result

    case TyUnary(UnaryOp.Sum, i, _) =>
      eval(i, env, funcEnv, sem, DiceMode.Sum)

    case TyUnary(UnaryOp.Prod, i, _) =>
      eval(i, env, funcEnv, sem, DiceMode.Prod)

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