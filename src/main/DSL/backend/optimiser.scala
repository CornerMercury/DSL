package DSL.backend

import DSL.frontend.AST._

object optimiser {

  def optimise(program: Program): Program = program match {
    case Program(stmts) => Program(optimiseBlock(stmts))
  }

  private def optimiseBlock(stmts: List[Stmt]): List[Stmt] = {
    val propagated = propagateConstants(stmts)
    eliminateDeadStores(propagated)
  }

  // ==========================================
  // PASS 1: Constant Propagation & Unreachable Code Removal
  // ==========================================

  private def propagateConstants(stmts: List[Stmt]): List[Stmt] = {
    val (finalStmts, _, _) = stmts.foldLeft((List.empty[Stmt], Map.empty[String, Expr], false)) {
      
      case ((acc, env, true), _) => (acc, env, true)

      case ((acc, env, false), stmt) => stmt match {
        case Assign(name, expr) =>
          val optimizedRHS = optimiseExpr(expr, env)
          val newEnv = optimizedRHS match {
            case l: IntLiteral => env + (name -> l)
            case _             => env - name
          }
          (acc :+ Assign(name, optimizedRHS), newEnv, false)

        case ExprStmt(expr) =>
          (acc :+ ExprStmt(optimiseExpr(expr, env)), env, false)

        case Return(expr) =>
          (acc :+ Return(optimiseExpr(expr, env)), env, true)

        case Func(name, params, body) =>
          val optimizedBody = optimiseBlock(body)
          (acc :+ Func(name, params, optimizedBody), env, false)

        case If(branches, elseBody) =>
          // 1. Optimise all branches
          val optBranches = branches.map { case (cond, body) =>
            (optimiseExpr(cond, env), optimiseBlock(body))
          }
          val optElse = elseBody.map(optimiseBlock)

          // 2. Determine which variables are "poisoned" (assigned to in any branch)
          // We must remove these from the constant environment for subsequent statements
          val assignedInIf = (branches.flatMap(_._2) ++ elseBody.getOrElse(Nil)).collect {
            case Assign(name, _) => name
          }.toSet

          val newEnv = env -- assignedInIf
          
          // Note: We don't mark 'hasReturned' as true unless we implement 
          // exhaustive return checking (i.e., every single path returns).
          (acc :+ If(optBranches, optElse), newEnv, false)
      }
    }
    finalStmts
  }

  private def optimiseExpr(node: Expr, env: Map[String, Expr]): Expr = node match {
    case Ident(name) => env.getOrElse(name, node)
    case Call(name, args) => Call(name, args.map(optimiseExpr(_, env)))

    case Add(l, r) => foldAdd(optimiseExpr(l, env), optimiseExpr(r, env))
    case Sub(l, r) => foldSub(optimiseExpr(l, env), optimiseExpr(r, env))
    case Mul(l, r) => foldMul(optimiseExpr(l, env), optimiseExpr(r, env))
    case Div(l, r) => foldDiv(optimiseExpr(l, env), optimiseExpr(r, env))
    
    case Eq(l, r)     => foldEq(optimiseExpr(l, env), optimiseExpr(r, env))
    case IdenEq(l, r) => foldIdenEq(optimiseExpr(l, env), optimiseExpr(r, env))

    case Dice(c, s)    => Dice(optimiseExpr(c, env), optimiseExpr(s, env))
    case Sum(inner)    => Sum(optimiseExpr(inner, env))
    case Prod(inner)   => Prod(optimiseExpr(inner, env))
    case CustomDist(d) => CustomDist(d)
    case i: IntLiteral => i
  }

  // ==========================================
  // Folding Logic
  // ==========================================

  private def foldAdd(left: Expr, right: Expr): Expr = (left, right) match {
    case (IntLiteral(a), IntLiteral(b)) => IntLiteral(a + b)
    case (x, IntLiteral(0)) => x
    case (IntLiteral(0), x) => x
    case _ => Add(left, right)
  }

  private def foldSub(left: Expr, right: Expr): Expr = (left, right) match {
    case (IntLiteral(a), IntLiteral(b)) => IntLiteral(a - b)
    case (x, IntLiteral(0)) => x
    case _ => Sub(left, right)
  }

  private def foldMul(left: Expr, right: Expr): Expr = (left, right) match {
    case (IntLiteral(a), IntLiteral(b)) => IntLiteral(a * b)
    case (_, IntLiteral(0)) | (IntLiteral(0), _) => IntLiteral(0)
    case (x, IntLiteral(1)) => x
    case (IntLiteral(1), x) => x
    case _ => Mul(left, right)
  }

  private def foldDiv(left: Expr, right: Expr): Expr = (left, right) match {
    case (IntLiteral(a), IntLiteral(b)) if b != 0 => IntLiteral(a / b)
    case (x, IntLiteral(1)) => x
    case _ => Div(left, right)
  }

  private def foldEq(left: Expr, right: Expr): Expr = (left, right) match {
    case (IntLiteral(a), IntLiteral(b)) => IntLiteral(if (a == b) 1 else 0)
    case _ => Eq(left, right)
  }

  private def foldIdenEq(left: Expr, right: Expr): Expr = (left, right) match {
    // Note: This is a simplified fold for identical distributions
    case (IntLiteral(a), IntLiteral(b)) => IntLiteral(if (a == b) 1 else 0)
    case (Ident(a), Ident(b)) if a == b => IntLiteral(1)
    case _ => IdenEq(left, right)
  }

  // ==========================================
  // PASS 2: Dead Store Elimination
  // ==========================================

  private def eliminateDeadStores(stmts: List[Stmt]): List[Stmt] = {
    val (reversedOptimized, _) = stmts.reverse.foldLeft((List.empty[Stmt], Set.empty[String])) {
      case ((acc, liveVars), stmt) => stmt match {
        
        case Assign(name, expr) =>
          if (liveVars.contains(name)) {
            val newLive = (liveVars - name) ++ getUsedVars(expr)
            (stmt :: acc, newLive)
          } else (acc, liveVars)

        case ExprStmt(expr) => (stmt :: acc, liveVars ++ getUsedVars(expr))
        case Return(expr)   => (stmt :: acc, liveVars ++ getUsedVars(expr))

        case f @ Func(_, params, body) =>
          val capturedVars = body.flatMap(getUsedVarsStmt).toSet -- params.toSet
          (f :: acc, liveVars ++ capturedVars)

        case If(branches, elseBody) =>
          val branchUses = branches.flatMap { case (cond, body) => 
            getUsedVars(cond) ++ body.flatMap(getUsedVarsStmt) 
          }
          val elseUses = elseBody.getOrElse(Nil).flatMap(getUsedVarsStmt)
          (stmt :: acc, liveVars ++ branchUses ++ elseUses)
      }
    }
    reversedOptimized
  }

  private def getUsedVarsStmt(stmt: Stmt): Set[String] = stmt match {
    case Assign(_, expr) => getUsedVars(expr)
    case ExprStmt(expr)  => getUsedVars(expr)
    case Return(expr)    => getUsedVars(expr)
    case Func(_, params, body) => body.flatMap(getUsedVarsStmt).toSet -- params.toSet
    case If(branches, elseBody) => 
      val bVars = branches.flatMap { case (c, b) => getUsedVars(c) ++ b.flatMap(getUsedVarsStmt) }
      val eVars = elseBody.getOrElse(Nil).flatMap(getUsedVarsStmt)
      bVars.toSet ++ eVars
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
    case Sum(i)        => getUsedVars(i)
    case Prod(i)       => getUsedVars(i)
    case _             => Set.empty
  }
}