package DSL.backend

import DSL.frontend.AST._

object optimiser {

  def optimise(program: Program): Program = program match {
    case Program(stmts) => Program(optimiseBlock(stmts))
  }

  /**
   * Optimises a list of statements.
   */
  private def optimiseBlock(stmts: List[Stmt]): List[Stmt] = {
    // Pass 1: Constant Propagation + Unreachable Code Removal (after Return)
    val propagated = propagateConstants(stmts)
    
    // Pass 2: Dead Store Elimination
    eliminateDeadStores(propagated)
  }

  // ==========================================
  // PASS 1: Constant Propagation & Unreachable Code Removal
  // ==========================================

  private def propagateConstants(stmts: List[Stmt]): List[Stmt] = {
    // Accumulator: (Accumulated Statements, Environment, HasReturnedFlag)
    val (finalStmts, _, _) = stmts.foldLeft((List.empty[Stmt], Map.empty[String, Expr], false)) {
      
      // If we have already hit a return statement, skip everything else (Dead Code)
      case ((acc, env, true), _) => 
        (acc, env, true)

      // Otherwise, process the statement
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
          // Optimize the return value
          val optReturn = Return(optimiseExpr(expr, env))
          // Add to list, AND set hasReturned = true
          (acc :+ optReturn, env, true)

        case Func(name, params, body) =>
          // We do not pass the global env into the function body during static optimization 
          // because globals might be mutated between function declaration and invocation.
          val optimizedBody = optimiseBlock(body)
          (acc :+ Func(name, params, optimizedBody), env, false)
      }
    }
    finalStmts
  }

  /**
   * Recursively simplifies an expression.
   */
  private def optimiseExpr(node: Expr, env: Map[String, Expr]): Expr = node match {
    case Ident(name) => env.getOrElse(name, node)

    case Call(name, args) => Call(name, args.map(optimiseExpr(_, env)))

    case Add(l, r) => foldAdd(optimiseExpr(l, env), optimiseExpr(r, env))
    case Sub(l, r) => foldSub(optimiseExpr(l, env), optimiseExpr(r, env))
    case Mul(l, r) => foldMul(optimiseExpr(l, env), optimiseExpr(r, env))
    case Div(l, r) => foldDiv(optimiseExpr(l, env), optimiseExpr(r, env))

    case Dice(c, s)  => Dice(optimiseExpr(c, env), optimiseExpr(s, env))
    case Sum(inner)  => Sum(optimiseExpr(inner, env))
    case Prod(inner) => Prod(optimiseExpr(inner, env))
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
    case (_, IntLiteral(0)) => IntLiteral(0)
    case (IntLiteral(0), _) => IntLiteral(0)
    case (x, IntLiteral(1)) => x
    case (IntLiteral(1), x) => x
    case _ => Mul(left, right)
  }

  private def foldDiv(left: Expr, right: Expr): Expr = (left, right) match {
    case (IntLiteral(a), IntLiteral(b)) if b != 0 => IntLiteral(a / b)
    case (x, IntLiteral(1)) => x
    case _ => Div(left, right)
  }

  // ==========================================
  // PASS 2: Dead Store Elimination
  // ==========================================

  private def eliminateDeadStores(stmts: List[Stmt]): List[Stmt] = {
    // Traverse backwards
    val (reversedOptimized, _) = stmts.reverse.foldLeft((List.empty[Stmt], Set.empty[String])) {
      case ((acc, liveVars), stmt) => stmt match {
        
        case Assign(name, expr) =>
          if (liveVars.contains(name)) {
            val newLive = (liveVars - name) ++ getUsedVars(expr)
            (stmt :: acc, newLive)
          } else {
            // Dead store eliminated!
            (acc, liveVars)
          }

        case ExprStmt(expr) =>
          (stmt :: acc, liveVars ++ getUsedVars(expr))

        case Return(expr) =>
          (stmt :: acc, liveVars ++ getUsedVars(expr))

        case f @ Func(_, params, body) =>
          // Extract variables captured by the function body (excluding its own parameters)
          val capturedVars = body.map(getUsedVarsStmt).foldLeft(Set.empty[String])(_ ++ _) -- params.toSet
          (f :: acc, liveVars ++ capturedVars)
      }
    }
    reversedOptimized
  }

  private def getUsedVarsStmt(stmt: Stmt): Set[String] = stmt match {
    case Assign(_, expr) => getUsedVars(expr)
    case ExprStmt(expr)  => getUsedVars(expr)
    case Return(expr)    => getUsedVars(expr)
    case Func(_, params, body) => 
      body.map(getUsedVarsStmt).foldLeft(Set.empty[String])(_ ++ _) -- params.toSet
  }

  private def getUsedVars(expr: Expr): Set[String] = expr match {
    case Ident(name) => Set(name)
    case Call(_, args) => args.map(getUsedVars).foldLeft(Set.empty[String])(_ ++ _)
    case Add(l, r)   => getUsedVars(l) ++ getUsedVars(r)
    case Sub(l, r)   => getUsedVars(l) ++ getUsedVars(r)
    case Mul(l, r)   => getUsedVars(l) ++ getUsedVars(r)
    case Div(l, r)   => getUsedVars(l) ++ getUsedVars(r)
    case Dice(c, s)  => getUsedVars(c) ++ getUsedVars(s)
    case Sum(i)      => getUsedVars(i)
    case Prod(i)     => getUsedVars(i)
    case _           => Set.empty
  }
}