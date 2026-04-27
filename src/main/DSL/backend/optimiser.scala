package DSL.backend

import DSL.frontend.AST._

object optimiser {

  def optimise(program: Program): Program = Program(optimiseBlock(program.stmts))

  private def optimiseBlock(stmts: List[Stmt]): List[Stmt] = {
    eliminateDeadStores(propagateConstants(stmts))
  }

  private def propagateConstants(stmts: List[Stmt]): List[Stmt] = {
    val (finalStmts, _, _) = stmts.foldLeft((List.empty[Stmt], Map.empty[String, Expr], false)) {
      case ((acc, env, true), _) => (acc, env, true)
      case ((acc, env, false), stmt) => stmt match {
        case Assign(name, expr) =>
          val opt = optimiseExpr(expr, env)
          val newEnv = opt match { case l: IntLiteral => env + (name -> l); case _ => env - name }
          (acc :+ Assign(name, opt), newEnv, false)

        case ExprStmt(e) => (acc :+ ExprStmt(optimiseExpr(e, env)), env, false)
        case Return(e)   => (acc :+ Return(optimiseExpr(e, env)), env, true)
        case Func(n, p, b) => (acc :+ Func(n, p, optimiseBlock(b)), env, false)

        case If(branches, elseBody) =>
          val optBranches = branches.map { b =>
            // Optimising the 'Expr' inside the RollBinding(String, Expr)
            val optBinds = b.bindings.map(x => x.copy(expr = optimiseExpr(x.expr, env)))
            Branch(optBinds, optimiseExpr(b.condition, env), optimiseBlock(b.body))
          }
          val optElse = elseBody.map(optimiseBlock)
          val poisoned = (branches.flatMap(_.body) ++ elseBody.getOrElse(Nil)).collect { case Assign(n, _) => n }.toSet
          (acc :+ If(optBranches, optElse), env -- poisoned, false)
      }
    }
    finalStmts
  }

  private def optimiseExpr(node: Expr, env: Map[String, Expr]): Expr = node match {
    case Ident(n)      => env.getOrElse(n, node)
    case Add(l, r)     => foldAdd(optimiseExpr(l, env), optimiseExpr(r, env))
    case Sub(l, r)     => foldSub(optimiseExpr(l, env), optimiseExpr(r, env))
    case Mul(l, r)     => foldMul(optimiseExpr(l, env), optimiseExpr(r, env))
    case Div(l, r)     => foldDiv(optimiseExpr(l, env), optimiseExpr(r, env))
    case Eq(l, r)      => foldEq(optimiseExpr(l, env), optimiseExpr(r, env))
    case Call(n, args) => Call(n, args.map(optimiseExpr(_, env)))
    case Dice(c, s)    => Dice(optimiseExpr(c, env), optimiseExpr(s, env))
    case Sum(i)        => Sum(optimiseExpr(i, env))
    case Prod(i)       => Prod(optimiseExpr(i, env))
    case other         => other
  }

  private def foldAdd(l: Expr, r: Expr) = (l, r) match {
    case (IntLiteral(a), IntLiteral(b)) => IntLiteral(a + b); case _ => Add(l, r)
  }
  private def foldSub(l: Expr, r: Expr) = (l, r) match {
    case (IntLiteral(a), IntLiteral(b)) => IntLiteral(a - b); case _ => Sub(l, r)
  }
  private def foldMul(l: Expr, r: Expr) = (l, r) match {
    case (IntLiteral(a), IntLiteral(b)) => IntLiteral(a * b); case _ => Mul(l, r)
  }
  private def foldDiv(l: Expr, r: Expr) = (l, r) match {
    case (IntLiteral(a), IntLiteral(b)) if b != 0 => IntLiteral(a / b); case _ => Div(l, r)
  }
  private def foldEq(l: Expr, r: Expr) = (l, r) match {
    case (IntLiteral(a), IntLiteral(b)) => IntLiteral(if (a == b) 1 else 0); case _ => Eq(l, r)
  }

  private def eliminateDeadStores(stmts: List[Stmt]): List[Stmt] = {
    val (rev, _) = stmts.reverse.foldLeft((List.empty[Stmt], Set.empty[String])) {
      case ((acc, live), stmt) => stmt match {
        case Assign(n, e) if live.contains(n) => (stmt :: acc, (live - n) ++ getUsed(e))
        case Assign(_, _) => (acc, live)
        case _ => (stmt :: acc, live ++ getUsedStmt(stmt))
      }
    }
    rev
  }

  private def getUsedStmt(s: Stmt): Set[String] = s match {
    case Assign(_, e) => getUsed(e)
    case ExprStmt(e)  => getUsed(e)
    case Return(e)    => getUsed(e)
    case Func(_, p, b) => b.flatMap(getUsedStmt).toSet -- p.toSet
    case If(br, el) => 
      val bUses = br.flatMap(b => 
        b.bindings.flatMap(x => getUsed(x.expr)) ++ 
        getUsed(b.condition) ++ 
        b.body.flatMap(getUsedStmt)
      )
      bUses.toSet ++ el.getOrElse(Nil).flatMap(getUsedStmt)
  }

  private def getUsed(e: Expr): Set[String] = e match {
    case Ident(n)      => Set(n)
    case Call(_, args) => args.flatMap(getUsed).toSet
    case Add(l, r)     => getUsed(l) ++ getUsed(r)
    case Sub(l, r)     => getUsed(l) ++ getUsed(r)
    case Mul(l, r)     => getUsed(l) ++ getUsed(r)
    case Div(l, r)     => getUsed(l) ++ getUsed(r)
    case Eq(l, r)      => getUsed(l) ++ getUsed(r)
    case Dice(c, s)    => getUsed(c) ++ getUsed(s)
    case Sum(i)        => getUsed(i)
    case Prod(i)       => getUsed(i)
    case _             => Set.empty
  }
}