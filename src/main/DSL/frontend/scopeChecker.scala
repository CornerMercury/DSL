package DSL.frontend

import DSL.frontend.AST._
import scala.collection.mutable

sealed trait ScopeError
case class UndeclaredVariable(name: String) extends ScopeError
case class UndeclaredFunction(name: String) extends ScopeError
case class DuplicateFunction(name: String) extends ScopeError
case class DuplicateParameter(name: String) extends ScopeError
case class MissingReturnPath(funcName: String) extends ScopeError
case object ReturnOutsideFunction extends ScopeError

object scopeChecker {

  def check(program: Program): List[ScopeError] = {
    val errors = mutable.ListBuffer.empty[ScopeError]
    val scopes = mutable.Stack.empty[mutable.Set[String]]
    val declaredFuncs = mutable.Set.empty[String]

    def downLayer(): Unit = scopes.push(mutable.Set.empty)
    def upLayer(): Unit = scopes.pop()
    def currentScope: mutable.Set[String] = scopes.head
    def isInScope(name: String): Boolean = scopes.exists(_.contains(name))
    def declareVar(name: String): Unit = currentScope.add(name)

    def declareFunc(name: String): Unit = {
      if (declaredFuncs.contains(name)) errors += DuplicateFunction(name)
      else declaredFuncs.add(name)
    }

    /** 
     * Recursively checks if a list of statements is guaranteed to hit a Return.
     */
    def returnsAlways(stmts: List[Stmt]): Boolean = {
      stmts.foldLeft(false) { (guaranteed, stmt) =>
        if (guaranteed) true // If a previous statement already guaranteed return
        else stmt match {
          case Return(_) => true
          case If(branches, Some(elseBody)) =>
            // An If only guarantees return if ALL branches and the ELSE guarantee return
            branches.forall(b => returnsAlways(b.body)) && returnsAlways(elseBody)
          case _ => false
        }
      }
    }

    def checkExpr(expr: Expr): Unit = expr match {
      case Ident(name) => if (!isInScope(name)) errors += UndeclaredVariable(name)
      case Call(name, args) =>
        if (!declaredFuncs.contains(name)) errors += UndeclaredFunction(name)
        args.foreach(checkExpr)
      case Dice(c, s) => checkExpr(c); checkExpr(s)
      case Sum(i)     => checkExpr(i)
      case Prod(i)    => checkExpr(i)
      case Add(l, r)  => checkExpr(l); checkExpr(r)
      case Sub(l, r)  => checkExpr(l); checkExpr(r)
      case Mul(l, r)  => checkExpr(l); checkExpr(r)
      case Div(l, r)  => checkExpr(l); checkExpr(r)
      case Eq(l, r)   => checkExpr(l); checkExpr(r)
      case IntLiteral(_) | CustomDist(_) => ()
    }

    def checkStmt(stmt: Stmt, inFunction: Boolean): Unit = stmt match {
      case Assign(name, expr) => checkExpr(expr); declareVar(name)
      case ExprStmt(expr)     => checkExpr(expr)
      case Return(expr)       => 
        if (!inFunction) errors += ReturnOutsideFunction
        checkExpr(expr)

      case Func(name, params, body) =>
        if (!returnsAlways(body)) errors += MissingReturnPath(name)
        downLayer()
        val paramSet = mutable.Set.empty[String]
        params.foreach { p =>
          if (paramSet.contains(p)) errors += DuplicateParameter(p)
          else { paramSet.add(p); declareVar(p) }
        }
        body.foreach(checkStmt(_, inFunction = true))
        upLayer()

      case If(branches, elseBody) =>
        downLayer() 
        branches.foreach { branch =>
          branch.bindings.foreach { b => checkExpr(b.expr); declareVar(b.name) }
          checkExpr(branch.condition)
          downLayer()
          branch.body.foreach(checkStmt(_, inFunction))
          upLayer()
        }
        elseBody.foreach { body =>
          downLayer()
          body.foreach(checkStmt(_, inFunction))
          upLayer()
        }
        upLayer() 
    }

    program match {
      case Program(stmts) =>
        downLayer()
        stmts.foreach { case f: Func => declareFunc(f.name); case _ => }
        stmts.foreach(checkStmt(_, inFunction = false))
        upLayer()
    }
    errors.toList
  }
}