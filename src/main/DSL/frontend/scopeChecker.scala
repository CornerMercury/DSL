package DSL.frontend

import DSL.frontend.AST._
import scala.collection.mutable

/** Scope errors reported by the scope checker (no source positions in AST). */
sealed trait ScopeError
case class UndeclaredVariable(name: String) extends ScopeError
case class UndeclaredFunction(name: String) extends ScopeError // <-- ADDED
case class DuplicateFunction(name: String) extends ScopeError
case class DuplicateParameter(name: String) extends ScopeError
case object ReturnOutsideFunction extends ScopeError

/** Verifies that all identifiers are declared before use and scopes are correct. */
object scopeChecker {

  /** Returns a list of scope errors; empty means success. */
  def check(program: Program): List[ScopeError] = {
    val errors = mutable.ListBuffer.empty[ScopeError]
    val scopes = mutable.Stack.empty[mutable.Set[String]]
    val declaredFuncs = mutable.Set.empty[String]

    def downLayer(): Unit = scopes.push(mutable.Set.empty)
    def upLayer(): Unit = scopes.pop()
    def currentScope: mutable.Set[String] = scopes.head

    // Checks if a variable exists in the current or any parent scope
    def isInScope(name: String): Boolean = scopes.exists(_.contains(name))

    // Registers a variable in the current scope.
    // Since we allow redeclaration (updates), we just add it.
    // If it exists, it effectively updates it; if not, it declares it.
    def declareVar(name: String): Unit = {
      currentScope.add(name)
    }

    def declareFunc(name: String): Boolean = {
      if (declaredFuncs.contains(name)) {
        errors += DuplicateFunction(name)
        false
      } else {
        declaredFuncs.add(name)
        true
      }
    }

    def checkExpr(expr: Expr): Unit = expr match {
      case Ident(name) =>
        if (!isInScope(name)) errors += UndeclaredVariable(name)
      case Call(name, args) =>
        if (!declaredFuncs.contains(name)) errors += UndeclaredFunction(name)
        args.foreach(checkExpr)
      case Dice(c, s) =>
        checkExpr(c)
        checkExpr(s)
      case Sum(inner) =>
        checkExpr(inner)
      case Prod(inner) =>
        checkExpr(inner)
      case Add(l, r) =>
        checkExpr(l)
        checkExpr(r)
      case Sub(l, r) =>
        checkExpr(l)
        checkExpr(r)
      case Mul(l, r) =>
        checkExpr(l)
        checkExpr(r)
      case Div(l, r) =>
        checkExpr(l)
        checkExpr(r)
      case IntLiteral(_) | CustomDist(_) =>
        ()
    }

    def checkStmt(stmt: Stmt, inFunction: Boolean): Unit = stmt match {
      case Assign(name, expr) =>
        // 1. Check the expression on the right first.
        // This ensures cases like "x = x + 1" fail if x wasn't defined BEFORE this line.
        checkExpr(expr)
        
        // 2. Now declare (or update) the variable name in the current scope.
        declareVar(name)

      case ExprStmt(expr) =>
        checkExpr(expr)

      case Return(expr) =>
        if (!inFunction) errors += ReturnOutsideFunction
        checkExpr(expr)

      case Func(name, params, body) =>
        downLayer()
        val paramSet = mutable.Set.empty[String]
        params.foreach { p =>
          if (paramSet.contains(p)) errors += DuplicateParameter(p)
          else { 
            paramSet.add(p)
            declareVar(p) // Parameters count as declarations in the new scope
          }
        }
        body.foreach(checkStmt(_, inFunction = true))
        upLayer()
    }

    program match {
      case Program(stmts) =>
        downLayer()
        stmts.foreach {
          case Func(name, _, _) => declareFunc(name)
          case _                =>
        }
        stmts.foreach(checkStmt(_, inFunction = false))
        upLayer()
    }

    errors.toList
  }
}