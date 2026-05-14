package DSL.backend

import DSL.frontend.AST._
import DSL.backend.typedAST._
import DSL.backend.{DistTy, ScalarTy, UnknownTy, GenericDistTy}
import scala.collection.mutable

sealed trait TypeError
case class NonScalarComparison(op: String, leftTy: DistTy, rightTy: DistTy) extends TypeError
case class ArgTypeMismatch(funcName: String, paramName: String, expected: DistTy, actual: DistTy) extends TypeError

object typeChecker {

  def check(program: Program): List[TypeError] = {
    val errors = mutable.ListBuffer.empty[TypeError]
    val funcEnv = program.topLevel.collect { case Left(f: Func) => f.name -> f }.toMap
    val funcConstraints = mutable.Map.empty[String, Map[String, DistTy]]
    
    // Pass 1: Derive parameter constraints for all functions
    program.topLevel.foreach {
      case Left(Func(name, params, body)) =>
        funcConstraints(name) = deriveConstraints(body, params.toSet)
      case _ => ()
    }
    
    // Pass 2: Traverse and check everything with a type environment
    var typeEnv = Map.empty[String, DistTy]

    def checkExpr(expr: Expr): Unit = {
      val tyExpr = typer.annotate(expr)
      checkTyExpr(tyExpr)
    }

    def checkTyExpr(tyExpr: TyExpr): Unit = tyExpr match {
      case TyIntLiteral(_, _) => ()
      case TyIdent(_, _) => ()
      case TyCustomDist(_, _) => ()
      
      case TyCall(name, args, _) =>
        args.foreach(checkTyExpr)
        // Check call site against derived constraints
        funcConstraints.get(name).foreach { constraints =>
          val func = funcEnv(name)
          args.zip(func.params).foreach { case (argTyExpr, paramName) =>
            constraints.get(paramName).foreach { required =>
              val actual = inferTyType(argTyExpr, typeEnv)
              if (!satisfies(actual, required)) {
                errors += ArgTypeMismatch(name, paramName, required, actual)
              }
            }
          }
        }
        
      case TyMapExpr(funcName, inner, _) =>
        checkTyExpr(inner)
        
      case TyBlock(stmts, finalExpr, _) =>
        stmts.foreach {
          case Assign(name, e) =>
            checkExpr(e)
            typeEnv = typeEnv.updated(name, inferType(e, typeEnv))
          case Func(_, _, _) => ()
        }
        checkTyExpr(finalExpr)
        
      case TyIfExpr(branches, elseB, _) =>
        branches.foreach { branch =>
          branch.bindings.foreach(b => checkExpr(b.expr))
          checkTyExpr(branch.condition)
          checkTyExpr(branch.body)
        }
        checkTyExpr(elseB)
        
      case TyUnary(_, inner, _) => checkTyExpr(inner)
        
      case TyBinary(op, l, r, _) =>
        checkTyExpr(l)
        checkTyExpr(r)
        
        // Removed: Type checking for scalars in binary comparisons (lt, le, gt, ge)
        // Comparisons now work on generic distributions.
    }

    program.topLevel.foreach {
      case Left(stmt) =>
        stmt match {
          case Assign(name, expr) =>
            checkExpr(expr)
            typeEnv = typeEnv.updated(name, inferType(expr, typeEnv))
          case Func(_, _, body) => checkExpr(body)
        }
      case Right(expr) => checkExpr(expr)
    }
    
    errors.toList
  }

  /** 
   *  Derives parameter constraints from a function body. 
   *  Previously enforced ScalarTy for comparison ops; now treats them generally.
   */
  private def deriveConstraints(body: Expr, paramNames: Set[String]): Map[String, DistTy] = {
    val constraints = mutable.Map.empty[String, DistTy]
    val tyBody = typer.annotate(body)
    
    // Removed: Specific constraint logic for comparison operators
    
    def walk(tyExpr: TyExpr): Unit = tyExpr match {
      case TyBinary(_, l, r, _) => walk(l); walk(r)
      case TyUnary(_, inner, _) => walk(inner)
      case TyMapExpr(_, inner, _) => walk(inner)
      
      case TyBlock(stmts, finalExpr, _) =>
        stmts.foreach {
          case Assign(_, e) => walk(typer.annotate(e))
          case Func(_, _, _) => ()
        }
        walk(finalExpr)
        
      case TyIfExpr(branches, elseB, _) =>
        branches.foreach { branch =>
          branch.bindings.foreach(b => walk(typer.annotate(b.expr)))
          walk(branch.condition)
          walk(branch.body)
        }
        walk(elseB)
        
      case TyCall(_, args, _) => args.foreach(walk)
      case _ => ()
    }
    
    walk(tyBody)
    constraints.toMap
  }

  /** Infers the type of an expression given a variable type environment */
  private def inferType(expr: Expr, typeEnv: Map[String, DistTy]): DistTy = {
    val t = typer.annotate(expr)
    inferTyType(t, typeEnv)
  }

  /** Resolves variable types using the environment; falls back to the annotated type */
  private def inferTyType(tyExpr: TyExpr, typeEnv: Map[String, DistTy]): DistTy = tyExpr match {
    case TyIdent(name, _) => typeEnv.getOrElse(name, UnknownTy)
    case _ => tyExpr.ty
  }

  private def isScalarOrUnknown(ty: DistTy): Boolean = 
    ty == ScalarTy || ty == UnknownTy

  private def satisfies(actual: DistTy, required: DistTy): Boolean = 
    if (required == ScalarTy) actual == ScalarTy || actual == UnknownTy
    else true
}