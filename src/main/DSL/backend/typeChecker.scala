package DSL.backend

import DSL.frontend.AST._
import DSL.backend.typedAST._
import DSL.backend.{DistTy, ScalarTy, UnknownTy, GenericDistTy, PoolTy}
import scala.collection.mutable

sealed trait TypeError
case class NonScalarComparison(op: String, leftTy: DistTy, rightTy: DistTy) extends TypeError
case class ArgTypeMismatch(funcName: String, paramName: String, expected: DistTy, actual: DistTy) extends TypeError
case class DiceCountMustBeScalar(actualTy: DistTy) extends TypeError

object typeChecker {

  def check(program: Program): List[TypeError] = {
    val errors = mutable.ListBuffer.empty[TypeError]
    val funcEnv = program.topLevel.collect { case Left(f: Func) => f.name -> f }.toMap
    
    // Build explicit signatures based on AST parameter types
    val funcSignatures: Map[String, Map[String, DistTy]] = 
      funcEnv.map { case (name, func) =>
        name -> func.params.map { p =>
          val ty = p.typ match {
            case Some(PoolType) => PoolTy
            case _ => GenericDistTy
          }
          p.name -> ty
        }.toMap
      }
    
    var typeEnv = Map.empty[String, DistTy]

    def checkExpr(expr: Expr): Unit = {
      val tyExpr = typer.annotate(expr)
      checkTyExpr(tyExpr)
    }

    def isScalar(expr: TyExpr): Boolean = expr.ty == ScalarTy

    def checkTyExpr(tyExpr: TyExpr): Unit = tyExpr match {
      case TyIntLiteral(_, _) => ()
      case TyIdent(_, _) => ()
      case TyCustomDist(_, _) => ()
      
      case TyPool(items, _) => items.foreach(checkTyExpr)
      case TyPoolConcat(l, r, _) => 
        checkTyExpr(l)
        checkTyExpr(r)
      
      case TyBinary(BinaryOp.Dice, countExpr, sidesExpr, _) =>
        if (!isScalar(countExpr)) {
          errors += DiceCountMustBeScalar(countExpr.ty)
        }
        checkTyExpr(countExpr)
        checkTyExpr(sidesExpr)

      case TyUnary(op, inner, _) => op match {
        case UnaryOp.Sum | UnaryOp.Prod =>
          checkTyExpr(inner)
        case _ => checkTyExpr(inner)
      }
      
      case TyCall(name, args, _) =>
        args.foreach(checkTyExpr)
        
        name match {
          case "keepLargest" | "keepSmallest" | "dropLargest" | "dropSmallest" =>
            if (args.size == 3) {
              if (!isScalar(args(0))) errors += ArgTypeMismatch(name, "count (arg 0)", ScalarTy, args(0).ty)
              if (!isScalar(args(1))) errors += ArgTypeMismatch(name, "pool (arg 1)", ScalarTy, args(1).ty)
            }
          case _ => 
            funcSignatures.get(name).foreach { signature =>
              funcEnv.get(name).foreach { funcDef =>
                // Zip arguments with the function's defined parameters
                args.zip(funcDef.params).foreach { case (argTyExpr, param) =>
                  signature.get(param.name).foreach { required =>
                    val actual = inferTyType(argTyExpr, typeEnv)
                    if (!satisfies(actual, required)) {
                      errors += ArgTypeMismatch(name, param.name, required, actual)
                    }
                  }
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
        
      case TyBinary(op, l, r, _) =>
        checkTyExpr(l)
        checkTyExpr(r)
    }

    program.topLevel.foreach {
      case Left(stmt) =>
        stmt match {
          case Assign(name, expr) =>
            checkExpr(expr)
            typeEnv = typeEnv.updated(name, inferType(expr, typeEnv))
          case Func(name, params, body) => 
            val oldEnv = typeEnv
            val signature = funcSignatures.getOrElse(name, Map.empty)
            val newEnv = typeEnv ++ params.map { p => 
              p.name -> signature.getOrElse(p.name, UnknownTy)
            }
            typeEnv = newEnv
            checkExpr(body)
            typeEnv = oldEnv
        }
      case Right(expr) => checkExpr(expr)
    }
    
    errors.toList
  }

  private def inferType(expr: Expr, typeEnv: Map[String, DistTy]): DistTy = {
    val t = typer.annotate(expr)
    inferTyType(t, typeEnv)
  }

  private def inferTyType(tyExpr: TyExpr, typeEnv: Map[String, DistTy]): DistTy = tyExpr match {
    case TyIdent(name, _) => typeEnv.getOrElse(name, UnknownTy)
    case _ => tyExpr.ty
  }

  private def isScalarOrUnknown(ty: DistTy): Boolean = 
    ty == ScalarTy || ty == UnknownTy

  private def satisfies(actual: DistTy, required: DistTy): Boolean = {
    if (required == PoolTy) {
      actual == PoolTy
    } 
    else if (required == GenericDistTy) {
      actual != PoolTy // Removed "&& actual != UnknownTy"
    }
    else if (required == ScalarTy) {
      isScalarOrUnknown(actual)
    }
    else {
      true
    }
  }
}