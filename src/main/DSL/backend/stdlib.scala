package DSL.backend

import typedAST._
import semanticTypes._

object Builtins {
  extension (v: Value) {
    private def asScalar: Int = v match {
      case DistValue(d) if d.size == 1 => d.keys.head
      case _ => throw new IllegalArgumentException(s"Expected scalar, got $v")
    }
    private def asDist: Distribution = v match {
      case DistValue(d) => d
      case _ => throw new IllegalArgumentException(s"Expected dist, got $v")
    }
  }

  case class BuiltinFunction(
    name: String,
    paramTypes: List[Ty], // The expected types for the arguments
    implementation: (List[Value], DistributionSemantics) => Value
  )

  private val functions: List[BuiltinFunction] = List(
    BuiltinFunction("keepLargest", List(DistTy(ScalarTy), DistTy(ScalarTy), DistTy(GenericTy)),
      (args, sem) => {
        val k = args(0).asScalar
        val n = args(1).asScalar
        val d = args(2).asDist
        DistValue(MathOps.keepLargest(k, n, d))
      }
    ),
    
    BuiltinFunction("keepSmallest", List(DistTy(ScalarTy), DistTy(ScalarTy), DistTy(GenericTy)),
      (args, sem) => {
        val k = args(0).asScalar
        val n = args(1).asScalar
        val d = args(2).asDist
        DistValue(MathOps.keepSmallest(k, n, d))
      }
    ),

    BuiltinFunction("dropLargest", List(DistTy(ScalarTy), DistTy(ScalarTy), DistTy(GenericTy)),
      (args, sem) => {
        val k = args(0).asScalar
        val n = args(1).asScalar
        val d = args(2).asDist
        DistValue(MathOps.dropLargest(k, n, d))
      }
    ),

    BuiltinFunction("dropSmallest", List(DistTy(ScalarTy), DistTy(ScalarTy), DistTy(GenericTy)),
      (args, sem) => {
        val k = args(0).asScalar
        val n = args(1).asScalar
        val d = args(2).asDist
        DistValue(MathOps.dropSmallest(k, n, d))
      }
    )
  )

  // Public access map
  val all: Map[String, BuiltinFunction] = functions.map(f => f.name -> f).toMap
}