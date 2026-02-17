package DSL.backend

import DSL.frontend.AST._
import semanticTypes.classify
import typedAST._

/** Probability distribution over integer outcomes. Probabilities sum to 1.0 (within rounding). */
type Distribution = Map[Int, Double]

object interpreter {

  /** Interpret only Sum or Prod at the root; rejects other top-level expressions. */
  def interpret(expr: Expr): Distribution = interpretWithTypes(expr)._1

  /** Self-optimizing: interpret and build typed AST (Scalar > Binomial > Uniform > Generic) in one pass. */
  def interpretWithTypes(expr: Expr): (Distribution, TyExpr) = expr match {
    case Sum(_) | Prod(_) =>
      evalExpr(expr, DiceMode.Sum) // initial mode is irrelevant; Sum/Prod nodes override it
    case other =>
      throw new IllegalArgumentException(s"Interpreter expects root to be Sum or Prod, got: $other")
  }

  private enum DiceMode {
    case Sum, Prod
  }

  private def evalExpr(expr: Expr, mode: DiceMode): (Distribution, TyExpr) = expr match {
    case IntLiteral(n) =>
      val d = Map(n -> 1.0)
      (d, TyIntLiteral(n, classify(d)))

    case Sum(inner) =>
      val (d, tyInner) = evalExpr(inner, DiceMode.Sum)
      (d, TyUnary(UnaryOp.Sum, tyInner, classify(d)))

    case Prod(inner) =>
      val (d, tyInner) = evalExpr(inner, DiceMode.Prod)
      (d, TyUnary(UnaryOp.Prod, tyInner, classify(d)))

    case Dice(c, s) =>
      val (dC, tyC) = evalExpr(c, mode)
      val (dS, tyS) = evalExpr(s, mode)
      val d = combineDice(dC, dS, diceDistributionFor(mode))
      (d, TyBinary(BinaryOp.Dice, tyC, tyS, classify(d)))

    case Add(l, r) =>
      val (dL, tyL) = evalExpr(l, mode)
      val (dR, tyR) = evalExpr(r, mode)
      val d = convolve(dL, dR, _ + _)
      (d, TyBinary(BinaryOp.Add, tyL, tyR, classify(d)))

    case Sub(l, r) =>
      val (dL, tyL) = evalExpr(l, mode)
      val (dR, tyR) = evalExpr(r, mode)
      val d = convolveSub(dL, dR)
      (d, TyBinary(BinaryOp.Sub, tyL, tyR, classify(d)))

    case Mul(l, r) =>
      val (dL, tyL) = evalExpr(l, mode)
      val (dR, tyR) = evalExpr(r, mode)
      val d = convolve(dL, dR, _ * _)
      (d, TyBinary(BinaryOp.Mul, tyL, tyR, classify(d)))

    case Div(l, r) =>
      val (dL, tyL) = evalExpr(l, mode)
      val (dR, tyR) = evalExpr(r, mode)
      val d = convolveDiv(dL, dR)
      (d, TyBinary(BinaryOp.Div, tyL, tyR, classify(d)))
  }

  /** For each (c, s) with prob p_c * p_s, add the distribution of c dice with s sides (sum). */
  private[backend] def combineDice(
      countDist: Distribution,
      sidesDist: Distribution,
      diceDist: (Int, Int) => Distribution
  ): Distribution = {
    var result: Distribution = Map.empty
    for ((c, pC) <- countDist; (s, pS) <- sidesDist) {
      val p = pC * pS
      val d = diceDist(safeCount(c), safeSides(s))
      result = mergeDistributions(result, scaleDistribution(d, p))
    }
    result
  }

  private[backend] def safeCount(c: Int): Int = if (c < 0) 0 else c
  private[backend] def safeSides(s: Int): Int = if (s < 1) 1 else s

  /** Distribution of the sum of `count` dice each with faces 1..sides (uniform). */
  private[backend] def diceSumDistribution(count: Int, sides: Int): Distribution =
    diceDistribution(count, sides, zero = 0, _ + _)

  /** Distribution of the product of `count` dice each with faces 1..sides (uniform). */
  private[backend] def diceProductDistribution(count: Int, sides: Int): Distribution =
    diceDistribution(count, sides, zero = 1, _ * _)

  private def diceDistribution(
      count: Int,
      sides: Int,
      zero: Int,
      combine: (Int, Int) => Int
  ): Distribution = {
    if (count <= 0) return Map(zero -> 1.0)
    val oneDie = (1 to sides).iterator.map(_ -> (1.0 / sides)).toMap
    (1 until count).foldLeft(oneDie) { (acc, _) => convolve(acc, oneDie, combine) }
  }

  private def diceDistributionFor(mode: DiceMode): (Int, Int) => Distribution = mode match {
    case DiceMode.Sum  => diceSumDistribution
    case DiceMode.Prod => diceProductDistribution
  }

  /** Convolve two distributions with combination function f(v1, v2). */
  private[backend] def convolve(d1: Distribution, d2: Distribution, f: (Int, Int) => Int): Distribution = {
    var result: Distribution = Map.empty
    for ((v1, p1) <- d1; (v2, p2) <- d2) {
      val v = f(v1, v2)
      result = result.updated(v, result.getOrElse(v, 0.0) + p1 * p2)
    }
    result
  }

  private[backend] def convolveSub(d1: Distribution, d2: Distribution): Distribution =
    convolve(d1, d2, _ - _)

  private[backend] def convolveDiv(d1: Distribution, d2: Distribution): Distribution = {
    var result: Distribution = Map.empty
    for ((v1, p1) <- d1; (v2, p2) <- d2 if v2 != 0) {
      val v = v1 / v2
      result = result.updated(v, result.getOrElse(v, 0.0) + p1 * p2)
    }
    result
  }

  private[backend] def scaleDistribution(d: Distribution, factor: Double): Distribution =
    d.view.mapValues(_ * factor).toMap

  private[backend] def mergeDistributions(d1: Distribution, d2: Distribution): Distribution = {
    val keys = d1.keySet ++ d2.keySet
    keys.map(k => k -> (d1.getOrElse(k, 0.0) + d2.getOrElse(k, 0.0))).toMap
  }
}
