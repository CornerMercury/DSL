package DSL.backend

import DSL.frontend.AST._

/** Probability distribution over integer outcomes. Probabilities sum to 1.0 (within rounding). */
type Distribution = Map[Int, Double]

object interpreter {

  /** Interpret only Sum or Prod at the root; rejects other top-level expressions. */
  def interpret(expr: Expr): Distribution = expr match {
    case Sum(inner) => evalSum(inner)
    case Prod(inner) => evalProd(inner)
    case other      => throw new IllegalArgumentException(s"Interpreter expects root to be Sum or Prod, got: $other")
  }

  private def evalSum(expr: Expr): Distribution = expr match {
    case IntLiteral(n)     => Map(n -> 1.0)
    case Dice(c, s)       => combineDiceSum(evalSum(c), evalSum(s))
    case Sum(inner)       => evalSum(inner)
    case Prod(inner)      => evalProd(inner)
    case Add(l, r)        => convolve(evalSum(l), evalSum(r), _ + _)
    case Sub(l, r)        => convolveSub(evalSum(l), evalSum(r))
    case Mul(l, r)        => convolve(evalSum(l), evalSum(r), _ * _)
    case Div(l, r)        => convolveDiv(evalSum(l), evalSum(r))
  }

  private def evalProd(expr: Expr): Distribution = expr match {
    case IntLiteral(n)     => Map(n -> 1.0)
    case Dice(c, s)       => combineDiceProd(evalProd(c), evalProd(s))
    case Sum(inner)       => evalSum(inner)
    case Prod(inner)      => evalProd(inner)
    case Add(l, r)        => convolve(evalProd(l), evalProd(r), _ + _)
    case Sub(l, r)        => convolveSub(evalProd(l), evalProd(r))
    case Mul(l, r)        => convolve(evalProd(l), evalProd(r), _ * _)
    case Div(l, r)        => convolveDiv(evalProd(l), evalProd(r))
  }

  /** For each (c, s) with prob p_c * p_s, add the distribution of c dice with s sides (sum). */
  private def combineDiceSum(countDist: Distribution, sidesDist: Distribution): Distribution = {
    var result: Distribution = Map.empty
    for ((c, pC) <- countDist; (s, pS) <- sidesDist) {
      val p = pC * pS
      val d = diceSumDistribution(safeCount(c), safeSides(s))
      result = mergeDistributions(result, scaleDistribution(d, p))
    }
    result
  }

  /** For each (c, s) with prob p_c * p_s, add the distribution of product of c dice with s sides. */
  private def combineDiceProd(countDist: Distribution, sidesDist: Distribution): Distribution = {
    var result: Distribution = Map.empty
    for ((c, pC) <- countDist; (s, pS) <- sidesDist) {
      val p = pC * pS
      val d = diceProductDistribution(safeCount(c), safeSides(s))
      result = mergeDistributions(result, scaleDistribution(d, p))
    }
    result
  }

  private def safeCount(c: Int): Int = if (c < 0) 0 else c
  private def safeSides(s: Int): Int = if (s < 1) 1 else s

  /** Distribution of the sum of `count` dice each with faces 1..sides (uniform). */
  private def diceSumDistribution(count: Int, sides: Int): Distribution = {
    if (count <= 0) return Map(0 -> 1.0)
    val oneDie = (1 to sides).map(_ -> (1.0 / sides)).toMap
    (1 until count).foldLeft(oneDie) { (acc, _) => convolve(acc, oneDie, _ + _) }
  }

  /** Distribution of the product of `count` dice each with faces 1..sides (uniform). */
  private def diceProductDistribution(count: Int, sides: Int): Distribution = {
    if (count <= 0) return Map(1 -> 1.0)
    val oneDie = (1 to sides).map(_ -> (1.0 / sides)).toMap
    (1 until count).foldLeft(oneDie) { (acc, _) => convolve(acc, oneDie, _ * _) }
  }

  /** Convolve two distributions with combination function f(v1, v2). */
  private def convolve(d1: Distribution, d2: Distribution, f: (Int, Int) => Int): Distribution = {
    var result: Distribution = Map.empty
    for ((v1, p1) <- d1; (v2, p2) <- d2) {
      val v = f(v1, v2)
      result = result.updated(v, result.getOrElse(v, 0.0) + p1 * p2)
    }
    result
  }

  private def convolveSub(d1: Distribution, d2: Distribution): Distribution =
    convolve(d1, d2, _ - _)

  private def convolveDiv(d1: Distribution, d2: Distribution): Distribution = {
    var result: Distribution = Map.empty
    for ((v1, p1) <- d1; (v2, p2) <- d2 if v2 != 0) {
      val v = v1 / v2
      result = result.updated(v, result.getOrElse(v, 0.0) + p1 * p2)
    }
    result
  }

  private def scaleDistribution(d: Distribution, factor: Double): Distribution =
    d.view.mapValues(_ * factor).toMap

  private def mergeDistributions(d1: Distribution, d2: Distribution): Distribution = {
    val keys = d1.keySet ++ d2.keySet
    keys.map(k => k -> (d1.getOrElse(k, 0.0) + d2.getOrElse(k, 0.0))).toMap
  }
}
