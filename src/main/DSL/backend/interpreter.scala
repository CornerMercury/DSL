package DSL.backend

import DSL.frontend.AST._

/** Probability distribution over integer outcomes. Probabilities sum to 1.0 (within rounding). */
type Distribution = Map[Int, Double]

object interpreter {
  
  def interpret(expr: Expr): Distribution = eval(expr)

  private def eval(expr: Expr): Distribution = expr match {
    case IntLiteral(n) =>
      Map(n -> 1.0)

    case Dice(countExpr, sidesExpr) =>
      val countDist = eval(countExpr)
      val sidesDist = eval(sidesExpr)
      combineDice(countDist, sidesDist)

    case Sum(inner) =>
      eval(inner)

    case Prod(inner) =>
      // TODO: implement product-of-dice distribution (frontend parsing done first)
      eval(inner)

    case Add(left, right) =>
      convolve(eval(left), eval(right), _ + _)

    case Sub(left, right) =>
      convolveSub(eval(left), eval(right))

    case Mul(left, right) =>
      convolve(eval(left), eval(right), _ * _)

    case Div(left, right) =>
      convolveDiv(eval(left), eval(right))
  }

  /** For each (c, s) with prob p_c * p_s, add the distribution of c dice with s sides (sum). */
  private def combineDice(countDist: Distribution, sidesDist: Distribution): Distribution = {
    var result: Distribution = Map.empty
    for ((c, pC) <- countDist; (s, pS) <- sidesDist) {
      val p = pC * pS
      val d = diceSumDistribution(safeCount(c), safeSides(s))
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
