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
    case Sum(inner) =>
      val (d, tyInner) = evalSum(inner)
      (d, TySum(tyInner, classify(d)))
    case Prod(inner) =>
      val (d, tyInner) = evalProd(inner)
      (d, TyProd(tyInner, classify(d)))
    case other =>
      throw new IllegalArgumentException(s"Interpreter expects root to be Sum or Prod, got: $other")
  }

  private def evalSum(expr: Expr): (Distribution, TyExpr) = expr match {
    case IntLiteral(n)     =>
      val d = Map(n -> 1.0)
      (d, TyIntLiteral(n, classify(d)))
    case Dice(c, s)        =>
      val (dC, tyC) = evalSum(c)
      val (dS, tyS) = evalSum(s)
      val d = combineDiceSum(dC, dS)
      (d, TyDice(tyC, tyS, classify(d)))
    case Sum(inner)        =>
      val (d, tyInner) = evalSum(inner)
      (d, TySum(tyInner, classify(d)))
    case Prod(inner)       =>
      val (d, tyInner) = evalProd(inner)
      (d, TyProd(tyInner, classify(d)))
    case Add(l, r)         =>
      val (dL, tyL) = evalSum(l)
      val (dR, tyR) = evalSum(r)
      val d = convolve(dL, dR, _ + _)
      (d, TyAdd(tyL, tyR, classify(d)))
    case Sub(l, r)         =>
      val (dL, tyL) = evalSum(l)
      val (dR, tyR) = evalSum(r)
      val d = convolveSub(dL, dR)
      (d, TySub(tyL, tyR, classify(d)))
    case Mul(l, r)         =>
      val (dL, tyL) = evalSum(l)
      val (dR, tyR) = evalSum(r)
      val d = convolve(dL, dR, _ * _)
      (d, TyMul(tyL, tyR, classify(d)))
    case Div(l, r)         =>
      val (dL, tyL) = evalSum(l)
      val (dR, tyR) = evalSum(r)
      val d = convolveDiv(dL, dR)
      (d, TyDiv(tyL, tyR, classify(d)))
  }

  private def evalProd(expr: Expr): (Distribution, TyExpr) = expr match {
    case IntLiteral(n)     =>
      val d = Map(n -> 1.0)
      (d, TyIntLiteral(n, classify(d)))
    case Dice(c, s)        =>
      val (dC, tyC) = evalProd(c)
      val (dS, tyS) = evalProd(s)
      val d = combineDiceProd(dC, dS)
      (d, TyDice(tyC, tyS, classify(d)))
    case Sum(inner)        =>
      val (d, tyInner) = evalSum(inner)
      (d, TySum(tyInner, classify(d)))
    case Prod(inner)       =>
      val (d, tyInner) = evalProd(inner)
      (d, TyProd(tyInner, classify(d)))
    case Add(l, r)         =>
      val (dL, tyL) = evalProd(l)
      val (dR, tyR) = evalProd(r)
      val d = convolve(dL, dR, _ + _)
      (d, TyAdd(tyL, tyR, classify(d)))
    case Sub(l, r)         =>
      val (dL, tyL) = evalProd(l)
      val (dR, tyR) = evalProd(r)
      val d = convolveSub(dL, dR)
      (d, TySub(tyL, tyR, classify(d)))
    case Mul(l, r)         =>
      val (dL, tyL) = evalProd(l)
      val (dR, tyR) = evalProd(r)
      val d = convolve(dL, dR, _ * _)
      (d, TyMul(tyL, tyR, classify(d)))
    case Div(l, r)         =>
      val (dL, tyL) = evalProd(l)
      val (dR, tyR) = evalProd(r)
      val d = convolveDiv(dL, dR)
      (d, TyDiv(tyL, tyR, classify(d)))
  }

  /** For each (c, s) with prob p_c * p_s, add the distribution of c dice with s sides (sum). */
  private[backend] def combineDiceSum(countDist: Distribution, sidesDist: Distribution): Distribution = {
    var result: Distribution = Map.empty
    for ((c, pC) <- countDist; (s, pS) <- sidesDist) {
      val p = pC * pS
      val d = diceSumDistribution(safeCount(c), safeSides(s))
      result = mergeDistributions(result, scaleDistribution(d, p))
    }
    result
  }

  /** For each (c, s) with prob p_c * p_s, add the distribution of product of c dice with s sides. */
  private[backend] def combineDiceProd(countDist: Distribution, sidesDist: Distribution): Distribution = {
    var result: Distribution = Map.empty
    for ((c, pC) <- countDist; (s, pS) <- sidesDist) {
      val p = pC * pS
      val d = diceProductDistribution(safeCount(c), safeSides(s))
      result = mergeDistributions(result, scaleDistribution(d, p))
    }
    result
  }

  private[backend] def safeCount(c: Int): Int = if (c < 0) 0 else c
  private[backend] def safeSides(s: Int): Int = if (s < 1) 1 else s

  /** Distribution of the sum of `count` dice each with faces 1..sides (uniform). */
  private[backend] def diceSumDistribution(count: Int, sides: Int): Distribution = {
    if (count <= 0) return Map(0 -> 1.0)
    val oneDie = (1 to sides).map(_ -> (1.0 / sides)).toMap
    (1 until count).foldLeft(oneDie) { (acc, _) => convolve(acc, oneDie, _ + _) }
  }

  /** Distribution of the product of `count` dice each with faces 1..sides (uniform). */
  private[backend] def diceProductDistribution(count: Int, sides: Int): Distribution = {
    if (count <= 0) return Map(1 -> 1.0)
    val oneDie = (1 to sides).map(_ -> (1.0 / sides)).toMap
    (1 until count).foldLeft(oneDie) { (acc, _) => convolve(acc, oneDie, _ * _) }
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
