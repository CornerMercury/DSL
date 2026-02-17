package DSL.backend

import DSL.frontend.AST._
import semanticTypes._
import typedAST._

type Distribution = Map[Int, Double]

object interpreter {

  enum DiceMode { case Sum, Prod }

  def interpret(expr: Expr): Distribution = expr match {
    case Sum(inner)  => eval(inner, DiceMode.Sum)._1
    case Prod(inner) => eval(inner, DiceMode.Prod)._1
    case other       => throw new IllegalArgumentException(s"Root must be Sum or Prod. Got: $other")
  }

  private def eval(expr: Expr, mode: DiceMode): (Distribution, TyExpr) = expr match {
    case IntLiteral(n) =>
      val d = MathOps.scalar(n)
      (d, TyIntLiteral(n, classify(d)))

    case Sum(inner) =>
      val (d, t) = eval(inner, DiceMode.Sum)
      (d, TyUnary(UnaryOp.Sum, t, classify(d)))

    case Prod(inner) =>
      val (d, t) = eval(inner, DiceMode.Prod)
      (d, TyUnary(UnaryOp.Prod, t, classify(d)))

    case Dice(c, s) =>
      val (dC, tC) = eval(c, mode)
      val (dS, tS) = eval(s, mode)
      val d = SmartConstructors.dice(dC, dS, mode)
      (d, TyBinary(BinaryOp.Dice, tC, tS, classify(d)))

    case Add(l, r) =>
      val (dL, tL) = eval(l, mode)
      val (dR, tR) = eval(r, mode)
      val d = SmartConstructors.add(dL, dR)
      (d, TyBinary(BinaryOp.Add, tL, tR, classify(d)))

    case Sub(l, r) =>
      val ((dL, tL), (dR, tR)) = (eval(l, mode), eval(r, mode))
      val d = MathOps.convolve(dL, dR, _ - _)
      (d, TyBinary(BinaryOp.Sub, tL, tR, classify(d)))

    case Mul(l, r) =>
      val ((dL, tL), (dR, tR)) = (eval(l, mode), eval(r, mode))
      val d = MathOps.convolve(dL, dR, _ * _)
      (d, TyBinary(BinaryOp.Mul, tL, tR, classify(d)))

    case Div(l, r) =>
      val ((dL, tL), (dR, tR)) = (eval(l, mode), eval(r, mode))
      val d = MathOps.convolveDiv(dL, dR)
      (d, TyBinary(BinaryOp.Div, tL, tR, classify(d)))
  }
}

object SmartConstructors {
  import interpreter.DiceMode

  def add(d1: Distribution, d2: Distribution): Distribution = {
    val t1 = classify(d1)
    val t2 = classify(d2)

    (t1, t2) match {
      // Optimization: Summing two compatible Bernoulli trials
      // We extract 'p' directly from the Type! No map lookups needed here.
      case (BernoulliTy(p1), BernoulliTy(p2)) if Math.abs(p1 - p2) < 1e-9 =>
         MathOps.fastBinomial(2, p1)
      case _ => 
         MathOps.convolve(d1, d2, _ + _)
    }
  }

  def dice(countDist: Distribution, sidesDist: Distribution, mode: DiceMode): Distribution = {
    // Optimization: N (Scalar) d Bernoulli(p) -> Binomial(N, p)
    if (mode == DiceMode.Sum) {
      val tCount = classify(countDist)
      val tSides = classify(sidesDist)

      (tCount, tSides) match {
        case (ScalarTy, BernoulliTy(p)) =>
           val n = countDist.keys.head
           return MathOps.fastBinomial(n, p)
        case _ =>
      }
    }
    
    // Slow Path
    val combineFunc = mode match {
      case DiceMode.Sum  => MathOps.diceSumDistribution
      case DiceMode.Prod => MathOps.diceProductDistribution
    }
    MathOps.combineDice(countDist, sidesDist, combineFunc)
  }
}

object MathOps {
  
  def scalar(n: Int): Distribution = Map(n -> 1.0)

  // O(N) Recurrence
  def fastBinomial(N: Int, p: Double): Distribution = {
    if (N <= 0) return Map(0 -> 1.0)
    if (p <= 0.0) return Map(0 -> 1.0)
    if (p >= 1.0) return Map(N -> 1.0)

    val q = 1.0 - p
    val dist = scala.collection.mutable.Map[Int, Double]()
    var currentProb = math.pow(q, N) 
    dist(0) = currentProb

    for (k <- 1 to N) {
      val numerator = p * (N - k + 1)
      val denominator = q * k
      currentProb = currentProb * (numerator / denominator)
      dist(k) = currentProb
    }
    dist.toMap
  }

  def convolve(d1: Distribution, d2: Distribution, f: (Int, Int) => Int): Distribution = {
    var result: Distribution = Map.empty
    for ((v1, p1) <- d1; (v2, p2) <- d2) {
      val v = f(v1, v2)
      result = result.updated(v, result.getOrElse(v, 0.0) + p1 * p2)
    }
    result
  }

  def convolveDiv(d1: Distribution, d2: Distribution): Distribution = {
    var result: Distribution = Map.empty
    for ((v1, p1) <- d1; (v2, p2) <- d2 if v2 != 0) {
      val v = v1 / v2
      result = result.updated(v, result.getOrElse(v, 0.0) + p1 * p2)
    }
    result
  }

  def combineDice(cDist: Distribution, sDist: Distribution, diceFn: (Int, Int) => Distribution): Distribution = {
    var result: Distribution = Map.empty
    for ((c, pC) <- cDist; (s, pS) <- sDist) {
      val d = diceFn(if (c < 0) 0 else c, if (s < 1) 1 else s)
      for ((valD, pD) <- d) {
        result = result.updated(valD, result.getOrElse(valD, 0.0) + pD * pC * pS)
      }
    }
    result
  }

  def diceSumDistribution(count: Int, sides: Int): Distribution =
    iterativeDice(count, sides, 0, _ + _)

  def diceProductDistribution(count: Int, sides: Int): Distribution =
    iterativeDice(count, sides, 1, _ * _)

  private def iterativeDice(count: Int, sides: Int, zero: Int, op: (Int, Int) => Int): Distribution = {
    if (count <= 0) return Map(zero -> 1.0)
    val oneDie = (1 to sides).map(i => i -> 1.0/sides).toMap
    (1 until count).foldLeft(oneDie)((acc, _) => convolve(acc, oneDie, op))
  }
}