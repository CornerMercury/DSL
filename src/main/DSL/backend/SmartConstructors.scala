package DSL.backend

import DSL.backend.semanticTypes._

object SmartConstructors {

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

