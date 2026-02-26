package DSL.backend

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
    val oneDie = (1 to sides).map(i => i -> 1.0 / sides).toMap
    (1 until count).foldLeft(oneDie)((acc, _) => convolve(acc, oneDie, op))
  }
}

