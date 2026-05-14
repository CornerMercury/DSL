package DSL.backend

import scala.collection.mutable

object MathOps {

  type Distribution = Map[Int, Double]

  def scalar(n: Int): Distribution = Map(n -> 1.0)

  // --- Main Dispatcher ---
  def keepLargest(k: Int, n: Int, dieDist: Distribution): Distribution = {
    if (k <= 0) return Map(0 -> 1.0)
    if (n <= 0) return Map(0 -> 1.0)
    
    // OPTIMIZATION: If K=1, use the O(F) CDF method
    // This is mathematically optimal and independent of N
    if (k == 1) return keepHighestOne(n, dieDist)
    
    // Optimization: If we keep everything, just sum the dice
    if (k >= n) {
      return (1 until n).foldLeft(dieDist)((acc, _) => convolve(acc, dieDist, _ + _))
    }

    // Default to the robust DP method for K > 1
    keepLargestDP(k, n, dieDist)
  }

  // --- Optimized: Keep Highest 1 (CDF Method) ---
  // Time Complexity: O(F log F) where F is the number of faces.
  // This is effectively O(1) with respect to N (number of dice).
  private def keepHighestOne(n: Int, dieDist: Distribution): Distribution = {
    if (n == 0) return Map(0 -> 1.0)

    // Sort faces so we can calculate cumulative probabilities
    val faces = dieDist.keys.toSeq.sorted
    val result = mutable.Map[Int, Double]()
    
    var cumProb = 0.0 // P(die <= current_face)
    
    for (face <- faces) {
      val pFace = dieDist(face)
      cumProb += pFace
      
      // P(One Die < current_face) = P(One Die <= current_face) - P(current_face)
      val prevCumProb = cumProb - pFace
      
      // P(Max == face) = P(All <= face)^N - P(All < face)^N
      val pMax = math.pow(cumProb, n) - math.pow(prevCumProb, n)
      
      if (pMax > 0.0) {
        result(face) = pMax
      }
    }
    result.toMap
  }

  // --- Optimized: Keep Lowest 1 (Survival Function Method) ---
  private def keepLowestOne(n: Int, dieDist: Distribution): Distribution = {
    if (n == 0) return Map(0 -> 1.0)

    // Sort descending for Lowest logic
    val faces = dieDist.keys.toSeq.sorted(Ordering[Int].reverse)
    val result = mutable.Map[Int, Double]()
    
    var cumProb = 0.0 // P(die >= current_face)
    
    for (face <- faces) {
      val pFace = dieDist(face)
      cumProb += pFace
      
      // P(One Die > current_face)
      val prevCumProb = cumProb - pFace
      
      // P(Min == face) = P(All >= face)^N - P(All > face)^N
      val pMin = math.pow(cumProb, n) - math.pow(prevCumProb, n)
      
      if (pMin > 0.0) {
        result(face) = pMin
      }
    }
    result.toMap
  }

  // --- Method: Keep Largest (Double Buffered DP with Conditional Probabilities) ---
  // Best for: K > 1
  private def keepLargestDP(k: Int, n: Int, dieDist: Distribution): Distribution = {
    // Sort faces from Highest to Lowest
    val faces = dieDist.keys.toSeq.sorted(Ordering[Int].reverse)
    
    // Precompute cumulative probabilities from the bottom up.
    // cum(i) = probability of a die being face 'i' OR LOWER.
    val cumProbs = new Array[Double](faces.length + 1)
    cumProbs(faces.length) = 0.0
    for (i <- (0 until faces.length).reverse) {
      cumProbs(i) = cumProbs(i + 1) + dieDist(faces(i))
    }

    // DP State: dp[rem][kept] = LongMap[Sum -> Prob]
    var dp = Array.fill(n + 1)(Array.fill(k + 1)(mutable.LongMap[Double]()))
    dp(n)(0)(0L) = 1.0

    // Iterate over each face
    for (i <- faces.indices) {
      val face = faces(i)
      val pFace = dieDist(face)
      val totalRemainingProb = cumProbs(i)

      if (totalRemainingProb > 0.0) {
        // Conditional Probability: P(face | face is <= current face)
        val pCond = pFace / totalRemainingProb

        // Precompute Binomial PMF for all possible rem values for this pCond
        val binomials = (0 to n).map(rem => computeBinomialPMF(rem, pCond, rem)).toArray

        // Double Buffering: Create fresh state for next step
        val nextDp = Array.fill(n + 1)(Array.fill(k + 1)(mutable.LongMap[Double]()))

        for (rem <- 0 to n) {
          for (r <- 0 to k) {
            val currentMap = dp(rem)(r)
            if (currentMap.nonEmpty) {
              val binomDist = binomials(rem)
              
              binomDist.foreach { case (c, pC) =>
                if (pC > 0) {
                  val newRem = rem - c
                  val take = math.min(c, k - r)
                  val newR = r + take
                  val deltaSum = take.toLong * face
                  
                  val targetMap = nextDp(newRem)(newR)
                  
                  // Merge probabilities
                  currentMap.foreach { case (sumKey, probState) =>
                    targetMap(sumKey + deltaSum) = targetMap.getOrElse(sumKey + deltaSum, 0.0) + probState * pC
                  }
                }
              }
            }
          }
        }
        dp = nextDp
      }
    }
    
    // Extract Result
    dp(0)(k).map { case (l, d) => l.toInt -> d }.toMap
  }

  // --- Method: Keep Smallest (DP) ---
  private def keepSmallestDP(k: Int, n: Int, dieDist: Distribution): Distribution = {
    // Sort faces from Lowest to Highest
    val faces = dieDist.keys.toSeq.sorted
    
    // Precompute cumulative probabilities from the top down (prob of face >= current)
    val cumProbs = new Array[Double](faces.length + 1)
    cumProbs(faces.length) = 0.0
    for (i <- (0 until faces.length).reverse) {
      cumProbs(i) = cumProbs(i + 1) + dieDist(faces(i))
    }

    var dp = Array.fill(n + 1)(Array.fill(k + 1)(mutable.LongMap[Double]()))
    dp(n)(0)(0L) = 1.0

    for (i <- faces.indices) {
      val face = faces(i)
      val pFace = dieDist(face)
      val totalRemainingProb = cumProbs(i)

      if (totalRemainingProb > 0.0) {
        val pCond = pFace / totalRemainingProb
        val binomials = (0 to n).map(rem => computeBinomialPMF(rem, pCond, rem)).toArray
        val nextDp = Array.fill(n + 1)(Array.fill(k + 1)(mutable.LongMap[Double]()))

        for (rem <- 0 to n) {
          for (r <- 0 to k) {
            val currentMap = dp(rem)(r)
            if (currentMap.nonEmpty) {
              val binomDist = binomials(rem)
              binomDist.foreach { case (c, pC) =>
                if (pC > 0) {
                  val newRem = rem - c
                  val take = math.min(c, k - r)
                  val newR = r + take
                  val deltaSum = take.toLong * face
                  
                  val targetMap = nextDp(newRem)(newR)
                  currentMap.foreach { case (sumKey, probState) =>
                    targetMap(sumKey + deltaSum) = targetMap.getOrElse(sumKey + deltaSum, 0.0) + probState * pC
                  }
                }
              }
            }
          }
        }
        dp = nextDp
      }
    }
    
    dp(0)(k).map { case (l, d) => l.toInt -> d }.toMap
  }

  // Helper: Compute Binomial PMF
  private def computeBinomialPMF(n: Int, p: Double, limit: Int): Map[Int, Double] = {
    if (n == 0) return Map(0 -> 1.0)
    if (p == 0.0) return Map(0 -> 1.0)
    if (p == 1.0) return Map(n -> 1.0)

    val m = mutable.Map[Int, Double]()
    var currentProb = math.pow(1.0 - p, n) 
    m(0) = currentProb
    
    val q = 1.0 - p
    val kMax = math.min(limit, n)
    
    for (k <- 1 to kMax) {
      val num = p * (n - k + 1).toDouble
      val den = q * k.toDouble
      currentProb = currentProb * (num / den)
      m(k) = currentProb
    }
    m.toMap
  }

  // --- Existing Math Ops ---
  
  def fastBinomial(N: Int, p: Double): Distribution = {
    if (N <= 0) return Map(0 -> 1.0)
    if (p <= 0.0) return Map(0 -> 1.0)
    if (p >= 1.0) return Map(N -> 1.0)

    val q = 1.0 - p
    val dist = mutable.Map[Int, Double]()
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
      val d = diceFn(if (c < 0) 0 else c, if (s < 0) 0 else s)
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
    if (count <= 0 || sides <= 0) return Map(zero -> 1.0)

    val oneDie = (1 to sides).map(i => i -> 1.0 / sides).toMap
    (1 until count).foldLeft(oneDie)((acc, _) => convolve(acc, oneDie, op))
  }

  def scale(d: Distribution, factor: Double): Distribution =
    d.view.mapValues(_ * factor).toMap

  def merge(d1: Distribution, d2: Distribution): Distribution = {
    d2.foldLeft(d1) { case (acc, (v, p)) =>
      acc.updated(v, acc.getOrElse(v, 0.0) + p)
    }
  }
}