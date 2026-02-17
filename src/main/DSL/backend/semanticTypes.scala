package DSL.backend

/** Distribution kind for self-optimizing interpretation. Preference order: Scalar > Binomial > Uniform > Generic. */
sealed trait DistTy

/** Single outcome with probability 1.0 (e.g. (x: 1.0)). Most preferred. */
case object ScalarTy extends DistTy

/** Exactly two outcomes (e.g. coin flip, 1d2). */
case object BinomialTy extends DistTy

/** Any number of outcomes, all with equal probability (e.g. 1d6). */
case object UniformTy extends DistTy

/** Arbitrary discrete distribution. Default/fallback. */
case object GenericDistTy extends DistTy

object semanticTypes {

  /** Prefer Scalar > Binomial > Uniform > Generic. Classify a computed distribution by its shape. */
  def classify(dist: Map[Int, Double]): DistTy = {
    if (dist.size == 1) ScalarTy
    else if (dist.size == 2) BinomialTy
    else if (dist.isEmpty) GenericDistTy
    else {
      val probs = dist.values.toSeq
      val p0 = probs.head
      if (probs.forall(p => math.abs(p - p0) < 1e-12)) UniformTy
      else GenericDistTy
    }
  }

  /** Preference order for merging: Scalar (highest) > Binomial > Uniform > Generic (lowest). */
  def prefer(left: DistTy, right: DistTy): DistTy = (left, right) match {
    case (ScalarTy, _) | (_, ScalarTy)       => ScalarTy
    case (BinomialTy, _) | (_, BinomialTy)   => BinomialTy
    case (UniformTy, _) | (_, UniformTy)     => UniformTy
    case _                                    => GenericDistTy
  }
}
