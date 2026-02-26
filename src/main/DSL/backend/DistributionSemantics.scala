package DSL.backend

import DSL.backend.typedAST._

trait DistributionSemantics {
  def scalar(n: Int): Distribution
  def custom(raw: Map[Int, Double]): Distribution

  def add(d1: Distribution, d2: Distribution): Distribution
  def sub(d1: Distribution, d2: Distribution): Distribution
  def mul(d1: Distribution, d2: Distribution): Distribution
  def div(d1: Distribution, d2: Distribution): Distribution

  def dice(count: Distribution, sides: Distribution, mode: DiceMode): Distribution
}

object DefaultDistributionSemantics extends DistributionSemantics {
  override def scalar(n: Int): Distribution =
    MathOps.scalar(n)

  override def custom(raw: Map[Int, Double]): Distribution = {
    val total = raw.values.sum
    if (total == 0) Map(0 -> 1.0) else raw.view.mapValues(_ / total).toMap
  }

  override def add(d1: Distribution, d2: Distribution): Distribution =
    SmartConstructors.add(d1, d2)

  override def sub(d1: Distribution, d2: Distribution): Distribution =
    MathOps.convolve(d1, d2, _ - _)

  override def mul(d1: Distribution, d2: Distribution): Distribution =
    MathOps.convolve(d1, d2, _ * _)

  override def div(d1: Distribution, d2: Distribution): Distribution =
    MathOps.convolveDiv(d1, d2)

  override def dice(count: Distribution, sides: Distribution, mode: DiceMode): Distribution =
    SmartConstructors.dice(count, sides, mode)
}

