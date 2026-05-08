package DSL.backend

import DSL.backend.typedAST._

trait DistributionSemantics {
  def scalar(n: Int): Distribution
  def custom(raw: Map[Int, Double]): Distribution

  def add(d1: Distribution, d2: Distribution): Distribution
  def sub(d1: Distribution, d2: Distribution): Distribution
  def mul(d1: Distribution, d2: Distribution): Distribution
  def div(d1: Distribution, d2: Distribution): Distribution

  def eq(d1: Distribution, d2: Distribution): Distribution
  def lt(d1: Distribution, d2: Distribution): Distribution
  def le(d1: Distribution, d2: Distribution): Distribution
  def gt(d1: Distribution, d2: Distribution): Distribution
  def ge(d1: Distribution, d2: Distribution): Distribution

  def dice(count: Distribution, sides: Distribution, mode: DiceMode): Distribution

  def max(d: Distribution): Distribution
  def min(d: Distribution): Distribution
}

object DefaultDistributionSemantics extends DistributionSemantics {

  override def scalar(n: Int): Distribution =
    MathOps.scalar(n)

  override def custom(raw: Map[Int, Double]): Distribution = {
    val total = raw.values.sum
    if (total == 0) Map(0 -> 1.0)
    else raw.view.mapValues(_ / total).toMap
  }

  override def add(d1: Distribution, d2: Distribution): Distribution =
    SmartConstructors.add(d1, d2)

  override def sub(d1: Distribution, d2: Distribution): Distribution =
    MathOps.convolve(d1, d2, _ - _)

  override def mul(d1: Distribution, d2: Distribution): Distribution =
    MathOps.convolve(d1, d2, _ * _)

  override def div(d1: Distribution, d2: Distribution): Distribution =
    MathOps.convolveDiv(d1, d2)

  override def eq(d1: Distribution, d2: Distribution): Distribution =
    if (d1 == d2) MathOps.scalar(1)
    else MathOps.scalar(0)

  override def lt(d1: Distribution, d2: Distribution): Distribution =
    scalarCompare(d1, d2, _ < _)

  override def le(d1: Distribution, d2: Distribution): Distribution =
    scalarCompare(d1, d2, _ <= _)

  override def gt(d1: Distribution, d2: Distribution): Distribution =
    scalarCompare(d1, d2, _ > _)

  override def ge(d1: Distribution, d2: Distribution): Distribution =
    scalarCompare(d1, d2, _ >= _)

  private def scalarCompare(d1: Distribution, d2: Distribution, op: (Int, Int) => Boolean): Distribution = {
    val v1 = d1.keys.head
    val v2 = d2.keys.head
    if (op(v1, v2)) MathOps.scalar(1) else MathOps.scalar(0)
  }

  override def dice(
    count: Distribution,
    sides: Distribution,
    mode: DiceMode
  ): Distribution =
    SmartConstructors.dice(count, sides, mode)

  override def max(d: Distribution): Distribution =
    if (d.isEmpty) Map(0 -> 1.0)
    else Map(d.keys.max -> 1.0)

  override def min(d: Distribution): Distribution =
    if (d.isEmpty) Map(0 -> 1.0)
    else Map(d.keys.min -> 1.0)
}