package cpgc.search

import cpgc.domain.{CompoundDesign, Ratio}

import scala.collection.mutable

/** Ranks designs by absolute error to a set of target ratios. Pure interface,
  * mutable bounded heap inside for performance.
  */
object Scorer:

  final case class Scored(target: Ratio, error: Double, design: CompoundDesign)

  def absoluteError(target: Ratio, actual: Ratio): Double =
    math.abs(math.abs(target.value) - math.abs(actual.value))

  /** Keep top-K designs per target across an arbitrarily large stream. */
  def topPerTarget(
      designs: LazyList[CompoundDesign],
      targets: Seq[Ratio],
      k: Int
  ): Map[Ratio, Vector[Scored]] =
    if targets.isEmpty || k <= 0 then Map.empty
    else
      given Ordering[Scored] = Ordering.by[Scored, Double](_.error).reverse
      val heaps = targets.map(t => t -> mutable.PriorityQueue.empty[Scored]).toMap
      designs.foreach { d =>
        targets.foreach { t =>
          val s = Scored(t, absoluteError(t, d.ratio), d)
          val h = heaps(t)
          if h.size < k then h.enqueue(s)
          else if h.head.error > s.error then
            h.dequeue()
            h.enqueue(s)
        }
      }
      heaps.view
        .mapValues(_.dequeueAll.toVector.sortBy(_.error))
        .toMap

  /** Rank a fully materialized list (used when no targets supplied). */
  def rankByRatio(designs: Iterable[CompoundDesign]): Vector[CompoundDesign] =
    designs.toVector.sortBy(_.ratio.value)
