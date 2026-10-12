package cpgc.search

import cpgc.domain.{CompoundDesign, Ratio}

import scala.collection.mutable

/** Ranks designs by absolute error to a set of target ratios. Pure interface,
  * mutable bounded heap inside for performance.
  */
object Scorer:

  final case class Scored(target: Ratio, error: Double, design: CompoundDesign)

  enum SortBy:
    case BestFit, Ratio, Efficiency, Simplicity

  object SortBy:
    def parse(s: String): Option[SortBy] = s.trim.toLowerCase match
      case "best" | "bestfit" | "best-fit" => Some(BestFit)
      case "ratio"                          => Some(Ratio)
      case "efficiency" | "eff"             => Some(Efficiency)
      case "simplicity" | "simple"          => Some(Simplicity)
      case _                                => None

  def absoluteError(target: Ratio, actual: Ratio): Double =
    math.abs(math.abs(target.value) - math.abs(actual.value))

  /** Aggregate error across multiple targets — minimum distance to any target. */
  def bestTargetError(targets: Seq[Ratio], actual: Ratio): Double =
    if targets.isEmpty then 0.0
    else targets.iterator.map(absoluteError(_, actual)).min

  /** Total tooth count of a design — a proxy for "simplicity" / package size. */
  def toothCount(d: CompoundDesign): Int =
    val s1 = d.stage1.set
    val s2 = d.stage2.fold(0)(s => s.set.sun.toInt + s.set.planet.toInt + s.set.ring.toInt)
    s1.sun.toInt + s1.planet.toInt + s1.ring.toInt + s2

  /** Canonical key for deduplication: tooth counts across both stages + mode. */
  def canonicalKey(d: CompoundDesign): (String, Int, Int, Int, Int, Int, Int) =
    val s1 = d.stage1.set
    val (s2s, s2p, s2r) = d.stage2 match
      case Some(st) => (st.set.sun.toInt, st.set.planet.toInt, st.set.ring.toInt)
      case None     => (0, 0, 0)
    (d.mode.toString, s1.sun.toInt, s1.planet.toInt, s1.ring.toInt, s2s, s2p, s2r)

  /** Sort an iterable of designs by the chosen criterion and deduplicate by
    * `canonicalKey`. Returns a materialised Vector. */
  def sort(
      designs: Iterable[CompoundDesign], by: SortBy, targets: Seq[Ratio]
  ): Vector[CompoundDesign] =
    val ordering: Ordering[CompoundDesign] = by match
      case SortBy.BestFit    => Ordering.by(d => bestTargetError(targets, d.ratio))
      case SortBy.Ratio      => Ordering.by(d => math.abs(d.ratio.value))
      case SortBy.Efficiency => Ordering.by[CompoundDesign, Double](_.efficiency).reverse
      case SortBy.Simplicity => Ordering.by(toothCount)
    designs.iterator.toVector.distinctBy(canonicalKey).sorted(using ordering)

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
