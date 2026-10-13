package cpgc.search

import cpgc.constraints.Topology
import cpgc.domain.{CompoundDesign, GearSet, Ratio}

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

  /** Canonical key for deduplication: topology, both stages' teeth and both
    * planet counts (N1, N2; 0 for an absent stage 2). */
  def canonicalKey(d: CompoundDesign): (Topology, Long) =
    (d.topology, dedupKey(d.stage1.set, d.stage2.map(_.set).orNull))

  /** The one deduplication rule: two designs of a topology are the same iff both stages' sun and
    * planet teeth (the ring follows) and both planet counts agree. `s2 == null` for a single
    * stage. Packed into a primitive long (12 bits per tooth count, 8 per planet count) so the
    * engine's hot path need not allocate; out-of-range values use a 64-bit hash with bit 63 set
    * (a collision there is astronomically unlikely). */
  def dedupKey(s1: GearSet, s2: GearSet): Long =
    val su1 = s1.sun.toInt; val p1 = s1.planet.toInt; val n1 = s1.numPlanets
    val su2 = if s2 == null then 0 else s2.sun.toInt
    val p2  = if s2 == null then 0 else s2.planet.toInt
    val n2  = if s2 == null then 0 else s2.numPlanets
    if ((su1 | p1 | su2 | p2) >>> 12) == 0 && ((n1 | n2) >>> 8) == 0 then
      (((((su1.toLong << 12 | p1) << 12 | su2) << 12 | p2) << 8 | n1) << 8) | n2
    else
      var h = 0x9E3779B97F4A7C15L
      for v <- Array(su1, p1, su2, p2, n1, n2) do
        h = (h ^ v.toLong) * 0xBF58476D1CE4E5B9L
        h ^= h >>> 29
      h | Long.MinValue

  /** Sort an iterable of designs by the chosen criterion and deduplicate by
    * `canonicalKey`. Returns a materialised Vector. */
  def sort(
      designs: Iterable[CompoundDesign], by: SortBy, targets: Seq[Ratio]
  ): Vector[CompoundDesign] =
    val primary: CompoundDesign => Double = by match
      case SortBy.BestFit    =>
        if targets.isEmpty then d => math.abs(d.ratio.value) else d => bestTargetError(targets, d.ratio)
      case SortBy.Ratio      => d => math.abs(d.ratio.value)
      case SortBy.Efficiency => d => -d.efficiency
      case SortBy.Simplicity => d => toothCount(d).toDouble
    val secondary = tieBreak(by, targets)
    val ordering: Ordering[CompoundDesign] =
      Ordering.by[CompoundDesign, (Double, Double)](d => (primary(d), secondary(d)))(
        using Ordering.Tuple2(Ordering.Double.TotalOrdering, Ordering.Double.TotalOrdering))
    designs.iterator.toVector.distinctBy(canonicalKey).sorted(using ordering)

  /** Secondary sort key, used by every sort mode so equal primary scores rank deterministically
    * (`SearchEngine`'s top-K applies the same key): best-fit error when there are targets (and the
    * mode is not already best-fit), else `|ratio|`. Remaining ties keep enumeration order. */
  def tieBreak(by: SortBy, targets: Seq[Ratio]): CompoundDesign => Double =
    if targets.nonEmpty && by != SortBy.BestFit then d => bestTargetError(targets, d.ratio)
    else d => math.abs(d.ratio.value)

  /** Keep the K best designs per target across an arbitrarily large stream
    * (smallest absolute error first; ties keep the earlier design). */
  def topPerTarget(
      designs: Iterable[CompoundDesign],
      targets: Seq[Ratio],
      k: Int
  ): Map[Ratio, Vector[Scored]] =
    if targets.isEmpty || k <= 0 then Map.empty
    else
      val accs = targets.distinct.map(t => t -> new TopK[Scored](k))
      val ids  = scala.collection.mutable.HashMap.empty[Any, Long]
      designs.foreach { d =>
        val key = ids.getOrElseUpdate(canonicalKey(d), ids.size.toLong)
        for (t, acc) <- accs do
          val e = absoluteError(t, d.ratio)
          acc.offer(e, math.abs(d.ratio.value), key, Scored(t, e, d))
      }
      accs.map((t, acc) => t -> acc.result).toMap
