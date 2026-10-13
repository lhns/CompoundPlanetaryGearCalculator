package cpgc.search

import cpgc.constraints.SearchConfig
import cpgc.domain.{CompoundDesign, Ratio}
import cpgc.search.Scorer.{Scored, SortBy}

import scala.collection.mutable

/** Bounded top-K accumulator with deduplication (ADR 0017).
  *
  * Smaller score is better. Ties keep the earlier offer (a total order on
  * `(score, arrival)`), so the result does not depend on heap internals. A key
  * already held is ignored. A design is only materialised (`value` is by-name)
  * when it is actually kept. */
final class TopK[A](k: Int):
  private final class Entry(val score: Double, val seq: Long, val key: Any, val value: A)
  // PriorityQueue is a max-heap: `head` is the worst kept entry.
  private val worstFirst: Ordering[Entry] = (a, b) =>
    val c = java.lang.Double.compare(a.score, b.score)
    if c != 0 then c else java.lang.Long.compare(a.seq, b.seq)
  private val heap = mutable.PriorityQueue.empty[Entry](using worstFirst)
  private val keys = mutable.HashSet.empty[Any]
  private var arrivals = 0L

  def size: Int = heap.size

  /** Would an offer with this score be kept (key aside)? Cheap pre-check. */
  def accepts(score: Double): Boolean =
    k > 0 && !score.isNaN && (heap.size < k || score < heap.head.score)

  def offer(score: Double, key: Any, value: => A): Unit =
    if accepts(score) && !keys.contains(key) then
      arrivals += 1
      if heap.size >= k then keys -= heap.dequeue().key
      heap.enqueue(new Entry(score, arrivals, key, value))
      keys += key

  /** Kept values, best first. */
  def result: Vector[A] = heap.toVector.sorted(using worstFirst).map(_.value)

  /** Kept scores, best first (same order as `result`). */
  def scores: Vector[Double] = heap.toVector.sorted(using worstFirst).map(_.score)

/** The calculator's one search engine (ADR 0017): `PairSearch` candidates feed
  * bounded top-K accumulators.
  *
  * Ranking: `BestFit` ranks by the smallest `| |target| − |ratio| |` over the
  * targets (sign ignored), or by `|ratio|` when there are no targets; `Ratio`
  * by `|ratio|`; `Efficiency` by efficiency descending; `Simplicity` by total
  * teeth. With targets, a separate top-K per target is also kept (`perTarget`).
  * Designs are deduplicated by both stages' teeth, N1, N2 and the topology.
  *
  * Driving it in chunks (web): call `step(n)` until it returns true, reading
  * `top` / `perTarget` between calls; call `stop()` when giving up early so
  * `truncated` reports "best so far". Not thread-safe.
  */
final class SearchEngine(val cfg: SearchConfig, val sortBy: SortBy = SortBy.BestFit, val k: Int = 10):
  private val search  = new PairSearch(cfg)
  private val ev      = search.evaluator
  private val effic   = cpgc.physics.Efficiency.overall(cfg.topology)
  private val targets = cfg.targets.distinct.toVector
  private val tops    = targets.map(t => t -> new TopK[Scored](k))
  private val overall = new TopK[CompoundDesign](k)
  private var foundCount = 0L
  private var stopped = false

  /** Run up to `units` units of work; true once the whole space is consumed. */
  def step(units: Int): Boolean =
    var i = 0
    while i < units && !search.done do
      if search.advance() then offer()
      i += 1
    search.done

  /** Run to completion. */
  def run(): this.type =
    while !step(1 << 16) do ()
    this

  def done: Boolean = search.done

  /** Declare that the caller gives up before `done` (time cap). */
  def stop(): Unit = if !search.done then stopped = true

  /** True when `stop()` was called before the space was consumed: `top` is the best so far. */
  def truncated: Boolean = stopped && !search.done

  /** Candidates evaluated so far (pairs, or single stages). */
  def examined: Long = search.examined

  /** Candidates accepted so far (valid designs). */
  def found: Long = foundCount

  /** Best designs overall by `sortBy`, best first. */
  def top: Vector[CompoundDesign] = overall.result

  /** Best designs per target by absolute error, best first (empty without targets). */
  def perTarget: Map[Ratio, Vector[Scored]] = tops.map((t, acc) => t -> acc.result).toMap

  private def offer(): Unit =
    foundCount += 1
    val r   = ev.ratio
    val a   = math.abs(r)
    val s1  = search.s1
    val s2  = search.s2
    var minErr = Double.PositiveInfinity
    var ti = 0
    while ti < tops.length do
      val e = math.abs(math.abs(tops(ti)._1.value) - a)
      if e < minErr then minErr = e
      ti += 1
    val score = sortBy match
      case SortBy.BestFit    => if targets.isEmpty then a else minErr
      case SortBy.Ratio      => a
      case SortBy.Efficiency => -effic
      case SortBy.Simplicity =>
        (s1.sun.toInt + s1.planet.toInt + s1.ring.toInt +
          (if s2 == null then 0 else s2.sun.toInt + s2.planet.toInt + s2.ring.toInt)).toDouble

    var want = overall.accepts(score)
    ti = 0
    while !want && ti < tops.length do
      want = tops(ti)._2.accepts(math.abs(math.abs(tops(ti)._1.value) - a))
      ti += 1
    if want then
      val design = Compounder.design(cfg, s1, Option(s2), ev)
      val key    = Scorer.canonicalKey(design)
      overall.offer(score, key, design)
      for (t, acc) <- tops do acc.offer(Scorer.absoluteError(t, design.ratio), key, Scored(t, Scorer.absoluteError(t, design.ratio), design))
