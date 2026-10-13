package cpgc.search

import cpgc.constraints.SearchConfig
import cpgc.domain.{CompoundDesign, GearSet, Ratio}
import cpgc.search.Scorer.{Scored, SortBy}


/** Open-addressing set of longs with deletion (backward shift); no boxing. */
private final class LongSet:
  private var table = new Array[Long](64)
  private var used  = new Array[Boolean](64)
  private var count = 0

  private def slot(key: Long, mask: Int): Int =
    val h = key * 0x9E3779B97F4A7C15L
    ((h ^ (h >>> 32)).toInt) & mask

  def contains(key: Long): Boolean =
    val mask = table.length - 1
    var i = slot(key, mask)
    while used(i) do
      if table(i) == key then return true
      i = (i + 1) & mask
    false

  def add(key: Long): Unit =
    if (count + 1) * 2 > table.length then grow()
    val mask = table.length - 1
    var i = slot(key, mask)
    while used(i) do
      if table(i) == key then return
      i = (i + 1) & mask
    table(i) = key; used(i) = true; count += 1

  def remove(key: Long): Unit =
    val mask = table.length - 1
    var i = slot(key, mask)
    while used(i) && table(i) != key do i = (i + 1) & mask
    if !used(i) then return
    count -= 1
    var j = i
    var done = false
    while !done do
      j = (j + 1) & mask
      if !used(j) then done = true
      else
        val h = slot(table(j), mask)
        // move j into the hole i unless its home slot lies cyclically in (i, j]
        val stays = if i <= j then i < h && h <= j else i < h || h <= j
        if !stays then { table(i) = table(j); i = j }
    used(i) = false

  private def grow(): Unit =
    val (ot, ou) = (table, used)
    table = new Array[Long](ot.length * 2); used = new Array[Boolean](ot.length * 2); count = 0
    var i = 0
    while i < ot.length do { if ou(i) then add(ot(i)); i += 1 }

/** Bounded top-K accumulator with deduplication (ADR 0017).
  *
  * Smaller score is better. Ties keep the earlier offer (a total order on
  * `(score, arrival)`), so the result does not depend on heap internals. A key
  * already held is ignored. Keys are primitive longs; entries are meant to be
  * lightweight candidates, materialised by the reader. */
final class TopK[A <: AnyRef](k: Int):
  private final class Entry(val score: Double, val score2: Double, val seq: Long, val key: Long, val value: A)
  private def worse(a: Entry, b: Entry): Boolean =
    val c = java.lang.Double.compare(a.score, b.score)
    if c != 0 then c > 0
    else
      val c2 = java.lang.Double.compare(a.score2, b.score2)
      if c2 != 0 then c2 > 0 else a.seq > b.seq
  // Binary max-heap: heap(0) is the worst kept entry.
  private var heap = new Array[Entry](math.max(1, math.min(k, 64)))
  private var n = 0
  private val keys = new LongSet
  private var arrivals = 0L

  def size: Int = n

  /** Would an offer with this score be kept (key aside)? Cheap pre-check. */
  def accepts(score: Double, score2: Double = 0.0): Boolean =
    k > 0 && !score.isNaN && (n < k || score < heap(0).score ||
      (score == heap(0).score && score2 < heap(0).score2))

  def offer(score: Double, key: Long, value: A): Unit = offer(score, 0.0, key, value)

  /** `score2` breaks ties in `score`; remaining ties keep the earlier offer. */
  def offer(score: Double, score2: Double, key: Long, value: A): Unit =
    if accepts(score, score2) && !keys.contains(key) then
      arrivals += 1
      val e = new Entry(score, score2, arrivals, key, value)
      if n >= k then
        keys.remove(heap(0).key)
        heap(0) = e
        siftDown(0)
      else
        if n == heap.length then heap = java.util.Arrays.copyOf(heap, math.min(k, n * 2))
        heap(n) = e; n += 1
        siftUp(n - 1)
      keys.add(key)

  private def siftUp(from: Int): Unit =
    var i = from
    val e = heap(i)
    while i > 0 && worse(e, heap((i - 1) >> 1)) do
      heap(i) = heap((i - 1) >> 1); i = (i - 1) >> 1
    heap(i) = e

  private def siftDown(from: Int): Unit =
    var i = from
    val e = heap(i)
    var go = true
    while go do
      var c = 2 * i + 1
      if c >= n then go = false
      else
        if c + 1 < n && worse(heap(c + 1), heap(c)) then c += 1
        if worse(heap(c), e) then { heap(i) = heap(c); i = c } else go = false
    heap(i) = e

  private def sortedEntries: Array[Entry] =
    val a = java.util.Arrays.copyOf(heap, n)
    java.util.Arrays.sort(a.asInstanceOf[Array[AnyRef]], (x, y) =>
      val (p, q) = (x.asInstanceOf[Entry], y.asInstanceOf[Entry])
      if worse(p, q) then 1 else if worse(q, p) then -1 else 0)
    a

  /** Kept values, best first. */
  def result: Vector[A] = sortedEntries.iterator.map(_.value).toVector

  /** Kept scores, best first (same order as `result`). */
  def scores: Vector[Double] = sortedEntries.iterator.map(_.score).toVector

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
  private val tops    = targets.map(t => t -> new TopK[SearchEngine.Cand](k))
  private val overall = new TopK[SearchEngine.Cand](k)
  private var topCache: Vector[CompoundDesign] = null
  private var perTargetCache: Map[Ratio, Vector[Scored]] = null
  private var foundCount = 0L
  private var stopped = false

  /** Run up to `units` units of work; true once the whole space is consumed. */
  def step(units: Int): Boolean =
    topCache = null; perTargetCache = null
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
  def top: Vector[CompoundDesign] =
    if topCache == null then topCache = overall.result.map(_.design)
    topCache

  /** Best designs per target by absolute error, best first (empty without targets). */
  def perTarget: Map[Ratio, Vector[Scored]] =
    if perTargetCache == null then
      perTargetCache = tops.map { (t, acc) =>
        t -> acc.result.map { c => val d = c.design; Scored(t, Scorer.absoluteError(t, d.ratio), d) }
      }.toMap
    perTargetCache

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

    // Secondary key (same as `Scorer.tieBreak`): best-fit error when targets exist and the mode is
    // not BestFit, else |ratio|. Per-target accumulators break ties by |ratio|.
    val score2 = if targets.nonEmpty && sortBy != SortBy.BestFit then minErr else a
    var want = overall.accepts(score, score2)
    ti = 0
    while !want && ti < tops.length do
      want = tops(ti)._2.accepts(math.abs(math.abs(tops(ti)._1.value) - a), a)
      ti += 1
    if want then
      val c   = new SearchEngine.Cand(cfg, s1, s2, r, ev.wellConditioned, ev.module1, ev.module2)
      val key = Scorer.dedupKey(s1, s2) // the topology is fixed for the engine
      overall.offer(score, score2, key, c)
      ti = 0
      while ti < tops.length do
        val (t, acc) = tops(ti)
        acc.offer(math.abs(math.abs(t.value) - a), a, key, c)
        ti += 1

object SearchEngine:
  /** A found design, kept light until a reader needs the full `CompoundDesign`. */
  final class Cand(
      cfg: SearchConfig, s1: GearSet, s2: GearSet,
      ratio: Double, wellConditioned: Boolean, module1: Double, module2: Double
  ):
    private var built: CompoundDesign = null
    def design: CompoundDesign =
      if built == null then
        built = Compounder.design(cfg, s1, Option(s2), ratio, wellConditioned, module1, module2)
      built
