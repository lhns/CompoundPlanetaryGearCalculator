package cpgc.search

import cpgc.constraints.SearchConfig
import cpgc.domain.{CompoundDesign, GearSet}

import scala.collection.mutable

/** The one candidate enumeration of the calculator (ADR 0017): every stage pair
  * from the per-topology stage pools.
  *
  *  - Stage 1 comes from the stage-1 pool, stage 2 from the stage-2 pool (they
  *    differ when only one stage has a sun). Identical pairs are included.
  *  - Stacked topologies pair only equal planet counts; non-stacked ones allow
  *    `N1 != N2`.
  *  - Stages that fail their own sum rule `(S + R) % N == 0` are dropped from
  *    the pool before pairing, then `Compounder.Evaluator` applies the
  *    remaining cheap rejects (assembly cross term, modules) before solving.
  *
  * One call to `advance()` is one unit of work: either pulling the next stage
  * into a pool or evaluating one pair. It returns true when that unit produced
  * an accepted candidate, which is then readable through `s1`, `s2` and
  * `evaluator`. Not thread-safe.
  */
final class PairSearch(cfg: SearchConfig):
  private val topology    = cfg.topology
  private val singleStage = topology.stages == 1
  private val samePool    = topology.s1Present == topology.s2Present
  private val eval        = new Compounder.Evaluator(cfg)

  private val buf1  = mutable.ArrayBuffer.empty[GearSet]
  private val buf2  = if samePool then buf1 else mutable.ArrayBuffer.empty[GearSet]
  private val iter1 = Enumerator.stage1Sets(cfg).iterator
  private val iter2 = if samePool || singleStage then Iterator.empty else Enumerator.stage2Sets(cfg).iterator
  private val byN   = mutable.HashMap.empty[Int, mutable.ArrayBuffer[GearSet]]
  private val noGears = mutable.ArrayBuffer.empty[GearSet]

  // 0 = pool 1, 1 = pool 2, 2 = pairing, 3 = done.
  private var phase = 0
  private var iIdx  = 0
  private var jIdx  = 0
  private var cands: mutable.ArrayBuffer[GearSet] = null

  private var examinedPairs = 0L

  /** Last accepted candidate. */
  var s1: GearSet = null
  var s2: GearSet = null // null for a single stage

  def evaluator: Compounder.Evaluator = eval
  def done: Boolean = phase == 3
  /** Number of candidates (pairs or single stages) evaluated so far. */
  def examined: Long = examinedPairs

  def advance(): Boolean = phase match
    case 0 =>
      if iter1.hasNext then
        val g = iter1.next()
        if !topology.s1Present || g.evenSpacing then buf1 += g
      else
        phase = if singleStage then 2 else if samePool then 2 else 1
        if buf1.isEmpty then phase = 3
        else if phase == 2 then prepareInner()
      false
    case 1 =>
      if iter2.hasNext then
        val g = iter2.next()
        if !topology.s2Present || g.evenSpacing then buf2 += g
      else
        if buf2.isEmpty then phase = 3
        else
          phase = 2
          prepareInner()
      false
    case 2 =>
      if singleStage then
        if iIdx >= buf1.length then { phase = 3; false }
        else
          val g = buf1(iIdx); iIdx += 1
          examinedPairs += 1
          if eval.accept(g, null) then { s1 = g; s2 = null; true } else false
      else if iIdx >= buf1.length then { phase = 3; false }
      else if jIdx >= cands.length then
        iIdx += 1; jIdx = 0
        if iIdx >= buf1.length then phase = 3 else setCands()
        false
      else
        val a = buf1(iIdx); val b = cands(jIdx); jIdx += 1
        examinedPairs += 1
        if eval.accept(a, b) then { s1 = a; s2 = b; true } else false
    case _ => false

  /** Index stage 2 by planet count (stacked) and position on the first stage-1 entry. */
  private def prepareInner(): Unit =
    if !singleStage then
      if topology.stacked then for g <- buf2 do byN.getOrElseUpdate(g.numPlanets, mutable.ArrayBuffer.empty) += g
      setCands()

  private def setCands(): Unit =
    cands = if topology.stacked then byN.getOrElse(buf1(iIdx).numPlanets, noGears) else buf2

object SearchSteps:

  /** Step-grained enumeration of all valid designs for `cfg`. Each `next()` does
    * one unit of work: `Some(design)` when it accepted a candidate, `None` when
    * it only advanced state. Use it when bounded latency per step matters (a
    * browser chunk loop). `hasNext == false` once the space is consumed. */
  def forConfig(cfg: SearchConfig): Iterator[Option[CompoundDesign]] =
    new Iterator[Option[CompoundDesign]]:
      private val ps = new PairSearch(cfg)
      override def hasNext: Boolean = !ps.done
      override def next(): Option[CompoundDesign] =
        if ps.done then Iterator.empty.next()
        else if ps.advance() then Some(Compounder.design(cfg, ps.s1, Option(ps.s2), ps.evaluator))
        else None
