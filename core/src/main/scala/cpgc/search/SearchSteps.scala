package cpgc.search

import cpgc.constraints.SearchConfig
import cpgc.domain.{CompoundDesign, GearSet}

/** Step-grained enumeration of all valid compound designs for `cfg`.
  *
  * The element type is `Option[CompoundDesign]` so that each `next()` does
  * exactly one unit of work and immediately returns control to the caller:
  *
  *   - `Some(d)` — this step produced and accepted a design.
  *   - `None`    — this step advanced internal state (peeled the next stage
  *                 candidate off the enumerator, evaluated a pair that didn't
  *                 form a valid compound design, etc.).
  *   - `hasNext == false` — the candidate space is consumed.
  *
  * Compared to `LazyList[CompoundDesign]` / `Iterator[CompoundDesign]`, this
  * shape moves the "did we emit?" signal from the iteration **rate** into the
  * element **type**, so a caller wanting bounded latency per step (a browser
  * chunk loop, an animation frame) never has to traverse an unbounded number
  * of internal rejections between deadline checks.
  */
object SearchSteps:

  def forConfig(cfg: SearchConfig): Iterator[Option[CompoundDesign]] =
    new StepIterator(cfg)

  private final class StepIterator(cfg: SearchConfig)
      extends Iterator[Option[CompoundDesign]]:

    private val topology    = cfg.topology
    private val singleStage = topology.stages == 1
    // Stage 1 and stage 2 draw from different pools when only one has a sun.
    private val samePool    = topology.s1Present == topology.s2Present
    private val buf1  = scala.collection.mutable.ArrayBuffer.empty[GearSet]
    private val buf2  = if samePool then buf1 else scala.collection.mutable.ArrayBuffer.empty[GearSet]
    private val iter1 = Enumerator.stage1Sets(cfg).iterator
    private val iter2 = if samePool then Iterator.empty else Enumerator.stage2Sets(cfg).iterator
    private val targets = cfg.targets.toVector
    // The inverse `Compounder.stage2For` is used whenever targets are given;
    // it falls back to a windowed brute force for non-canonical topologies.
    private val useStage2For = targets.nonEmpty && !singleStage && topology.legacyMode.isDefined

    // 0 = materialising stage-1 candidates; 1 = stage-2 candidates; 2 = pairing/emitting.
    private var phase: Int = 0
    private var iIdx: Int  = 0  // outer (stage-1) index in buf1
    private var jIdx: Int  = 0  // inner (stage-2) index in buf2 — full cross-product
    private var tIdx: Int  = 0  // current target index — useStage2For path
    private var s2Iter: Iterator[GearSet] = Iterator.empty

    private var done: Boolean = false

    override def hasNext: Boolean = !done

    override def next(): Option[CompoundDesign] =
      if done then Iterator.empty.next()
      else phase match
        case 0 =>
          if iter1.hasNext then
            buf1 += iter1.next()
            None
          else
            phase = if samePool || singleStage || useStage2For then 2 else 1
            if buf1.isEmpty then done = true
            None
        case 1 =>
          if iter2.hasNext then
            buf2 += iter2.next()
            None
          else
            phase = 2
            if buf2.isEmpty then done = true
            None
        case 2 =>
          val n  = buf1.length
          val n2 = buf2.length
          if singleStage then
            if iIdx >= n then { done = true; None }
            else
              val r = Compounder.single(cfg, buf1(iIdx))
              iIdx += 1
              r
          else if useStage2For then
            if iIdx >= n then { done = true; None }
            else if s2Iter.hasNext then
              val s2 = s2Iter.next()
              Compounder.compound(cfg, buf1(iIdx), s2)
            else if tIdx < targets.length then
              s2Iter = Compounder.stage2For(targets(tIdx).value, buf1(iIdx), cfg)
              tIdx += 1
              None
            else
              tIdx = 0
              iIdx += 1
              None
          else
            // Full cross-product. One pair per step.
            if iIdx >= n then { done = true; None }
            else if jIdx >= n2 then
              jIdx = 0
              iIdx += 1
              if iIdx >= n then done = true
              None
            else
              val r =
                if samePool && iIdx == jIdx then None
                else Compounder.compound(cfg, buf1(iIdx), buf2(jIdx))
              jIdx += 1
              r
        case _ =>
          done = true
          None
