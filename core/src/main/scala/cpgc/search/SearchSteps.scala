package cpgc.search

import cpgc.constraints.SearchConfig
import cpgc.domain.{CompoundDesign, GearSet, Mode}

/** Step-grained enumeration of all valid compound designs for `cfg`.
  *
  * The element type is `Option[CompoundDesign]` so that each `next()` does
  * exactly one unit of work and immediately returns control to the caller:
  *
  *   - `Some(d)` — this step produced and accepted a design.
  *   - `None`    — this step advanced internal state (peeled the next stage-1
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

    private val stagesBuf  = scala.collection.mutable.ArrayBuffer.empty[GearSet]
    private val stagesIter = Enumerator.gearSets(cfg).iterator
    private val singleStage = cfg.mode == Mode.SingleStage
    private val targets    = cfg.targets.toVector
    // Closed-form `Compounder.stage2For` only exists for the four legacy
    // topologies; novel role tuples fall back to the full cross product even
    // when targets are present (Scorer ranks by target fit at the end).
    private val useStage2For =
      targets.nonEmpty && cfg.effectiveTopology.legacyMode.isDefined

    // 0 = materialising stage candidates; 1 = pairing/emitting.
    private var phase: Int = 0
    private var iIdx: Int  = 0  // outer (stage-1) index in stagesBuf
    private var jIdx: Int  = 0  // inner (stage-2) index — full cross-product
    private var tIdx: Int  = 0  // current target index — useStage2For path
    private var s2Iter: Iterator[GearSet] = Iterator.empty

    private var done: Boolean = false

    override def hasNext: Boolean = !done

    override def next(): Option[CompoundDesign] =
      if done then Iterator.empty.next()
      else phase match
        case 0 =>
          if stagesIter.hasNext then
            stagesBuf += stagesIter.next()
            None
          else
            phase = 1
            // If there are zero stage-1 candidates we're already done.
            if stagesBuf.isEmpty then done = true
            None
        case 1 =>
          val n = stagesBuf.length
          if singleStage then
            if iIdx >= n then { done = true; None }
            else
              val r = Compounder.single(cfg, stagesBuf(iIdx))
              iIdx += 1
              r
          else if useStage2For then
            if iIdx >= n then { done = true; None }
            else if s2Iter.hasNext then
              val s2 = s2Iter.next()
              Compounder.compound(cfg, stagesBuf(iIdx), s2)
            else if tIdx < targets.length then
              s2Iter = Compounder.stage2For(targets(tIdx).value, stagesBuf(iIdx), cfg)
              tIdx += 1
              None
            else
              tIdx = 0
              iIdx += 1
              None
          else
            // Full cross-product. One pair per step.
            if iIdx >= n then { done = true; None }
            else if jIdx >= n then
              jIdx = 0
              iIdx += 1
              if iIdx >= n then done = true
              None
            else
              val r =
                if iIdx == jIdx then None
                else Compounder.compound(cfg, stagesBuf(iIdx), stagesBuf(jIdx))
              jIdx += 1
              r
        case _ =>
          done = true
          None
