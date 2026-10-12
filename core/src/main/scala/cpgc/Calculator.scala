package cpgc

import cpgc.constraints.SearchConfig
import cpgc.domain.{CompoundDesign, Mode, Ratio}
import cpgc.search.{Compounder, Enumerator, Scorer, SearchSteps}

object Calculator:

  /** Step-grained iterator over the same candidate space as `search` /
    * `searchTargeted`. Each `next()` does one unit of work, returning
    * `Some(design)` when a step emits or `None` when it only advances state.
    * Use this when bounded per-step latency matters (browser chunk loops,
    * cancellation between deadline checks). See `cpgc.search.SearchSteps`.
    */
  def searchSteps(cfg: SearchConfig): Iterator[Option[CompoundDesign]] =
    SearchSteps.forConfig(cfg)

  /** Lazy stream of all valid designs implied by `cfg`. */
  def search(cfg: SearchConfig): LazyList[CompoundDesign] = cfg.mode match
    case Mode.SingleStage =>
      Enumerator.gearSets(cfg).flatMap(Compounder.single(cfg, _))
    case _ =>
      // All multi-stage modes share the same single-stage candidate pool.
      // Per-mode spacing rules are applied inside `Compounder.compound`:
      // Standard requires both stages' Willis; NSS only stage 1's; CDCP
      // neither.
      val stages = Enumerator.gearSets(cfg).toVector
      for
        a <- stages.to(LazyList)
        b <- stages.to(LazyList)
        if a != b
        d <- Compounder.compound(cfg, a, b)
      yield d

  /** Targeted compound search using the inverse-solver fast path. For each
    * stage-1 candidate from the enumerator, algebraically derive stage-2
    * candidates that produce ratios close to each target. ~50× faster than
    * full O(K²) pair enumeration for typical bounds.
    *
    * For NSS the inverse formula is identical (Ts2 doesn't appear) but the
    * stage-2 validity check skips the sun-range and uses ring-only spacing
    * `Tr2 % N == 0`. See `Compounder.stage2For` for the per-mode branching.
    */
  def searchTargeted(cfg: SearchConfig): LazyList[CompoundDesign] =
    if cfg.mode == Mode.SingleStage then search(cfg)
    else if cfg.targets.isEmpty then search(cfg)
    else
      val stage1s = Enumerator.gearSets(cfg).toVector
      val targets = cfg.targets.map(_.value)
      for
        a <- stage1s.to(LazyList)
        t <- targets.to(LazyList)
        b <- Compounder.stage2For(t, a, cfg).to(LazyList)
        d <- Compounder.compound(cfg, a, b)
      yield d

  /** Top-K designs per target ratio. */
  def best(cfg: SearchConfig): Map[Ratio, Vector[Scorer.Scored]] =
    val stream = if cfg.targets.nonEmpty then searchTargeted(cfg) else search(cfg)
    Scorer.topPerTarget(stream, cfg.targets, cfg.topPerTarget)
