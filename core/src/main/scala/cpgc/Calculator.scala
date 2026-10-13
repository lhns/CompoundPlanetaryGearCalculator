package cpgc

import cpgc.constraints.SearchConfig
import cpgc.domain.{CompoundDesign, Ratio}
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

  /** Lazy stream of all valid designs implied by `cfg.topology`. Stage 1 is
    * drawn from the stage-1 pool and stage 2 from the stage-2 pool (they differ
    * when only one stage has a sun). Spacing, stacking and module rules are
    * applied inside `Compounder`. */
  def search(cfg: SearchConfig): LazyList[CompoundDesign] =
    if cfg.topology.stages == 1 then
      Enumerator.stage1Sets(cfg).flatMap(Compounder.single(cfg, _))
    else
      val pool1   = Enumerator.stage1Sets(cfg).toVector
      val samePool = cfg.topology.s1Present == cfg.topology.s2Present
      val pool2   = if samePool then pool1 else Enumerator.stage2Sets(cfg).toVector
      for
        a <- pool1.to(LazyList)
        b <- pool2.to(LazyList)
        if !(samePool && a == b)
        d <- Compounder.compound(cfg, a, b)
      yield d

  /** Targeted compound search using the inverse-solver fast path. For each
    * stage-1 candidate, derive stage-2 candidates whose ratio is close to each
    * target (`Compounder.stage2For`). Faster than the full pair enumeration but
    * not complete (bug register B4; replaced in milestone B).
    */
  def searchTargeted(cfg: SearchConfig): LazyList[CompoundDesign] =
    if cfg.topology.stages == 1 || cfg.targets.isEmpty then search(cfg)
    else
      val stage1s = Enumerator.stage1Sets(cfg).toVector
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
