package cpgc

import cpgc.constraints.SearchConfig
import cpgc.domain.{CompoundDesign, Ratio}
import cpgc.search.{Scorer, SearchEngine, SearchSteps}
import cpgc.search.Scorer.SortBy

/** Thin wrappers over the one search engine (ADR 0017). */
object Calculator:

  /** Step-grained iterator over every valid design. Each `next()` does one unit
    * of work, returning `Some(design)` when it accepted a candidate or `None`
    * when it only advanced state. See `cpgc.search.SearchSteps`. */
  def searchSteps(cfg: SearchConfig): Iterator[Option[CompoundDesign]] =
    SearchSteps.forConfig(cfg)

  /** Lazy stream of every valid design implied by `cfg` (enumeration order). */
  def search(cfg: SearchConfig): LazyList[CompoundDesign] =
    LazyList.from(SearchSteps.forConfig(cfg)).flatten

  /** The engine for `cfg`, to be driven in chunks (`step`) or run to completion. */
  def engine(cfg: SearchConfig, sortBy: SortBy = SortBy.BestFit, k: Int = 10): SearchEngine =
    new SearchEngine(cfg, sortBy, k)

  /** The `k` best designs by `sortBy` over the whole space, best first. */
  def top(cfg: SearchConfig, sortBy: SortBy = SortBy.BestFit, k: Int = 10): Vector[CompoundDesign] =
    engine(cfg, sortBy, k).run().top

  /** Top `cfg.topPerTarget` designs per target ratio (empty without targets). */
  def best(cfg: SearchConfig): Map[Ratio, Vector[Scorer.Scored]] =
    if cfg.targets.isEmpty then Map.empty
    else engine(cfg, SortBy.BestFit, cfg.topPerTarget).run().perTarget
