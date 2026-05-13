package cpgc

import cpgc.constraints.SearchConfig
import cpgc.domain.{CompoundDesign, Mode, Ratio}
import cpgc.search.{Compounder, Enumerator, Scorer}

object Calculator:

  /** Lazy stream of all valid designs implied by `cfg`. */
  def search(cfg: SearchConfig): LazyList[CompoundDesign] = cfg.mode match
    case Mode.SingleStage =>
      Enumerator.gearSets(cfg).flatMap(Compounder.single(cfg, _))
    case _ =>
      val stages = Enumerator.gearSets(cfg).toVector
      for
        a <- stages.to(LazyList)
        b <- stages.to(LazyList)
        if a != b
        d <- Compounder.compound(cfg, a, b)
      yield d

  /** Top-K designs per target ratio. */
  def best(cfg: SearchConfig): Map[Ratio, Vector[Scorer.Scored]] =
    Scorer.topPerTarget(search(cfg), cfg.targets, cfg.topPerTarget)
