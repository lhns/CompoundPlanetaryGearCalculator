package cpgc.search

import cpgc.constraints.SearchConfig
import cpgc.domain.{GearSet, Mode, Teeth}
import cpgc.physics.PlanetSpacing

/** Enumerates valid single-stage `GearSet` candidates under the supplied
  * configuration. Lazy — callers may take only what they need.
  */
object Enumerator:

  def gearSets(cfg: SearchConfig): LazyList[GearSet] =
    val ps = cfg.planets.range.to(LazyList)
    ps.flatMap(np => gearSetsFor(cfg, np))

  def gearSetsFor(cfg: SearchConfig, numPlanets: Int): LazyList[GearSet] =
    val suns = cfg.sunTeeth.range.to(LazyList)
    for
      s        <- suns
      p        <- cfg.planetTeeth.range.to(LazyList)
      set       = GearSet(Teeth(s), Teeth(p), numPlanets)
      ring      = set.ring.toInt
      if ring >= cfg.ringTeeth.min && ring <= cfg.ringTeeth.max
      if p.toDouble < cfg.sunPlanetFactor * s.toDouble
      if matchesMode(cfg.mode, set)
      if PlanetSpacing.planetsFitWithoutOverlap(set)
    yield set

  private def matchesMode(mode: Mode, set: GearSet): Boolean = mode match
    case Mode.CDCP                          => true            // sun and spacing relaxed
    case Mode.Standard | Mode.SingleStage   => set.evenSpacing
    case Mode.NSS                           => set.evenSpacing
