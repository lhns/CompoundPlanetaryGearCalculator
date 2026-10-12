package cpgc.search

import cpgc.constraints.SearchConfig
import cpgc.domain.{GearSet, Teeth}
import cpgc.physics.PlanetSpacing

/** Enumerates valid single-stage `GearSet` candidates under the supplied
  * configuration. Lazy — callers may take only what they need.
  *
  * Mode-dependent spacing rules live in `Compounder.compound` /
  * `Compounder.single`. The enumerator emits the broad geometric candidate
  * pool (tooth-count ranges, `sunPlanetFactor`, planets-fit-without-overlap);
  * the spacing requirement for each stage is applied per mode at pair-up.
  */
object Enumerator:

  def gearSets(cfg: SearchConfig): LazyList[GearSet] =
    val ps = cfg.planets.range.to(LazyList)
    ps.flatMap(np => gearSetsFor(cfg, np))

  def gearSetsFor(cfg: SearchConfig, numPlanets: Int): LazyList[GearSet] =
    val suns = cfg.sunTeeth.range.to(LazyList)
    for
      s <- suns
      p <- cfg.planetTeeth.range.to(LazyList)
      set  = GearSet(Teeth(s), Teeth(p), numPlanets)
      ring = set.ring.toInt
      if ring >= cfg.ringTeeth.min && ring <= cfg.ringTeeth.max
      if p.toDouble < cfg.sunPlanetFactor * s.toDouble
      if PlanetSpacing.planetsFitWithoutOverlap(set)
    yield set
