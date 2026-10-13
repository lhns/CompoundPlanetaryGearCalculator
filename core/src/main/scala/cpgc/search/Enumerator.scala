package cpgc.search

import cpgc.constraints.SearchConfig
import cpgc.domain.{GearSet, Teeth}
import cpgc.physics.PlanetSpacing

/** Enumerates valid single-stage `GearSet` candidates under the supplied
  * configuration. Lazy: callers may take only what they need.
  *
  * Spacing (`(S + R) % N == 0`) is applied per stage at pair-up in
  * `Compounder`; the enumerator emits the geometric candidate pool: tooth-count
  * ranges, `sunPlanetFactor` and the planet fit (with `planetClearance`).
  *
  * A stage whose sun is present is enumerated over the sun/planet ranges. A
  * sun-less stage (ADR 0015 §6) is enumerated over the ring/planet ranges with
  * the virtual sun `Ts = Tr − 2·Tp >= 1`; the sun-teeth range and the
  * sun/planet factor don't apply, the planet-fit predicate rejects the rest.
  */
object Enumerator:

  /** Candidate pool for stage 1 / stage 2 of `cfg.topology`. */
  def stage1Sets(cfg: SearchConfig): LazyList[GearSet] = gearSets(cfg, cfg.topology.s1Present)
  def stage2Sets(cfg: SearchConfig): LazyList[GearSet] = gearSets(cfg, cfg.topology.s2Present)

  def gearSets(cfg: SearchConfig, sunPresent: Boolean = true): LazyList[GearSet] =
    cfg.planets.range.to(LazyList).flatMap(np => gearSetsFor(cfg, np, sunPresent))

  def gearSetsFor(cfg: SearchConfig, numPlanets: Int, sunPresent: Boolean = true): LazyList[GearSet] =
    if sunPresent then
      for
        s <- cfg.sunTeeth.range.to(LazyList)
        p <- cfg.planetTeeth.range.to(LazyList)
        set  = GearSet(Teeth(s), Teeth(p), numPlanets)
        ring = set.ring.toInt
        if ring >= cfg.ringTeeth.min && ring <= cfg.ringTeeth.max
        if p.toDouble < cfg.sunPlanetFactor * s.toDouble
        if PlanetSpacing.planetsFitWithoutOverlap(set, cfg.planetClearance)
      yield set
    else
      for
        p <- cfg.planetTeeth.range.to(LazyList)
        r <- cfg.ringTeeth.range.to(LazyList)
        if r - 2 * p >= 1
        set = GearSet(Teeth(r - 2 * p), Teeth(p), numPlanets)
        if PlanetSpacing.planetsFitWithoutOverlap(set, cfg.planetClearance)
      yield set
