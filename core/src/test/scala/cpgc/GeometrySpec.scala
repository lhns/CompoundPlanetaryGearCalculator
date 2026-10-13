package cpgc

import cpgc.constraints.{SearchConfig, TeethBounds, Topology}
import cpgc.domain.{GearSet, Module, Stage, Teeth}
import cpgc.physics.PlanetSpacing
import cpgc.search.Compounder
import munit.FunSuite

/** ADR 0016: physical ring outer diameter and planet tip clearance. */
class GeometrySpec extends FunSuite:

  private val s1 = GearSet(Teeth(24), Teeth(12), 3) // ring 48

  test("golden: m1 = (OD − 2·ringWall) / (Tr1 + 2.5)") {
    val cfg = SearchConfig(firstStageOD = 150.0, ringWall = 2.0)
    assertEqualsDouble(Compounder.firstModule(cfg, s1).get.value, 146.0 / 50.5, 1e-12)
    assertEqualsDouble(Compounder.firstModule(cfg.copy(ringWall = 0.0), s1).get.value, 150.0 / 50.5, 1e-12)
    assertEqualsDouble(Compounder.firstModule(cfg.copy(ringWall = 5.0, firstStageOD = 100.0), s1).get.value, 90.0 / 50.5, 1e-12)
  }

  test("defaults: ringWall 2 mm, planetClearance 0.5 modules") {
    val cfg = SearchConfig()
    assertEquals(cfg.ringWall, 2.0)
    assertEquals(cfg.planetClearance, 0.5)
  }

  test("golden: Stage.ringOuterDiameter = m·(Tr + 2.5) + 2·wall") {
    val st = Stage(s1, Module(2.0), ringWall = 3.0)
    assertEqualsDouble(st.ringOuterDiameter, 2.0 * 50.5 + 6.0, 1e-12)
    assertEqualsDouble(Stage(s1, Module(2.0)).ringOuterDiameter, 2.0 * 50.5 + 4.0, 1e-12) // default wall 2
  }

  test("a built design's stage-1 ring OD equals the requested OD") {
    val cfg = SearchConfig(topology = Topology.singleStage, firstStageOD = 120.0, ringWall = 3.0, minModule = 0.0)
    val d = Compounder.single(cfg, s1).get
    assertEqualsDouble(d.stage1.ringOuterDiameter, 120.0, 1e-9)
    assertEquals(d.stage1.ringWall, 3.0)
  }

  test("planet fit: Tp + 2 + clearance < (Ts + Tp)·sin(π/N)") {
    // Ts = 4, Tp = 10, N = 3: chord = 14·sin60° = 12.124 > Tp + 2 = 12 by 0.124 modules.
    val g = GearSet(Teeth(4), Teeth(10), 3)
    assert(PlanetSpacing.planetsFitWithoutOverlap(g, 0.0))
    assert(PlanetSpacing.planetsFitWithoutOverlap(g, 0.1))
    assert(!PlanetSpacing.planetsFitWithoutOverlap(g, 0.2))
    assert(!PlanetSpacing.planetsFitWithoutOverlap(g, SearchConfig.DefaultPlanetClearance))
    // roomy set
    assert(PlanetSpacing.planetsFitWithoutOverlap(s1, 0.5))
    // too many planets
    assert(!PlanetSpacing.planetsFitWithoutOverlap(GearSet(Teeth(12), Teeth(20), 8), 0.0))
  }

  test("the search applies planetClearance") {
    val base = SearchConfig(
      topology = Topology.singleStage, sunTeeth = TeethBounds(4, 4), planetTeeth = TeethBounds(10, 10),
      ringTeeth = TeethBounds(24, 24), planets = TeethBounds(3, 3), minModule = 0.0, sunPlanetFactor = 10.0)
    // (4 + 24) % 3 != 0, so use the enumerator pool only
    assertEquals(cpgc.search.Enumerator.stage1Sets(base.copy(planetClearance = 0.0)).size, 1)
    assertEquals(cpgc.search.Enumerator.stage1Sets(base.copy(planetClearance = 0.5)).size, 0)
  }

  test("B7: an OD that cannot hold the ring wall yields no designs and does not throw") {
    for od <- Seq(0.0, -10.0, 4.0, Double.NaN, Double.PositiveInfinity) do
      val cfg = SearchConfig(
        topology = Topology.standardCompound, firstStageOD = od, minModule = 0.0,
        sunTeeth = TeethBounds(10, 14), planetTeeth = TeethBounds(8, 10), ringTeeth = TeethBounds(30, 40),
        planets = TeethBounds(3, 3), targets = Seq(cpgc.domain.Ratio(10.0)))
      assertEquals(Calculator.search(cfg).size, 0, s"OD=$od")
      assertEquals(Calculator.top(cfg).size, 0, s"OD=$od")
      assertEquals(Calculator.searchSteps(cfg).flatten.size, 0, s"OD=$od")
      assertEquals(Calculator.best(cfg).values.map(_.size).sum, 0, s"OD=$od")
  }
