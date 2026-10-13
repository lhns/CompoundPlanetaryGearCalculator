package cpgc.search

import cpgc.Calculator
import cpgc.constraints.{Body, BodyRole, Coupling, SearchConfig, TeethBounds, Topology}
import cpgc.domain.{GearSet, Teeth}
import cpgc.kinematics.Kinematics
import munit.FunSuite

/** Absent suns (ADR 0015 §2, §3, §5, §6): the solver layout, the sun-less CDCP
  * goldens, the spacing rule and the stage pools. */
class AbsentSunSpec extends FunSuite:

  private val sunless = Topology.sunlessCdcpCompound

  // Rings 48 and 42 (virtual suns 24 and 22), N = 3.
  private val s1 = GearSet(Teeth(24), Teeth(12), 3)
  private val s2 = GearSet(Teeth(22), Teeth(10), 3)

  // -- goldens -----------------------------------------------------------------

  test("golden: sun-less CDCP ratio = Tp1·Tr2 / (Tp1·Tr2 − Tr1·Tp2)") {
    // 12·42 / (12·42 − 48·10) = 504 / 24 = 21
    val r = RoleSolver.ratio(sunless, s1, Some(s2)).get.value
    assertEqualsDouble(r, 21.0, 1e-12)
  }

  test("sun-less CDCP ratio formula holds over a sweep") {
    for p1 <- 8 to 14; r1 <- 40 to 52; p2 <- 8 to 14; r2 <- 40 to 52 do
      val a = GearSet(Teeth(r1 - 2 * p1), Teeth(p1), 3)
      val b = GearSet(Teeth(r2 - 2 * p2), Teeth(p2), 3)
      val den = p1.toDouble * r2 - r1.toDouble * p2
      val got = RoleSolver.ratio(sunless, a, Some(b))
      if den == 0.0 then assertEquals(got, None)
      else assertEqualsDouble(got.get.value, p1.toDouble * r2 / den, 1e-9)
  }

  test("sun-less CDCP: planet velocities are exposed and satisfy both ring meshes") {
    val v = RoleSolver.velocities(sunless, s1, Some(s2), omegaIn = 1.0).get
    // C1 in (=1), R1 fixed: Tr1·(0 − 1) = Tp1·(ωP1 − 1)
    assertEqualsDouble(v.omegaP1, 1.0 - 48.0 / 12.0, 1e-12)
    assertEqualsDouble(v.omegaP2, v.omegaP1, 1e-12) // stacked planets
    assertEqualsDouble(42.0 * (v.omegaR2 - v.omegaC2) - 10.0 * (v.omegaP2 - v.omegaC2), 0.0, 1e-12)
    assertEqualsDouble(v.omegaS1, 0.0, 0.0) // absent suns report 0
    assertEqualsDouble(v.omegaS2, 0.0, 0.0)
    assertEquals(v.values.keySet, sunless.presentBodies)
  }

  test("sun mesh row only exists when the sun is present: unknown / row counts match") {
    for t <- Seq(Topology.singleStage, Topology.standardCompound, Topology.cdcpCompound,
                 Topology.nssCompound, sunless) do
      val sets = if t.stages == 1 then None else Some(s2)
      assert(RoleSolver.ratio(t, s1, sets).isDefined, s"$t")
    assertEquals(sunless.unknowns, 6)               // C1 R1 C2 R2 P1 P2
    assertEquals(Topology.nssCompound.unknowns, 7)  // + S1
    assertEquals(Topology.standardCompound.unknowns, 8)
    assertEquals(Topology.singleStage.unknowns, 4)
  }

  test("S1 absent with a Free-body input: ratio solves and matches the sun-less-stage-1 closed form") {
    // C1 in, R1 fixed, R2 out, CC + PS, S1 absent (S2 present): stage 2's sun mesh adds the S2 unknown/row.
    val t = Topology(2, sunless.roles.updated(Body.S2, BodyRole.Free))
    assertEquals(t.unknowns, 7)
    val r = RoleSolver.ratio(t, s1, Some(s2)).get.value
    // Stage 1 (ring only) fixes the planet; stage 2's planet equals it and its ring runs
    // as in the sun-less case, S2 free-floating. Same ratio as the sun-less CDCP.
    val ref = RoleSolver.ratio(sunless, s1, Some(s2)).get.value
    assertEqualsDouble(r, ref, 1e-12)
  }

  test("couplings apply to S2-absent topologies (the NSS special row is gone)") {
    val roles = Topology.nssCompound.roles.updated(Body.R1, BodyRole.Free).updated(Body.C1, BodyRole.Fixed)
    val ps = Topology(2, roles, Coupling.CarrierToCarrier, Coupling.PlanetStack)
    val rr = Topology(2, roles, Coupling.CarrierToCarrier, Coupling.RingToRing)
    val a = RoleSolver.ratio(ps, s1, Some(s2)).get.value
    val b = RoleSolver.ratio(rr, s1, Some(s2)).get.value
    assert(math.abs(a - b) > 1e-6, s"PlanetStack ($a) and RingToRing ($b) must differ")
    val v = RoleSolver.velocities(rr, s1, Some(s2), 1.0).get
    assertEqualsDouble(v.omegaR1, v.omegaR2, 1e-12)
  }

  test("the solver is total: a compound without a stage-2 gear set is None") {
    assertEquals(RoleSolver.ratio(Topology.standardCompound, s1, None), None)
    assertEquals(RoleSolver.velocities(sunless, s1, None, 1.0), None)
  }

  // -- kinematics --------------------------------------------------------------

  test("Kinematics.at takes planet spins from the solver for sun-less stages") {
    val cfg = SearchConfig(
      topology = sunless, planetTeeth = TeethBounds(8, 12), ringTeeth = TeethBounds(40, 52),
      planets = TeethBounds(3, 3), minModule = 0.0, planetClearance = 0.0)
    val ds = Calculator.search(cfg).take(20).toVector
    assert(ds.nonEmpty)
    ds.foreach { d =>
      val b = Kinematics.at(d, 2.0)
      val (in, out) = Kinematics.io(d, b)
      assertEqualsDouble(in, 2.0, 1e-12)
      assertEqualsDouble(in / out, d.ratio.value, 1e-9 * math.max(1.0, math.abs(d.ratio.value)))
      assertEqualsDouble(b.sun1, 0.0, 0.0)
      assertEqualsDouble(b.sun2, 0.0, 0.0)
      val a = d.stage1.set; val c = d.stage2.get.set
      assertEqualsDouble(a.ring.toDouble * (b.ring1 - b.carrier1) - a.planet.toDouble * (b.planet1Spin - b.carrier1), 0.0, 1e-9)
      assertEqualsDouble(c.ring.toDouble * (b.ring2 - b.carrier2) - c.planet.toDouble * (b.planet2Spin - b.carrier2), 0.0, 1e-9)
    }
  }

  // -- spacing (ADR 0015 §5) ---------------------------------------------------

  test("canonical CDCP now requires (S + R) % N == 0 for both stages") {
    val cfg = SearchConfig(topology = Topology.cdcpCompound, minModule = 0.0, planetClearance = 0.0)
    val even   = GearSet(Teeth(24), Teeth(12), 3) // (24 + 48) % 3 == 0
    val uneven = GearSet(Teeth(20), Teeth(14), 3) // (20 + 48) % 3 == 2
    assert(even.evenSpacing && !uneven.evenSpacing)
    assert(Compounder.compound(cfg, even, GearSet(Teeth(20), Teeth(16), 3)).isDefined)
    assertEquals(Compounder.compound(cfg, uneven, GearSet(Teeth(20), Teeth(16), 3)), None)
    assertEquals(Compounder.compound(cfg, even, uneven), None)
  }

  test("sun-less stages have no spacing condition of their own") {
    val cfg = SearchConfig(topology = sunless, minModule = 0.0, planetClearance = 0.0)
    val uneven = GearSet(Teeth(20), Teeth(14), 3)
    assert(!uneven.evenSpacing)
    assert(Compounder.compound(cfg, uneven, GearSet(Teeth(22), Teeth(10), 3)).isDefined)
    // NSS: stage 1 (has a sun) still needs it, stage 2 does not
    val nss = SearchConfig(topology = Topology.nssCompound, minModule = 0.0, planetClearance = 0.0)
    assertEquals(Compounder.compound(nss, uneven, GearSet(Teeth(22), Teeth(10), 3)), None)
    assert(Compounder.compound(nss, GearSet(Teeth(24), Teeth(12), 3), uneven).isDefined)
  }

  // -- stacking (ADR 0015 §4) --------------------------------------------------

  test("stacked topologies need N1 == N2 and match the centre distance with virtual suns") {
    val cfg = SearchConfig(topology = sunless, minModule = 0.0, planetClearance = 0.0)
    assertEquals(Compounder.compound(cfg, s1, GearSet(Teeth(22), Teeth(10), 4)), None)
    val d = Compounder.compound(cfg, s1, s2).get
    val c1 = d.stage1.module.value * (24 + 12)
    val c2 = d.stage2.get.module.value * (22 + 10)
    assertEqualsDouble(c1, c2, 1e-9)
  }

  test("non-stacked topologies may pair different planet counts and keep m2 = m1") {
    val roles = Topology.standardCompound.roles
    val t = Topology(2, roles, Coupling.CarrierToCarrier, Coupling.RingToSun)
    val cfg = SearchConfig(topology = t, minModule = 0.0, planetClearance = 0.0)
    val d = Compounder.compound(cfg, GearSet(Teeth(24), Teeth(12), 3), GearSet(Teeth(24), Teeth(12), 4))
    assert(d.isDefined)
    assertEqualsDouble(d.get.stage2.get.module.value, d.get.stage1.module.value, 1e-12)
  }

  // -- pools (ADR 0015 §6) -----------------------------------------------------

  test("a sun-less stage is enumerated over ring/planet ranges with the virtual sun >= 1") {
    val cfg = SearchConfig(
      topology = sunless, sunTeeth = TeethBounds(60, 70), sunPlanetFactor = 0.01,
      planetTeeth = TeethBounds(8, 10), ringTeeth = TeethBounds(30, 34), planets = TeethBounds(3, 3))
    val pool = Enumerator.stage1Sets(cfg).toVector
    assert(pool.nonEmpty)
    pool.foreach { g =>
      assert(g.ring.toInt >= 30 && g.ring.toInt <= 34)
      assertEquals(g.sun.toInt, g.ring.toInt - 2 * g.planet.toInt)
      assert(g.sun.toInt >= 1)
    }
    // the sun-teeth range / factor don't apply: the same cfg with a sun gets nothing
    assert(Enumerator.gearSets(cfg, sunPresent = true).isEmpty)
  }

  test("NSS draws stage 1 from the sun pool and stage 2 from the sun-less pool") {
    val cfg = SearchConfig(
      topology = Topology.nssCompound, sunTeeth = TeethBounds(10, 12), planetTeeth = TeethBounds(8, 10),
      ringTeeth = TeethBounds(30, 40), planets = TeethBounds(3, 3))
    assert(Enumerator.stage1Sets(cfg).forall(g => g.sun.toInt >= 10 && g.sun.toInt <= 12))
    assert(Enumerator.stage2Sets(cfg).exists(g => g.sun.toInt > 12))
  }

  // -- NSS vs Standard (ADR 0012, 0015) -----------------------------------------

  private def pairs(t: Topology): Set[(GearSet, GearSet)] =
    val cfg = SearchConfig(
      topology = t, sunTeeth = TeethBounds(10, 30), planetTeeth = TeethBounds(8, 20),
      ringTeeth = TeethBounds(30, 100), planets = TeethBounds(3, 3), minModule = 0.3)
    Calculator.search(cfg).map(d => (d.stage1.set, d.stage2.get.set)).toSet

  test("NSS designs are a strict superset of Standard designs (no stage-2 sum rule without a sun)") {
    val std = pairs(Topology.standardCompound)
    val nss = pairs(Topology.nssCompound)
    assertEquals(std -- nss, Set.empty[(GearSet, GearSet)])
    assert((nss -- std).nonEmpty, "expected NSS to unlock designs Standard rejects")
  }

  test("NSS ratio equals Standard ratio when the stage-2 sum rule holds (the stage-2 sun is irrelevant)") {
    val a = GearSet(Teeth(20), Teeth(15), 3)
    val b = GearSet(Teeth(15), Teeth(15), 3) // (S2 + R2) % 3 == 0
    val std = RoleSolver.ratio(Topology.standardCompound, a, Some(b)).get.value
    val nss = RoleSolver.ratio(Topology.nssCompound, a, Some(b)).get.value
    assertEqualsDouble(nss, std, 1e-12)
  }
