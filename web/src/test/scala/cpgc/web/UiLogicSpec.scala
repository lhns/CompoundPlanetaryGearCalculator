package cpgc.web

import cpgc.Calculator
import cpgc.constraints.{Body, BodyRole, Coupling, SearchConfig, Topology}
import cpgc.domain.{GearSet, Mode, Teeth}
import cpgc.search.Compounder
import munit.FunSuite
import scala.util.Try

/** Pure UI logic tests (no DOM). Tests tagged U1..U7 / N* pin down known UI
  * bugs; those still failing in production are marked `.fail` with the
  * observed wrong value, so they flip to red the moment the bug is fixed and
  * the marker must be removed. */
class UiLogicSpec extends FunSuite:

  import UiLogic.*

  private val base = FormValues(
    topology = Topology.cdcpCompound,
    sunMin = 10, sunMax = 12,
    planetMin = 10, planetMax = 11,
    ringMin = 30, ringMax = 40,
    planetsMin = 3, planetsMax = 3,
    od = 150.0, minModule = 0.5,
    sunPlanetFactor = "", target = ""
  )

  private def pins(t: Topology) = (t.input, t.output, t.fixed)

  /** Build the config (whatever the builder returns now or later: config or
    * `Right(config)`) and run the whole search over it. */
  private def buildAndSearch(f: FormValues): Unit =
    (buildConfig(f, 50): Any) match
      case c: SearchConfig        => Calculator.searchSteps(c).foreach(_ => ())
      case Right(c: SearchConfig) => Calculator.searchSteps(c).foreach(_ => ())
      case _                      => ()

  // ── parseDouble / urlParam / parseRange ────────────────────────────────────

  test("parseDouble accepts decimal comma, trims, rejects junk"):
    assertEquals(parseDouble("1,5"), Some(1.5))
    assertEquals(parseDouble(" 2.25 "), Some(2.25))
    assertEquals(parseDouble("abc"), None)
    assertEquals(parseDouble(""), None)

  test("urlParam reads params with or without leading '?'"):
    assertEquals(urlParam("?a=1&b=2", "b"), Some("2"))
    assertEquals(urlParam("a=1&b=2", "a"), Some("1"))
    assertEquals(urlParam("?a=1", "zzz"), None)
    assertEquals(urlParam("", "a"), None)

  test("parseRange parses lo-hi and rejects malformed"):
    assertEquals(parseRange("10-40"), Some((10, 40)))
    assertEquals(parseRange("10-"), None)
    assertEquals(parseRange("a-b"), None)
    assertEquals(parseRange("1-2-3"), None)

  // ── initialTopology ────────────────────────────────────────────────────────

  test("initialTopology maps ?mode= to the canonical topologies"):
    assertEquals(initialTopology(""), Topology.standardCompound)
    assertEquals(initialTopology("?mode=single"), Topology.singleStage)
    assertEquals(initialTopology("?mode=cdcp"), Topology.cdcpCompound)
    assertEquals(initialTopology("?mode=nss"), Topology.nssCompound)

  test("initialTopology honours coupling params and falls back on invalid pairs"):
    val t = initialTopology("?coupling1=CarrierToSun&coupling2=RingToRing")
    assertEquals((t.firstCoupling, t.secondCoupling), (Coupling.CarrierToSun, Coupling.RingToRing))
    assertEquals(initialTopology("?coupling1=RingToRing&coupling2=RingToRing"), Topology.standardCompound)

  test("initialTopology applies input/output/fixed overrides"):
    val t = initialTopology("?input=C1&output=R2&fixed=S1")
    assertEquals(pins(t), (Body.C1, Body.R2, Body.S1))

  test("U5a initialTopology matches mode= only as a whole parameter".fail):
    // observed: Topology.cdcpCompound (substring `contains("mode=cdcp")` matches `xmode=cdcp`)
    assertEquals(initialTopology("?xmode=cdcp"), Topology.standardCompound)

  test("U5b urlParam percent-decodes values".fail):
    // observed: Some("53%2C12")
    assertEquals(urlParam("?target=53%2C12", "target"), Some("53,12"))

  test("U5c parseRange accepts a single value as a degenerate range".fail):
    // observed: None
    assertEquals(parseRange("3"), Some((3, 3)))

  test("U5d ?planets=3 sets both planets bounds to 3".fail):
    // observed: (3,4) defaults, the single value is ignored
    val f = initialForm("?planets=3")
    assertEquals((f.planetsMin, f.planetsMax), (3, 3))

  test("initialForm reads ranges, od, minmod, target from the query"):
    val f = initialForm("?sun=12-30&od=200&minmod=0.8&target=100&spfact=2")
    assertEquals((f.sunMin, f.sunMax), (12, 30))
    assertEquals((f.od, f.minModule, f.target, f.sunPlanetFactor), (200.0, 0.8, "100", "2"))

  // ── bounds / buildConfig ───────────────────────────────────────────────────

  test("bounds normalises an inverted pair"):
    val b = bounds(40, 10)
    assertEquals((b.min, b.max), (10, 40))

  test("buildConfig copies form values and the topology"):
    val c = buildConfig(base.copy(target = "100", sunPlanetFactor = "2"), 77)
    assertEquals(c.topology, Topology.cdcpCompound)
    assertEquals((c.sunTeeth.min, c.sunTeeth.max), (10, 12))
    assertEquals(c.sunPlanetFactor, 2.0)
    assertEquals(c.topPerTarget, 77)
    assertEquals(c.targets.map(_.value), Seq(100.0))

  test("buildConfig: blank / junk sunPlanetFactor means unlimited, a novel topology is passed through"):
    assertEquals(buildConfig(base, 1).sunPlanetFactor, 1e9)
    assertEquals(buildConfig(base.copy(sunPlanetFactor = "x"), 1).sunPlanetFactor, 1e9)
    assertEquals(buildConfig(base.copy(topology = ravigneauxTopology), 1).topology, ravigneauxTopology)

  test("U2 typed teeth minimum below 2 must not throw while building the config".fail):
    // observed: IllegalArgumentException "min teeth must be >= 2, got 1"
    assert(Try(buildConfig(base.copy(sunMin = 1), 50)).isSuccess)

  test("U2 zero / negative bounds must not throw".fail):
    // observed: IllegalArgumentException from TeethBounds
    assert(Try(buildConfig(base.copy(ringMin = -3, ringMax = 0), 50)).isSuccess)

  test("U2 URL ?sun=1-5 must not throw while building the config".fail):
    // observed: IllegalArgumentException "min teeth must be >= 2, got 1"
    assert(Try(buildConfig(initialForm("?sun=1-5"), 50)).isSuccess)

  test("U2 planets minimum 1 must not throw".fail):
    // observed: IllegalArgumentException from TeethBounds
    assert(Try(buildConfig(base.copy(planetsMin = 1, planetsMax = 4), 50)).isSuccess)

  // ── U3: target parsing ─────────────────────────────────────────────────────
  // Rule under test: ';' or whitespace separates targets; a comma between
  // digits (no whitespace) is a decimal comma.

  private def targets(s: String): Seq[Double] = parseTargets(s).map(_.value)

  test("parseTargets: single and blank"):
    assertEquals(targets("53.12"), Seq(53.12))
    assertEquals(targets(""), Seq.empty[Double])
    assertEquals(targets("  "), Seq.empty[Double])

  test("parseTargets: multiple targets separated by comma + space"):
    assertEquals(targets("100, 53.12"), Seq(100.0, 53.12))

  test("U3 a lone comma between digits is a decimal comma".fail):
    // observed: List(53.0, 12.0)
    assertEquals(targets("53,12"), Seq(53.12))

  test("U3 ';' separates targets".fail):
    // observed: List() (whole string fails to parse)
    assertEquals(targets("100;53,12"), Seq(100.0, 53.12))

  test("U3 whitespace separates targets".fail):
    // observed: List() ("100 53.12" fails to parse)
    assertEquals(targets("100 53.12"), Seq(100.0, 53.12))

  test("U3 non-finite targets never throw and are dropped".fail):
    // observed: IllegalArgumentException "Ratio must be finite, got NaN" (also Infinity, 1e999)
    for s <- Seq("NaN", "Infinity", "-Infinity", "1e999", "100, 1e999") do
      val r = Try(parseTargets(s))
      assert(r.isSuccess, s"parseTargets($s) threw ${r.failed.toOption}")
      assert(r.get.forall(_.value.isFinite))

  // ── U4: OD / min module ────────────────────────────────────────────────────

  for (name, od) <- Seq("zero" -> 0.0, "negative" -> -5.0, "NaN" -> Double.NaN, "Infinity" -> Double.PositiveInfinity) do
    test(s"U4 OD $name must not throw anywhere in the search"):
      val r = Try(buildAndSearch(base.copy(od = od)))
      assert(r.isSuccess, s"threw ${r.failed.toOption}")

  for (name, mm) <- Seq("zero" -> 0.0, "negative" -> -1.0, "NaN" -> Double.NaN, "Infinity" -> Double.PositiveInfinity) do
    test(s"U4 min module $name must not throw anywhere in the search"):
      val r = Try(buildAndSearch(base.copy(minModule = mm)))
      assert(r.isSuccess, s"threw ${r.failed.toOption}")

  // ── Presets ────────────────────────────────────────────────────────────────

  test("preset <-> topology round-trips for every non-Custom preset"):
    for p <- GearboxPreset.values if p != GearboxPreset.Custom do
      assertEquals(topologyPreset(presetTopology(p)), p)

  test("an edited preset topology is Custom"):
    assertEquals(topologyPreset(Topology.standardCompound.copy(secondCoupling = Coupling.SunToSun)), GearboxPreset.Custom)

  // ── Stage count ────────────────────────────────────────────────────────────

  test("setStageCount switches 1 <-> 2 and keeps a compound as is"):
    assertEquals(setStageCount(Topology.standardCompound, 1), Topology.singleStage)
    assertEquals(setStageCount(Topology.singleStage, 2), Topology.standardCompound)
    assertEquals(setStageCount(ravigneauxTopology, 2), ravigneauxTopology)
    assertEquals(setStageCount(ravigneauxTopology, 3), ravigneauxTopology)

  // ── assignRole ─────────────────────────────────────────────────────────────

  test("assignRole swaps with the previous holder and keeps couplings, for every present body/role"):
    for
      p    <- GearboxPreset.values if p != GearboxPreset.Custom
      t     = presetTopology(p)
      (b, r0) <- t.roles.toList if r0 != BodyRole.Absent
      role <- Seq(BodyRole.Input, BodyRole.Output, BodyRole.Fixed) // the pickers never assign Free
    do
      val r = assignRole(t, b, role)
      assertEquals(r.roles(b), role, s"$p: $b -> $role")
      assertEquals((r.firstCoupling, r.secondCoupling), (t.firstCoupling, t.secondCoupling))
      assertEquals(r.roles.keySet, t.roles.keySet)

  test("N1 assignRole on an Absent body must not throw"):
    assert(Try(assignRole(Topology.nssCompound, Body.S2, BodyRole.Input)).isSuccess)

  test("N1 assignRole on a body that does not exist must not throw".fail):
    // observed: NoSuchElementException: key not found: R2
    assert(Try(assignRole(Topology.singleStage, Body.R2, BodyRole.Input)).isSuccess)

  // ── U1: setS2Present ───────────────────────────────────────────────────────

  test("U1 Standard -> NSS toggle yields a valid NSS topology and keeps pins/couplings"):
    val t = setS2Present(Topology.standardCompound, false)
    assert(!t.s2Present)
    assertEquals(pins(t), pins(Topology.standardCompound))
    assertEquals((t.firstCoupling, t.secondCoupling), (Coupling.CarrierToCarrier, Coupling.PlanetStack))

  test("U1 NSS -> Standard toggle yields a valid compound"):
    assertEquals(setS2Present(Topology.nssCompound, true), Topology.standardCompound)

  test("U1 Standard -> NSS -> Standard round-trips"):
    val t = Topology.standardCompound
    assertEquals(setS2Present(setS2Present(t, false), true), t)

  test("U1 CDCP -> NSS keeps the carrier input"):
    val t = setS2Present(Topology.cdcpCompound, false)
    assert(!t.s2Present)
    assertEquals(t.input, Body.C1)

  test("U1 toggling S2 off when C2 holds a pin role yields a valid topology".fail):
    // observed: IllegalArgumentException (C2 Output survives into NSS)
    val t = setS2Present(ravigneauxTopology, false)
    assert(!t.s2Present)
    assertEquals((t.input, t.fixed), (Body.S1, Body.R1))

  test("U1 toggling S2 off when S2 holds a pin role yields a valid topology"):
    val withS2Input = assignRole(Topology.standardCompound, Body.S2, BodyRole.Input)
    val t = setS2Present(withS2Input, false)
    assert(!t.s2Present)

  test("setS2Present is a no-op for single stage and when already in the requested state"):
    assertEquals(setS2Present(Topology.singleStage, false), Topology.singleStage)
    assertEquals(setS2Present(Topology.singleStage, true), Topology.singleStage)
    assertEquals(setS2Present(Topology.standardCompound, true), Topology.standardCompound)
    assertEquals(setS2Present(Topology.nssCompound, false), Topology.nssCompound)

  // ── U6: NSS ignores couplings ──────────────────────────────────────────────

  test("U6 coupling selectors are disabled for NSS".fail):
    // observed: false (only `stages != 2` is checked)
    assert(couplingSelectorsDisabled(Topology.nssCompound))

  test("coupling selectors are disabled for single stage, enabled for a full compound"):
    assert(couplingSelectorsDisabled(Topology.singleStage))
    assert(!couplingSelectorsDisabled(Topology.standardCompound))

  test("U6 changing a coupling in NSS is a no-op".fail):
    // observed: a Custom topology with firstCoupling = RingToSun that RoleSolver ignores
    for slot <- CouplingSlot.values do
      val c = if slot == CouplingSlot.First then Coupling.RingToSun else Coupling.RingToRing
      assertEquals(setCoupling(Topology.nssCompound, slot, c), Topology.nssCompound)

  // ── U7: PlanetStack needs CarrierToCarrier ─────────────────────────────────

  private def stackedWithoutSharedCarrier(t: Topology): Boolean =
    t.stages == 2 && t.s2Present && (
      (t.firstCoupling == Coupling.PlanetStack && t.secondCoupling != Coupling.CarrierToCarrier) ||
      (t.secondCoupling == Coupling.PlanetStack && t.firstCoupling != Coupling.CarrierToCarrier)
    )

  test("U7 moving Coupling 1 away from CarrierToCarrier must not leave Coupling 2 = PlanetStack".fail):
    // observed: Topology(firstCoupling = RingToSun, secondCoupling = PlanetStack), a non-buildable pair
    val t = setCoupling(Topology.standardCompound, CouplingSlot.First, Coupling.RingToSun)
    assert(!stackedWithoutSharedCarrier(t), s"got ${t.firstCoupling} / ${t.secondCoupling}")

  test("U7 moving Coupling 2 away from CarrierToCarrier must not leave Coupling 1 = PlanetStack".fail):
    // observed: Topology(firstCoupling = PlanetStack, secondCoupling = RingToSun)
    val start = Topology.standardCompound.copy(firstCoupling = Coupling.PlanetStack, secondCoupling = Coupling.CarrierToCarrier)
    val t = setCoupling(start, CouplingSlot.Second, Coupling.RingToSun)
    assert(!stackedWithoutSharedCarrier(t), s"got ${t.firstCoupling} / ${t.secondCoupling}")

  test("couplingOptionDisabled: no duplicate weld, PlanetStack only next to CarrierToCarrier"):
    val std = Topology.standardCompound // C2C / PlanetStack
    assert(couplingOptionDisabled(std, CouplingSlot.First, Coupling.PlanetStack))
    assert(couplingOptionDisabled(std, CouplingSlot.Second, Coupling.CarrierToCarrier))
    assert(!couplingOptionDisabled(std, CouplingSlot.First, Coupling.CarrierToCarrier))
    val rav = ravigneauxTopology // CarrierToSun / RingToRing
    assert(couplingOptionDisabled(rav, CouplingSlot.First, Coupling.PlanetStack))
    assert(couplingOptionDisabled(rav, CouplingSlot.Second, Coupling.PlanetStack))

  // Stacked-pinion handling in the search core must treat PlanetStack in either slot.
  // (S+R) is divisible by 3 for n3 (60) and by 4 for n4 (84), so spacing never
  // rejects them and only the planet-count guard can. n4 differs from n3 so the
  // ratio is not degenerate (identical stages give no design at all).
  private val n3 = GearSet(Teeth(20), Teeth(10), 3)
  private val n4 = GearSet(Teeth(30), Teeth(12), 4)

  private def cfgFor(t: Topology) =
    SearchConfig(topology = t, minModule = 0.01, firstStageOD = 150.0)

  test("stacked planets with different planet counts are rejected (PlanetStack in slot 2)"):
    assertEquals(Compounder.compound(cfgFor(Topology.standardCompound), n3, n4), None)

  test("U7 stacked planets with different planet counts are rejected (PlanetStack in slot 1)"):
    val t = Topology.standardCompound.copy(firstCoupling = Coupling.PlanetStack, secondCoupling = Coupling.CarrierToCarrier)
    assertEquals(Compounder.compound(cfgFor(t), n3, n4), None)

  test("U7 stacked-pinion module matching applies with PlanetStack in slot 1"):
    val t = Topology.standardCompound.copy(firstCoupling = Coupling.PlanetStack, secondCoupling = Coupling.CarrierToCarrier)
    val s2 = GearSet(Teeth(30), Teeth(12), 3)
    val d = Compounder.compound(cfgFor(t), n3, s2)
    val d1 = d.getOrElse(fail("expected a design"))
    val expectedM2 = d1.stage1.module.value * (20.0 + 10.0) / (30.0 + 12.0)
    assertEqualsDouble(d1.stage2.get.module.value, expectedM2, 1e-9)
