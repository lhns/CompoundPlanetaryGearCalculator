package cpgc.web

import cpgc.Calculator
import cpgc.constraints.{Body, BodyRole, Coupling, SearchConfig, Topology}
import cpgc.domain.{GearSet, Teeth}
import cpgc.kinematics.Kinematics
import cpgc.search.Scorer.SortBy
import cpgc.search.Compounder
import munit.FunSuite
import scala.util.Try

/** Pure UI logic tests (no DOM). Tests tagged U1..U7 / N* pin down known UI
  * bugs; those still failing in production would be marked `.fail` with the
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
    buildConfig(f, 50).foreach(c => Calculator.searchSteps(c).foreach(_ => ()))

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

  test("U5a initialTopology matches mode= only as a whole parameter"):
    // observed: Topology.cdcpCompound (substring `contains("mode=cdcp")` matches `xmode=cdcp`)
    assertEquals(initialTopology("?xmode=cdcp"), Topology.standardCompound)

  test("U5b urlParam percent-decodes values"):
    // observed: Some("53%2C12")
    assertEquals(urlParam("?target=53%2C12", "target"), Some("53,12"))

  test("U5c parseRange accepts a single value as a degenerate range"):
    // observed: None
    assertEquals(parseRange("3"), Some((3, 3)))

  test("U5d ?planets=3 sets both planets bounds to 3"):
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

  private def cfgOf(f: FormValues, k: Int = 1): SearchConfig =
    buildConfig(f, k).fold(e => fail(s"expected a config, got: $e"), identity)

  test("buildConfig copies form values and the topology"):
    val c = cfgOf(base.copy(target = "100", sunPlanetFactor = "2"), 77)
    assertEquals(c.topology, Topology.cdcpCompound)
    assertEquals((c.sunTeeth.min, c.sunTeeth.max), (10, 12))
    assertEquals(c.sunPlanetFactor, 2.0)
    assertEquals(c.topPerTarget, 77)
    assertEquals(c.targets.map(_.value), Seq(100.0))

  test("buildConfig: blank sunPlanetFactor means unlimited, junk is an error, a novel topology is passed through"):
    assertEquals(cfgOf(base).sunPlanetFactor, 1e9)
    assert(buildConfig(base.copy(sunPlanetFactor = "x"), 1).isLeft)
    assert(buildConfig(base.copy(sunPlanetFactor = "0"), 1).isLeft)
    assertEquals(cfgOf(base.copy(topology = ravigneauxTopology)).topology, ravigneauxTopology)

  test("buildConfig passes ring wall, clearance and phased pinions through"):
    val c = cfgOf(base.copy(ringWall = 3.0, clearance = 0.8, phasedPinions = true))
    assertEquals((c.ringWall, c.planetClearance, c.phasedPinions), (3.0, 0.8, true))

  test("U2 typed teeth minimum below 2 is an inline error, never a throw"):
    assert(Try(buildConfig(base.copy(sunMin = 1), 50)).toOption.exists(_.isLeft))

  test("U2 zero / negative bounds are an error, never a throw"):
    assert(Try(buildConfig(base.copy(ringMin = -3, ringMax = 0), 50)).toOption.exists(_.isLeft))

  test("U2 URL ?sun=1-5 is an error, never a throw"):
    assert(Try(buildConfig(initialForm("?sun=1-5"), 50)).toOption.exists(_.isLeft))

  test("U2 planets minimum 1 is an error, never a throw"):
    assert(Try(buildConfig(base.copy(planetsMin = 1, planetsMax = 4), 50)).toOption.exists(_.isLeft))

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

  test("U3 a lone comma between digits is a decimal comma"):
    // observed: List(53.0, 12.0)
    assertEquals(targets("53,12"), Seq(53.12))

  test("U3 ';' separates targets"):
    // observed: List() (whole string fails to parse)
    assertEquals(targets("100;53,12"), Seq(100.0, 53.12))

  test("U3 whitespace separates targets"):
    // observed: List() ("100 53.12" fails to parse)
    assertEquals(targets("100 53.12"), Seq(100.0, 53.12))

  test("U3 non-finite targets never throw and are dropped"):
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

  test("the preset list has CDCP (sun-less) and no selectable Custom"):
    assertEquals(presetTopology(GearboxPreset.SunlessCdcp), Topology.sunlessCdcpCompound)
    assert(selectablePresets.contains(GearboxPreset.SunlessCdcp))
    assert(!selectablePresets.contains(GearboxPreset.Custom))
    assertEquals(topologyPreset(Topology.sunlessCdcpCompound), GearboxPreset.SunlessCdcp)

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

  test("N1 assignRole on a body that does not exist must not throw"):
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

  test("U1 a sun welded by a coupling cannot be removed: Left with a reason, topology unchanged"):
    // Ravigneaux welds C1 to S2, so S2 cannot be absent (the old expectation of a valid NSS was wrong).
    val r = setSunPresent(ravigneauxTopology, 2, false)
    assert(r.isLeft, r.toString)
    assert(r.left.exists(_.contains("Absent")), r.toString)
    assertEquals(setS2Present(ravigneauxTopology, false), ravigneauxTopology)
    assert(sunToggleDisabledReason(ravigneauxTopology, 2).isDefined)

  test("U1 toggling S2 off when C2 holds a pin role is valid and keeps the pins"):
    val t0 = assignRole(Topology.standardCompound, Body.C2, BodyRole.Output)
    val t  = setSunPresent(t0, 2, false).fold(e => fail(e), identity)
    assert(!t.s2Present)
    assertEquals(pins(t), pins(t0))

  test("sun toggles: stage 1 sun removal moves the Input pin to a free body"):
    val t = setSunPresent(Topology.standardCompound, 1, false).fold(e => fail(e), identity)
    assert(!t.s1Present && t.s2Present)
    assertEquals(t.input, Body.C1)
    assertEquals(t.fixed, Body.R1)

  test("sun toggles: both suns absent from CDCP is the sun-less CDCP"):
    val t = setSunPresent(Topology.cdcpCompound, 1, false).flatMap(setSunPresent(_, 2, false))
    assertEquals(t, Right(Topology.sunlessCdcpCompound))
    assertEquals(setSunPresent(Topology.sunlessCdcpCompound, 1, true).flatMap(setSunPresent(_, 2, true)), Right(Topology.cdcpCompound))

  test("sun toggles: no free body means a reason, not a throw"):
    // single stage: S1 in, C1 out, R1 fixed: no free body at all
    assert(setSunPresent(Topology.singleStage, 1, false).isLeft)
    assert(sunToggleDisabledReason(Topology.singleStage, 1).isDefined)
    assert(sunToggleDisabledReason(Topology.singleStage, 2).isDefined)
    assert(sunToggleDisabledReason(Topology.standardCompound, 2).isEmpty)
    assert(sunToggleDisabledReason(Topology.nssCompound, 2).isEmpty) // absent: can always come back

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

  test("U6 coupling selectors stay enabled for NSS (couplings matter there, ADR 0015)"):
    assert(!couplingSelectorsDisabled(Topology.nssCompound))
    assert(!couplingSelectorsDisabled(Topology.sunlessCdcpCompound))

  test("coupling selectors are disabled for single stage, enabled for a full compound"):
    assert(couplingSelectorsDisabled(Topology.singleStage))
    assert(!couplingSelectorsDisabled(Topology.standardCompound))

  test("U6 changing a coupling in NSS changes the topology, except welds on the absent sun"):
    val t = setCoupling(Topology.nssCompound, CouplingSlot.First, Coupling.CarrierToRing)
    assertNotEquals(t, Topology.nssCompound)
    assertEquals(t.firstCoupling, Coupling.CarrierToRing)
    // Sun 2 does not exist: the weld "Ring 1 <-> Sun 2" is greyed out and refused
    assert(couplingOptionDisabled(Topology.nssCompound, CouplingSlot.First, Coupling.RingToSun))
    assertEquals(setCoupling(Topology.nssCompound, CouplingSlot.First, Coupling.RingToSun), Topology.nssCompound)
    assert(!couplingOptionDisabled(Topology.nssCompound, CouplingSlot.First, Coupling.CarrierToRing))

  // ── U7: PlanetStack needs CarrierToCarrier ─────────────────────────────────

  private def stackedWithoutSharedCarrier(t: Topology): Boolean =
    t.stages == 2 && t.s2Present && (
      (t.firstCoupling == Coupling.PlanetStack && t.secondCoupling != Coupling.CarrierToCarrier) ||
      (t.secondCoupling == Coupling.PlanetStack && t.firstCoupling != Coupling.CarrierToCarrier)
    )

  test("U7 moving Coupling 1 away from CarrierToCarrier must not leave Coupling 2 = PlanetStack"):
    // observed: Topology(firstCoupling = RingToSun, secondCoupling = PlanetStack), a non-buildable pair
    val t = setCoupling(Topology.standardCompound, CouplingSlot.First, Coupling.RingToSun)
    assert(!stackedWithoutSharedCarrier(t), s"got ${t.firstCoupling} / ${t.secondCoupling}")

  test("U7 moving Coupling 2 away from CarrierToCarrier must not leave Coupling 1 = PlanetStack"):
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

  // ── U7 (auto-deselect) ─────────────────────────────────────────────────────

  test("U7 choosing another partner for PlanetStack deselects it: the displaced shared carrier takes its place"):
    val t = setCoupling(Topology.standardCompound, CouplingSlot.First, Coupling.RingToSun)
    assertEquals((t.firstCoupling, t.secondCoupling), (Coupling.RingToSun, Coupling.CarrierToCarrier))
    assert(!t.stacked)
    val t2 = setCoupling(Topology.standardCompound.copy(firstCoupling = Coupling.PlanetStack, secondCoupling = Coupling.CarrierToCarrier),
                         CouplingSlot.Second, Coupling.RingToSun)
    assertEquals((t2.firstCoupling, t2.secondCoupling), (Coupling.CarrierToCarrier, Coupling.RingToSun))

  test("PlanetStack is only selectable next to CarrierToCarrier; every reachable pair is buildable"):
    for
      p <- selectablePresets if presetTopology(p).stages == 2
      slot <- CouplingSlot.values
      c <- Coupling.values
    do
      val t = presetTopology(p)
      val r = trySetCoupling(t, slot, c)
      assertEquals(couplingOptionDisabled(t, slot, c), r.isLeft)
      r.foreach { t2 =>
        assert(!t2.stacked || Set(t2.firstCoupling, t2.secondCoupling).contains(Coupling.CarrierToCarrier), s"$p $slot $c")
      }

  // ── URL round trip (U5) ────────────────────────────────────────────────────

  test("urlParam: exact names, first occurrence, percent-decoding and '+'"):
    assertEquals(urlParam("?xtarget=1&target=2", "target"), Some("2"))
    assertEquals(urlParam("?target=1&target=2", "target"), Some("1"))
    assertEquals(urlParam("?target=100%3B53%2C12", "target"), Some("100;53,12"))
    assertEquals(urlParam("?t=a+b%20c%E2%82%AC", "t"), Some("a b c€"))
    assertEquals(urlParam("?t=100%", "t"), Some("100%"))

  test("percentEncode / percentDecode round-trip"):
    for s <- Seq("", "53,12", "100; 53.12", "a&b=c#d", "€ ü", "50%") do
      assertEquals(percentDecode(percentEncode(s)), s)

  test("parseRange: single value, negatives, junk"):
    assertEquals(parseRange("3"), Some((3, 3)))
    assertEquals(parseRange("-3-0"), Some((-3, 0)))
    assertEquals(parseRange(""), None)

  private val roundTripForms = Seq(
    initialForm(""),
    initialForm("?sun=1-5&planets=3&od=0&target=100%3B53%2C12"),
    initialForm("?mode=cdcp-sunless&ringwall=3.5&clearance=0.25&phased=1&spfact=2"),
    base,
    base.copy(topology = ravigneauxTopology, target = "100, 53.12", sunPlanetFactor = "1.5"),
    base.copy(topology = simpsonTopology, ringWall = 0.0, clearance = 1.25, phasedPinions = true),
    base.copy(topology = Topology.nssCompound.copy(firstCoupling = Coupling.CarrierToRing, secondCoupling = Coupling.CarrierToCarrier)),
    base.copy(topology = Topology.singleStage),
    base.copy(topology = setSunPresent(Topology.standardCompound, 1, false).toOption.get)
  )

  test("toQuery -> initialForm round-trips the whole form, topology included"):
    for f <- roundTripForms; sort <- SortBy.values.toSeq do
      val q = toQuery(f, sort)
      assertEquals(initialForm(q), f, q)
      assertEquals(initialSort(q), sort)

  test("every preset round-trips through the URL"):
    for p <- selectablePresets do
      val f = base.copy(topology = presetTopology(p))
      assertEquals(initialForm(toQuery(f, SortBy.Ratio)).topology, presetTopology(p), p.toString)

  test("URL absent / roles / couplings params build the topology"):
    assertEquals(initialTopology("?mode=cdcp&absent=S1,S2"), Topology.sunlessCdcpCompound)
    assertEquals(initialTopology("?mode=nss&absent="), Topology.standardCompound)
    // illegal combinations are skipped, not fatal
    assertEquals(initialTopology("?mode=nss&coupling1=RingToSun"), Topology.nssCompound)
    assertEquals(initialTopology("?absent=R1"), Topology.standardCompound)

  test("single-value ranges, od=0 and sun=1-5 come through the URL and fail validation inline"):
    assert(buildConfig(initialForm("?od=0"), 5).isLeft)
    assert(buildConfig(initialForm("?sun=1-5"), 5).isLeft)
    assert(buildConfig(initialForm("?planets=3"), 5).isRight)

  // ── Targets and numbers ────────────────────────────────────────────────────

  test("targets: comma + space is two targets, comma between digits is decimal, ';' and spaces separate"):
    assertEquals(targets("100, 53.12"), Seq(100.0, 53.12))
    assertEquals(targets("100,53.12"), Seq.empty[Double]) // "100.53.12" is not a number: dropped with a hint
    assertEquals(targets("100,5"), Seq(100.5))
    assertEquals(targets("100;53,12; 7"), Seq(100.0, 53.12, 7.0))
    assertEquals(targets("100,"), Seq(100.0))

  test("targets: junk and non-finite are dropped with a hint"):
    assertEquals(targets("abc 5 1e999 NaN"), Seq(5.0))
    assert(targetHint("abc 5").exists(_.contains("abc")))
    assertEquals(targetHint("5; 7"), None)

  test("parseDouble: strict decimal syntax"):
    assertEquals(parseDouble("0."), Some(0.0))
    assertEquals(parseDouble(".5"), Some(0.5))
    assertEquals(parseDouble("1e3"), Some(1000.0))
    assertEquals(parseDouble("1,000.5"), None)
    assertEquals(parseDouble("1d"), None)
    assertEquals(parseDouble("0x10"), None)
    assertEquals(parseFinite("1e999"), None)

  test("U10 typing '0.' does not rewrite the field; an external change does"):
    assert(!shouldRewrite("0.", 0.0))
    assert(!shouldRewrite("150.", 150.0))
    assert(shouldRewrite("150.", 120.0))
    assert(shouldRewrite("", 150.0))
    assert(!shouldRewrite(" 12 ", 12))
    assert(shouldRewrite("1x", 1))

  // ── Search bookkeeping ─────────────────────────────────────────────────────

  test("re-rank without searching only for a finished, untruncated, complete run"):
    assert(rerankable(done = true, truncated = false, found = 100, k = 10000))
    assert(!rerankable(done = true, truncated = false, found = 20000, k = 10000))
    assert(!rerankable(done = false, truncated = true, found = 100, k = 10000))
    assert(!rerankable(done = false, truncated = false, found = 100, k = 10000))

  test("default sort is labelled by what it does: no target means lowest ratio"):
    assertEquals(sortLabel(SortBy.BestFit, hasTargets = true), "Best fit")
    assertNotEquals(sortLabel(SortBy.BestFit, hasTargets = false), "Best fit")
    assert(sortTooltip(SortBy.BestFit, hasTargets = false).contains("ratio"))

  test("engine run to completion: re-ranking its top by another criterion equals a fresh engine for that criterion"):
    val c = cfgOf(base.copy(planetMax = 12, ringMax = 36), 10000)
    val e = Calculator.engine(c, SortBy.BestFit, 10000).run()
    assert(e.found > 0 && e.found <= 10000 && !e.truncated)
    for by <- Seq(SortBy.Ratio, SortBy.Efficiency, SortBy.Simplicity) do
      val re    = cpgc.search.Scorer.sort(e.top, by, c.targets)
      val fresh = Calculator.engine(c, by, 10000).run().top
      assertEquals(re.map(cpgc.search.Scorer.canonicalKey).toSet, fresh.map(cpgc.search.Scorer.canonicalKey).toSet)
      assertEquals(re.size, fresh.size)

  test("fmtEfficiency prints a percentage"):
    assertEquals(fmtEfficiency(0.9406), "94.1%")

  // ── Animation consistency for every preset (U8) ────────────────────────────

  test("U8 the animation (Kinematics) matches the ratio for every preset's designs"):
    val f = base.copy(sunMin = 10, sunMax = 20, planetMin = 10, planetMax = 16, ringMin = 30, ringMax = 60, planetsMin = 3, planetsMax = 4)
    for p <- selectablePresets do
      val t  = presetTopology(p)
      val c  = cfgOf(f.copy(topology = t), 1)
      val ds = Calculator.searchSteps(c).flatten.take(25).toVector
      assert(ds.nonEmpty, s"no designs for $p")
      for d <- ds do
        val (in, out) = Kinematics.io(d, Kinematics.at(d, 1.0))
        if math.abs(out) > 1e-9 then
          assertEqualsDouble(in / out, d.ratio.value, 1e-6 * math.max(1.0, math.abs(d.ratio.value)), s"$p $d")
        assertEquals(d.topology, t)
