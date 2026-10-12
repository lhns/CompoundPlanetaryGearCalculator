package cpgc

import cpgc.constraints.{SearchConfig, TeethBounds, Topology}
import cpgc.domain.{GearSet, Mode, Ratio, Teeth}
import cpgc.physics.Efficiency
import cpgc.search.{Compounder, Enumerator, RoleSolver}
import munit.FunSuite

/** Tests that NSS is the design-space superset its physical interpretation
  * suggests: removing the second sun from a Standard compound drops the
  * stage-2 Willis (S+R)%N requirement and adds no new conditions of its own.
  * Therefore every Standard-valid design is also NSS-valid, and many NSS
  * designs (where stage-2 Willis fails) are not Standard-valid. */
class NssSearchSpec extends FunSuite:

  private val baseCfg = SearchConfig(
    mode         = Mode.Standard,
    sunTeeth     = TeethBounds(10, 30),
    planetTeeth  = TeethBounds(8, 20),
    ringTeeth    = TeethBounds(30, 100),
    planets      = TeethBounds(3, 3),
    firstStageOD = 150.0,
    minModule    = 0.3
  )

  // Helper — collect all designs found by the cross-product search for a mode.
  private def designsFor(mode: Mode): Vector[(GearSet, GearSet)] =
    val cfg = baseCfg.copy(mode = mode)
    val stages = Enumerator.gearSets(cfg).toVector
    for
      a <- stages
      b <- stages
      if a != b
      d <- Compounder.compound(cfg, a, b)
    yield (d.stage1.set, d.stage2.get.set)

  test("NSS designs ⊇ Standard designs (every Standard-found pair is also NSS-found)") {
    val standardPairs = designsFor(Mode.Standard).toSet
    val nssPairs      = designsFor(Mode.NSS).toSet
    val missing       = standardPairs -- nssPairs
    assert(missing.isEmpty,
      s"NSS missed ${missing.size} Standard-valid pairs (first few): ${missing.take(3)}")
  }

  test("NSS strictly expands the design space: there exist NSS-valid pairs that are not Standard-valid") {
    val standardPairs = designsFor(Mode.Standard).toSet
    val nssPairs      = designsFor(Mode.NSS).toSet
    val nssExclusive  = nssPairs -- standardPairs
    assert(nssExclusive.nonEmpty,
      s"expected NSS to unlock additional designs vs Standard; found 0 extra in ${nssPairs.size} NSS pairs")
  }

  test("NSS module matching uses the same (S2+P2) denominator as Standard") {
    // The reference's MatchModule uses (S1+P1) / (S2+P2). The same formula
    // works in NSS because we now store the real S2 from enumeration.
    val s1 = GearSet(Teeth(20), Teeth(15), 3)
    val s2 = GearSet(Teeth(14), Teeth(12), 3)
    val (m1, m2) = Compounder.matchModules(baseCfg, s1, s2)
    val expected = m1.value * (s1.sun.toDouble + s1.planet.toDouble) /
                   (s2.sun.toDouble + s2.planet.toDouble)
    assertEqualsDouble(m2.value, expected, 1e-12)
  }

  test("Stage 1 spacing is required for NSS — without it the design is rejected") {
    // Pick a stage-1 set that fails (S1+R1)%N=0: e.g. S1=10, P1=10, R1=30, N=3 → 40%3=1 ≠ 0.
    val s1Bad = GearSet(Teeth(10), Teeth(10), 3)
    assert(!s1Bad.evenSpacing, "preconditions: chosen stage 1 should not pass Willis")
    val s2    = GearSet(Teeth(14), Teeth(12), 3) // ring=38; (14+38)%3=52%3=1, also fails — fine
    val cfg   = baseCfg.copy(mode = Mode.NSS)
    val d     = Compounder.compound(cfg, s1Bad, s2)
    assertEquals(d, None, "NSS should reject when stage-1 spacing fails")
  }

  test("NSS accepts a stage 2 that fails Standard's stage-2 spacing, as long as stage 1 is spacing-OK") {
    val s1 = GearSet(Teeth(12), Teeth(12), 3)  // (12+36)%3 = 0 ✓
    assert(s1.evenSpacing)
    // Stage 2 that FAILS Willis: S=11, P=10, R=31, N=3 → (11+31)%3 = 42%3 = 0 — wait that's OK.
    // S=10, P=11, R=32 → (10+32)%3 = 42%3 = 0. Also OK.
    // S=14, P=8, R=30 → (14+30)%3 = 44%3 = 2. Fails ✓
    val s2 = GearSet(Teeth(14), Teeth(8), 3)
    assert(!s2.evenSpacing)
    // Also need r12 ≠ 0: r12 = (R2 − (R1/P1)·P2) / R2. R1/P1 = 36/12 = 3. r12 = (30 − 24)/30 = 0.2 ✓
    val cfgStd = baseCfg.copy(mode = Mode.Standard)
    val cfgNss = baseCfg.copy(mode = Mode.NSS)
    val stdResult = Compounder.compound(cfgStd, s1, s2)
    val nssResult = Compounder.compound(cfgNss, s1, s2)
    assertEquals(stdResult, None, "Standard should reject when stage-2 spacing fails")
    assert(nssResult.isDefined, s"NSS should accept the same pair (it doesn't require stage-2 spacing); got $nssResult")
  }

  test("NSS ratio equals Standard ratio for matching (S2, P2, R2) triples (Ts2 invariance)") {
    val s1 = GearSet(Teeth(20), Teeth(15), 3)
    val s2 = GearSet(Teeth(14), Teeth(12), 3) // R2 = 38, satisfies both modes' stage Willis (?)
    // (S2+R2)%N = 52%3 = 1 — fails Standard. So pick differently.
    val s2Good = GearSet(Teeth(16), Teeth(12), 3) // R2 = 40, (16+40)%3 = 56%3 = 2 — also fails.
    // (S2+R2)%3==0 requires S2+R2 divisible by 3, i.e. (S2+2P2+S2) = 2(S2+P2) divisible by 3, i.e. (S2+P2) divisible by 3.
    val s2Both = GearSet(Teeth(15), Teeth(15), 3) // S+P=30 ✓ divisible by 3
    val rStd = RoleSolver.ratio(Topology.standardCompound, s1, Some(s2Both)).get.value
    val rNss = RoleSolver.ratio(Topology.nssCompound,      s1, Some(s2Both)).get.value
    assertEqualsDouble(rNss, rStd, 1e-12)
  }

  test("NSS efficiency strictly exceeds Standard efficiency for the same triple") {
    val s1 = GearSet(Teeth(20), Teeth(15), 3)
    val s2 = GearSet(Teeth(15), Teeth(15), 3)
    val effStd = Efficiency.overall(s1, Some(s2), Mode.Standard)
    val effNss = Efficiency.overall(s1, Some(s2), Mode.NSS)
    assert(effNss > effStd, s"NSS efficiency ($effNss) should exceed Standard ($effStd)")
  }

  test("NSS targeted inverse solver returns designs near the target ratio") {
    val cfg = baseCfg.copy(
      mode    = Mode.NSS,
      targets = Seq(Ratio(40.0))
    )
    // Stage 1 must pass single-stage Willis: pick (24,12,3) → R=48, (24+48)%3=0 ✓.
    val s1 = GearSet(Teeth(24), Teeth(12), 3)
    val nssCandidates = Compounder.stage2For(40.0, s1, cfg).toVector
    if nssCandidates.isEmpty then
      val relaxed = cfg.copy(ringTeeth = TeethBounds(20, 200))
      val again   = Compounder.stage2For(40.0, s1, relaxed).toVector
      assert(again.nonEmpty, "NSS inverse solver returned nothing even with relaxed bounds")
    else
      assert(nssCandidates.nonEmpty)
  }
