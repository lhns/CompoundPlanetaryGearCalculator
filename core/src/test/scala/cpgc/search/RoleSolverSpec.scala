package cpgc.search

import cpgc.constraints.{Body, BodyRole, Coupling, Topology}
import cpgc.domain.{GearSet, Ratio, Teeth}
import munit.FunSuite
import scala.util.Random

class RoleSolverSpec extends FunSuite:

  private val Seed = 42L

  private def randomSunPlanetRing(rng: Random): GearSet =
    val s = rng.between(8, 60)
    val p = rng.between(8, 30)
    GearSet(Teeth(s), Teeth(p), 3)

  // The N-version cross-check against an independent superposition-method
  // solver lives in `cpgc.verify.TabularSolverSpec`.

  test("forward solver matches 1 + R/S for the single stage — 100 random gear sets") {
    val rng = new Random(Seed + 3)
    val topology = Topology.singleStage
    for _ <- 1 to 100 do
      val s = randomSunPlanetRing(rng)
      val expected = Ratio(1.0 + s.ring.toDouble / s.sun.toDouble)
      val actual   = RoleSolver.ratio(topology, s, None).getOrElse(fail(s"None for single $s"))
      assert(math.abs(expected.value - actual.value) < 1e-9,
        s"single stage $s: expected ${expected.value}, got ${actual.value}")
  }

  test("hand-derived: Standard with S1=20/P1=15/R1=50, S2=22/P2=14/R2=50 gives ratio 52.5") {
    val s1 = GearSet(Teeth(20), Teeth(15), 3)
    val s2 = GearSet(Teeth(22), Teeth(14), 3)
    val r  = RoleSolver.ratio(Topology.standardCompound, s1, Some(s2)).get.value
    assertEqualsDouble(r, 52.5, 1e-9)
  }

  test("hand-derived: CDCP with same gears gives ratio 15") {
    val s1 = GearSet(Teeth(20), Teeth(15), 3)
    val s2 = GearSet(Teeth(22), Teeth(14), 3)
    val r  = RoleSolver.ratio(Topology.cdcpCompound, s1, Some(s2)).get.value
    assertEqualsDouble(r, 15.0, 1e-9)
  }

  test("a kinematically singular Standard pair (Tp1·Tr2 = Tr1·Tp2 gives zero output) has no ratio") {
    val a = GearSet(Teeth(24), Teeth(12), 3) // ring=48
    val b = GearSet(Teeth(48), Teeth(24), 3) // ring=96
    assertEquals(RoleSolver.ratio(Topology.standardCompound, a, Some(b)), None)
  }

  test("hand-derived: Single stage S=20/P=15/R=50 gives ratio 3.5") {
    val s = GearSet(Teeth(20), Teeth(15), 3)
    val r = RoleSolver.ratio(Topology.singleStage, s, None).get.value
    assertEqualsDouble(r, 3.5, 1e-9)
  }

  test("novel tuple: single-stage ring-input + sun-output + carrier-fixed (overdrive)") {
    // For a single planetary with S=20, R=50: Willis is 20 ωS + 50 ωR − 70 ωC = 0.
    // Pin ωR = 1, ωC = 0  ⇒  20 ωS + 50 = 0  ⇒  ωS = −2.5.
    // Ratio = ω_in / ω_out = 1 / (−2.5) = −0.4 (overdrive with sign flip).
    val s = GearSet(Teeth(20), Teeth(15), 3)
    val novel = Topology(1, Map(
      Body.R1 -> BodyRole.Input,
      Body.S1 -> BodyRole.Output,
      Body.C1 -> BodyRole.Fixed
    ))
    val r = RoleSolver.ratio(novel, s, None).get.value
    assertEqualsDouble(r, -0.4, 1e-9)
  }

  test("novel tuple: compound carrier-input + sun1-output + ring1-fixed (single-stage equivalent)") {
    // Compound with the carrier as input and sun-1 as output, ring-1 grounded,
    // means stage 2 is along for the ride (S2 + R2 free). The reachable ratio
    // is the same as a single-stage with carrier in / sun out / ring fixed.
    val s1 = GearSet(Teeth(20), Teeth(15), 3)
    val s2 = GearSet(Teeth(22), Teeth(14), 3)
    val novel = Topology(2, Map(
      Body.C1 -> BodyRole.Input,
      Body.S1 -> BodyRole.Output,
      Body.R1 -> BodyRole.Fixed,
      Body.S2 -> BodyRole.Free,
      Body.C2 -> BodyRole.Free,
      Body.R2 -> BodyRole.Free
    ))
    // From Willis stage 1: 20 ωS1 + 50·0 - 70·1 = 0 ⇒ ωS1 = 3.5.
    // Ratio = 1/3.5 ≈ 0.2857.
    val r = RoleSolver.ratio(novel, s1, Some(s2)).get.value
    assertEqualsDouble(r, 1.0 / 3.5, 1e-9)
  }

  test("solver returns None for a kinematically degenerate (input == fixed) ... rejected at Topology") {
    // Topology requires distinct Input/Output/Fixed, so this is caught at
    // construction. Verify the require triggers.
    intercept[IllegalArgumentException] {
      Topology(1, Map(
        Body.S1 -> BodyRole.Input,
        Body.C1 -> BodyRole.Output,
        Body.R1 -> BodyRole.Input   // two Inputs
      ))
    }
  }

  test("solver is total: a 2-stage topology with no stage-2 gear set returns None (no throw)") {
    // Happens transiently in the UI when the topology flips to compound before
    // a fresh compound design arrives. The solver must be total — `None`, not
    // an exception.
    val s1 = GearSet(Teeth(20), Teeth(15), 3)
    assertEquals(RoleSolver.ratio(Topology.standardCompound, s1, None), None)
    assertEquals(RoleSolver.velocities(Topology.standardCompound, s1, None, 1.0), None)
  }

  // ── Textbook golden oracles ────────────────────────────────────────────────
  // The three published single-stage closed forms (see
  // docs/verification/correctness-strategy.md §2.1). The ring-fixed form is
  // already covered by the "ratio 3.5" test above; here are the other two.

  test("textbook golden: single-stage sun-fixed, ring in, carrier out → i = 1 + Ns/Nr") {
    // S=20, R=50  ⇒  i = 1 + 20/50 = 1.4
    val s = GearSet(Teeth(20), Teeth(15), 3)
    val t = Topology(1, Map(
      Body.R1 -> BodyRole.Input,
      Body.C1 -> BodyRole.Output,
      Body.S1 -> BodyRole.Fixed
    ))
    assertEqualsDouble(RoleSolver.ratio(t, s, None).get.value, 1.4, 1e-9)
  }

  test("textbook golden: single-stage carrier-fixed, sun in, ring out → i = −Nr/Ns") {
    // S=20, R=50  ⇒  i = −50/20 = −2.5
    val s = GearSet(Teeth(20), Teeth(15), 3)
    val t = Topology(1, Map(
      Body.S1 -> BodyRole.Input,
      Body.R1 -> BodyRole.Output,
      Body.C1 -> BodyRole.Fixed
    ))
    assertEqualsDouble(RoleSolver.ratio(t, s, None).get.value, -2.5, 1e-9)
  }

  test("hand-worked compound golden: Standard S1=24/P1=12/R1=48, S2=20/P2=16/R2=52") {
    // Standard compound, planets stacked, carriers shared. By hand:
    //   Willis 1, R1 fixed, S1 = 1  ⇒  ωC = Ts1/(Ts1+Tr1) = 24/72 = 1/3.
    //   ωR2 = ωC·(Tp1·Tr2 − Tr1·Tp2)/(Tp1·Tr2)
    //       = (1/3)·(12·52 − 48·16)/(12·52)
    //       = (1/3)·(624 − 768)/624 = (1/3)·(−144/624) = −0.076923077
    //   ratio = 1/ωR2 = −13.0
    val s1 = GearSet(Teeth(24), Teeth(12), 3)
    val s2 = GearSet(Teeth(20), Teeth(16), 3)
    val r  = RoleSolver.ratio(Topology.standardCompound, s1, Some(s2)).get.value
    assertEqualsDouble(r, -13.0, 1e-9)
  }

  // ── Self-consistency residual check ────────────────────────────────────────
  // Independently re-derive every physical constraint equation and confirm the
  // solved velocity vector satisfies it. This catches sign / index errors in
  // RoleSolver's matrix assembly directly — including the case where two such
  // errors cancel in the final ratio but not in the intermediate velocities.

  test("self-consistency: solved velocities satisfy every constraint equation") {
    val rng = new Random(Seed + 7)
    val ε   = 1e-9

    def standard(second: Coupling): Topology = Topology.standardCompound.copy(secondCoupling = second)
    val topologies = Vector(
      Topology.standardCompound, Topology.cdcpCompound,
      standard(Coupling.RingToSun), standard(Coupling.SunToRing),
      standard(Coupling.RingToRing), standard(Coupling.SunToSun)
    )

    var checked = 0
    for _ <- 1 to 400 do
      val s1 = randomSunPlanetRing(rng)
      val s2 = randomSunPlanetRing(rng)
      for topo <- topologies do
        RoleSolver.velocities(topo, s1, Some(s2), omegaIn = 1.0).foreach { v =>
          checked += 1
          val (ts1, tp1, tr1) = (s1.sun.toDouble, s1.planet.toDouble, s1.ring.toDouble)
          val (ts2, tp2, tr2) = (s2.sun.toDouble, s2.planet.toDouble, s2.ring.toDouble)
          // Willis stage 1
          assert(math.abs(ts1*v.omegaS1 + tr1*v.omegaR1 - (ts1+tr1)*v.omegaC1) < ε,
            s"Willis-1 residual, topo=$topo s1=$s1 s2=$s2")
          // Willis stage 2
          assert(math.abs(ts2*v.omegaS2 + tr2*v.omegaR2 - (ts2+tr2)*v.omegaC2) < ε,
            s"Willis-2 residual, topo=$topo s1=$s1 s2=$s2")
          // Pin rows
          assertEqualsDouble(v.get(topo.input), 1.0, ε, s"input pin, topo=$topo")
          assertEqualsDouble(v.get(topo.fixed), 0.0, ε, s"fixed pin, topo=$topo")
          // Coupling rows — re-derived independently from RoleSolver.
          for c <- Seq(topo.firstCoupling, topo.secondCoupling) do
            c match
              case Coupling.PlanetStack =>
                val wp1 = v.omegaC1 - (v.omegaS1 - v.omegaC1) * ts1 / tp1
                val wp2 = v.omegaC2 - (v.omegaS2 - v.omegaC2) * ts2 / tp2
                assert(math.abs(wp1 - wp2) < ε, s"PlanetStack residual, topo=$topo s1=$s1 s2=$s2")
              case other =>
                val (ba, bb) = Coupling.bodyPair(other).get
                assert(math.abs(v.get(ba) - v.get(bb)) < ε,
                  s"$other weld residual, topo=$topo s1=$s1 s2=$s2")
        }
    assert(checked > 1000, s"expected >1000 self-consistency checks, got $checked")
  }
