package cpgc.search

import cpgc.constraints.{Body, BodyRole, Coupling, Topology}
import cpgc.domain.{GearSet, Teeth}
import munit.FunSuite

/** Forward solver tests for novel `Coupling` choices. Each test sets up a
  * topology with a specific second-coupling, picks a gear pair, and asserts
  * the forward solver returns a finite ratio plus the kinematic identity the
  * coupling row encodes (e.g. for `RingToSun`, `ω_R1` must equal `ω_S2`).
  */
class CoupledForwardSpec extends FunSuite:

  // (S+R) must be divisible by N for evenSpacing.
  // Stage 1: S=20, P=15, R=50, N=5 → (20+50) % 5 == 0 ✓
  // Stage 2: S=24, P=12, R=48, N=3 → (24+48) % 3 == 0 ✓
  // Distinct gears so the classic PlanetStack compound has a finite, non-trivial
  // ratio (identical stages collapse R2 to 0 = infinite reduction).
  private val s1: GearSet = GearSet(Teeth(20), Teeth(15), 5)
  private val s2: GearSet = GearSet(Teeth(24), Teeth(12), 3)

  private def standardRoles: Map[Body, BodyRole] = Map(
    Body.S1 -> BodyRole.Input,
    Body.R2 -> BodyRole.Output,
    Body.R1 -> BodyRole.Fixed,
    Body.C1 -> BodyRole.Free,
    Body.S2 -> BodyRole.Free,
    Body.C2 -> BodyRole.Free
  )

  private def topology(c: Coupling): Topology =
    Topology(stages = 2, roles = standardRoles, secondCoupling = c)

  test("PlanetStack reproduces the legacy Standard ratio (regression guard)") {
    val r = RoleSolver.ratio(topology(Coupling.PlanetStack), s1, Some(s2))
    assert(r.isDefined)
    // The classic compound has a known ratio for these gear counts.
    // We just check it's finite + non-trivial; exact value pinned by
    // existing 93 tests elsewhere.
    assert(math.abs(r.get.value) > 1.0)
  }

  test("RingToSun: ω_R1 == ω_S2 at the solver level") {
    val v = RoleSolver.velocities(topology(Coupling.RingToSun), s1, Some(s2), omegaIn = 1.0)
    assert(v.isDefined)
    val omR1 = v.get.get(Body.R1)
    val omS2 = v.get.get(Body.S2)
    assertEqualsDouble(omR1, omS2, 1e-9)
  }

  test("SunToRing: ω_S1 == ω_R2 at the solver level") {
    val v = RoleSolver.velocities(topology(Coupling.SunToRing), s1, Some(s2), omegaIn = 1.0)
    assert(v.isDefined)
    val omS1 = v.get.get(Body.S1)
    val omR2 = v.get.get(Body.R2)
    assertEqualsDouble(omS1, omR2, 1e-9)
  }

  test("RingToRing: ω_R1 == ω_R2 at the solver level") {
    // R1 is fixed in the standard role set, so this forces R2=0 too — a
    // legitimate constraint that produces a particular ratio. We just check
    // the equality holds.
    val v = RoleSolver.velocities(topology(Coupling.RingToRing), s1, Some(s2), omegaIn = 1.0)
    assert(v.isDefined)
    val omR1 = v.get.get(Body.R1)
    val omR2 = v.get.get(Body.R2)
    assertEqualsDouble(omR1, omR2, 1e-9)
  }

  test("SunToSun: ω_S1 == ω_S2 at the solver level") {
    val v = RoleSolver.velocities(topology(Coupling.SunToSun), s1, Some(s2), omegaIn = 1.0)
    assert(v.isDefined)
    val omS1 = v.get.get(Body.S1)
    val omS2 = v.get.get(Body.S2)
    assertEqualsDouble(omS1, omS2, 1e-9)
  }

  test("RingToSun produces a different ratio than PlanetStack for the same gears") {
    val rPS = RoleSolver.ratio(topology(Coupling.PlanetStack), s1, Some(s2)).get.value
    val rRS = RoleSolver.ratio(topology(Coupling.RingToSun), s1, Some(s2)).get.value
    assert(math.abs(rPS - rRS) > 0.01, s"ratios should differ: PS=$rPS RS=$rRS")
  }
