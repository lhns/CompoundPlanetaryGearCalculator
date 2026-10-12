package cpgc.search

import cpgc.constraints.{Body, BodyRole, Coupling, Topology}
import cpgc.domain.{GearSet, Teeth}
import munit.FunSuite

/** Forward-solver coverage for independent-carrier compounds — coupling pairs
  * where the two carriers are *not* welded together (so `Body.C1` and
  * `Body.C2` are distinct kinematic unknowns). These are the Ravigneaux /
  * Simpson families unlocked by the C1/C2 split.
  */
class IndependentCarrierSpec extends FunSuite:

  // (S+R)%N == 0 for both stages so they're even-spaced.
  private val s1: GearSet = GearSet(Teeth(20), Teeth(15), 5) // R=50, (20+50)%5=0
  private val s2: GearSet = GearSet(Teeth(24), Teeth(12), 3) // R=48, (24+48)%3=0

  /** A compound topology with the two couplings + an explicit role tuple. */
  private def topology(
      first: Coupling, second: Coupling,
      input: Body, output: Body, fixed: Body
  ): Topology =
    val pinned = Map(input -> BodyRole.Input, output -> BodyRole.Output, fixed -> BodyRole.Fixed)
    val roles = Vector(Body.S1, Body.C1, Body.R1, Body.S2, Body.C2, Body.R2)
      .map(b => b -> pinned.getOrElse(b, BodyRole.Free)).toMap
    Topology(stages = 2, roles = roles, firstCoupling = first, secondCoupling = second)

  // Ravigneaux-family layout: C1≡S2 weld + R1≡R2 weld. With R1 fixed, R2 is
  // also fixed (rings welded), so the output must be C2 — not R2.
  private val ravigneaux =
    topology(Coupling.CarrierToSun, Coupling.RingToRing,
             input = Body.S1, output = Body.C2, fixed = Body.R1)

  test("Ravigneaux-family {CarrierToSun, RingToRing} solves to a finite ratio") {
    val r = RoleSolver.ratio(ravigneaux, s1, Some(s2))
    assert(r.isDefined, "expected a finite ratio for the Ravigneaux-family compound")
    assert(r.get.value.isFinite && r.get.value != 0.0)
  }

  test("CarrierToSun weld: ω_C1 == ω_S2 at the solver level") {
    val v = RoleSolver.velocities(ravigneaux, s1, Some(s2), omegaIn = 1.0)
    assert(v.isDefined)
    assertEqualsDouble(v.get.get(Body.C1), v.get.get(Body.S2), 1e-9)
  }

  test("RingToRing weld: ω_R1 == ω_R2 at the solver level") {
    val v = RoleSolver.velocities(ravigneaux, s1, Some(s2), omegaIn = 1.0)
    assert(v.isDefined)
    assertEqualsDouble(v.get.get(Body.R1), v.get.get(Body.R2), 1e-9)
  }

  test("Simpson-family {CarrierToRing, SunToSun}: both welds hold, carriers independent") {
    // C1≡R2 and S1≡S2. C1 and C2 are NOT welded — free to differ.
    val t = topology(Coupling.CarrierToRing, Coupling.SunToSun,
                     input = Body.S1, output = Body.C2, fixed = Body.R1)
    val v = RoleSolver.velocities(t, s1, Some(s2), omegaIn = 1.0)
    assert(v.isDefined)
    assertEqualsDouble(v.get.get(Body.C1), v.get.get(Body.R2), 1e-9, "C1≡R2 weld")
    assertEqualsDouble(v.get.get(Body.S1), v.get.get(Body.S2), 1e-9, "S1≡S2 weld")
  }

  test("independent-carrier compound differs from the classic compound") {
    val classic = RoleSolver.ratio(
      topology(Coupling.CarrierToCarrier, Coupling.PlanetStack,
               input = Body.S1, output = Body.R2, fixed = Body.R1),
      s1, Some(s2)
    )
    val rav = RoleSolver.ratio(ravigneaux, s1, Some(s2))
    (classic, rav) match
      case (Some(a), Some(b)) =>
        assert(math.abs(a.value - b.value) > 0.01,
          s"ratios should differ: classic=${a.value} ravigneaux=${b.value}")
      case _ => fail(s"both topologies should yield a ratio: classic=$classic ravigneaux=$rav")
  }
