package cpgc.constraints

import cpgc.domain.{GearSet, Ratio, Teeth}
import cpgc.search.RoleSolver
import munit.FunSuite

/** Cross-check the symbolic `Topology.isStageDecoupled` lookup against a
  * numerical probe via `RoleSolver.ratio`: for a "decoupled" tuple, varying
  * stage 2's gear counts must not change the output ratio; for a non-
  * decoupled tuple, it must. */
class TopologyDependenceSpec extends FunSuite:

  // Two distinct gear sets per stage so we can probe "varying this stage
  // changes the ratio" both ways.
  private val stage1a = GearSet(Teeth(24), Teeth(12), 3) // R=48
  private val stage1b = GearSet(Teeth(30), Teeth(9),  3) // R=48, different teeth
  private val stage2a = GearSet(Teeth(18), Teeth(15), 3) // R=48
  private val stage2b = GearSet(Teeth(30), Teeth(9),  3) // R=48, different teeth

  /** Iterate all kinematically distinct (Input, Output, Fixed) tuples for the
    * given (stages, s2Present) pair.
    *
    * Enumerates over the **logical** body set — `Body.C1` stands for the
    * shared carrier (this spec only exercises the classic shared-carrier
    * compound family). `Body.C2` is never given a pin role; it's always
    * `Free` and kinematically tied to `C1` by the default `CarrierToCarrier`
    * first-coupling. So the compound-with-S2 enumeration stays at 5 logical
    * bodies → 60 tuples, exactly as before the C1/C2 split. */
  private def allTuples(stages: Int, s2Present: Boolean): Iterator[Topology] =
    val bodies =
      if stages == 1 then Vector(Body.S1, Body.C1, Body.R1)
      else if s2Present then Vector(Body.S1, Body.C1, Body.R1, Body.S2, Body.R2)
      else Vector(Body.S1, Body.C1, Body.R1, Body.R2) // NSS
    for
      in    <- bodies.iterator
      out   <- bodies.iterator if out != in
      fixed <- bodies.iterator if fixed != in && fixed != out
    yield
      val baseRoles: Map[Body, BodyRole] =
        bodies.map { b =>
          val role =
            if      b == in    then BodyRole.Input
            else if b == out   then BodyRole.Output
            else if b == fixed then BodyRole.Fixed
            else                    BodyRole.Free
          b -> role
        }.toMap
      val withAbsent =
        if stages == 2 && !s2Present then baseRoles.updated(Body.S2, BodyRole.Absent)
        else baseRoles
      // Full compound: add the always-Free C2 (tied to C1 by CarrierToCarrier).
      val withC2 =
        if stages == 2 && s2Present then withAbsent.updated(Body.C2, BodyRole.Free)
        else withAbsent
      Topology(stages, withC2)

  private def ratiosEqual(a: Option[Ratio], b: Option[Ratio]): Boolean = (a, b) match
    case (Some(x), Some(y)) => math.abs(x.value - y.value) < 1e-9
    case (None, None)       => true
    case _                  => false

  /** A topology is "decoupled" iff varying one of the two stages' gear counts
    * leaves the output ratio unchanged. The numerical probe checks both
    * directions independently — vary stage 1 with stage 2 fixed, then vary
    * stage 2 with stage 1 fixed — and reports true if either variation is
    * a no-op. */
  private def numericallyDecoupled(t: Topology): Boolean =
    val stage2Irrelevant = ratiosEqual(
      RoleSolver.ratio(t, stage1a, Some(stage2a)),
      RoleSolver.ratio(t, stage1a, Some(stage2b))
    )
    val stage1Irrelevant = ratiosEqual(
      RoleSolver.ratio(t, stage1a, Some(stage2a)),
      RoleSolver.ratio(t, stage1b, Some(stage2a))
    )
    stage2Irrelevant || stage1Irrelevant

  test("isStageDecoupled agrees with the numerical RoleSolver probe (S2 present, 60 tuples)") {
    val tuples = allTuples(stages = 2, s2Present = true).toVector
    assertEquals(tuples.size, 60, "expected 60 tuples for compound-with-S2")
    val mismatches = tuples.filterNot(t => Topology.isStageDecoupled(t) == numericallyDecoupled(t))
    assert(mismatches.isEmpty,
      s"${mismatches.size} mismatches between symbolic and numerical, first: ${mismatches.head}")
  }

  test("isStageDecoupled agrees with the numerical RoleSolver probe (NSS, 24 tuples)") {
    val tuples = allTuples(stages = 2, s2Present = false).toVector
    assertEquals(tuples.size, 24, "expected 24 tuples for NSS")
    val mismatches = tuples.filterNot(t => Topology.isStageDecoupled(t) == numericallyDecoupled(t))
    assert(mismatches.isEmpty,
      s"${mismatches.size} mismatches between symbolic and numerical, first: ${mismatches.head}")
  }

  test("isStageDecoupled is false for single-stage topologies") {
    val tuples = allTuples(stages = 1, s2Present = false).toVector
    assertEquals(tuples.size, 6, "expected 6 single-stage tuples")
    tuples.foreach { t =>
      assert(!Topology.isStageDecoupled(t),
        s"single-stage topology should not be flagged decoupled: $t")
    }
  }

  test("the four legacy compound modes are not decoupled") {
    assert(!Topology.isStageDecoupled(Topology.standardCompound))
    assert(!Topology.isStageDecoupled(Topology.cdcpCompound))
    assert(!Topology.isStageDecoupled(Topology.nssCompound))
    assert(!Topology.isStageDecoupled(Topology.singleStage))
  }
