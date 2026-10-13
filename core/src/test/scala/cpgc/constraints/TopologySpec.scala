package cpgc.constraints

import cpgc.domain.Mode
import munit.FunSuite

class TopologySpec extends FunSuite:

  test("fromMode followed by toLegacyMode round-trips for all four legacy modes") {
    for m <- Mode.values do
      val t  = Topology.fromMode(m)
      val m2 = Topology.toLegacyModeOpt(t)
      assertEquals(m2, Some(m), s"$m did not round-trip via $t")
  }

  test("constructor rejects topologies with two Input bodies") {
    intercept[IllegalArgumentException] {
      Topology(2, Map(
        Body.S1 -> BodyRole.Input,
        Body.C1 -> BodyRole.Input,
        Body.R1 -> BodyRole.Fixed,
        Body.S2 -> BodyRole.Output,
        Body.C2 -> BodyRole.Free,
        Body.R2 -> BodyRole.Free
      ))
    }
  }

  test("constructor rejects topologies with zero Input bodies") {
    intercept[IllegalArgumentException] {
      Topology(2, Map(
        Body.S1 -> BodyRole.Free,
        Body.C1 -> BodyRole.Free,
        Body.R1 -> BodyRole.Fixed,
        Body.S2 -> BodyRole.Output,
        Body.C2 -> BodyRole.Free,
        Body.R2 -> BodyRole.Free
      ))
    }
  }

  test("constructor rejects topologies missing a Fixed body") {
    intercept[IllegalArgumentException] {
      Topology(2, Map(
        Body.S1 -> BodyRole.Input,
        Body.C1 -> BodyRole.Free,
        Body.R1 -> BodyRole.Free,
        Body.S2 -> BodyRole.Output,
        Body.C2 -> BodyRole.Free,
        Body.R2 -> BodyRole.Free
      ))
    }
  }

  test("constructor rejects topologies missing an Output body") {
    intercept[IllegalArgumentException] {
      Topology(2, Map(
        Body.S1 -> BodyRole.Input,
        Body.C1 -> BodyRole.Free,
        Body.R1 -> BodyRole.Fixed,
        Body.S2 -> BodyRole.Free,
        Body.C2 -> BodyRole.Free,
        Body.R2 -> BodyRole.Free
      ))
    }
  }

  test("constructor rejects Absent on carriers and rings (only suns may be Absent)") {
    for b <- Seq(Body.C2, Body.R2) do
      intercept[IllegalArgumentException] {
        Topology(2, Map(
          Body.S1 -> BodyRole.Input,
          Body.C1 -> BodyRole.Output,
          Body.R1 -> BodyRole.Fixed,
          Body.S2 -> BodyRole.Free,
          Body.C2 -> BodyRole.Free,
          Body.R2 -> BodyRole.Free
        ).updated(b, BodyRole.Absent))
      }
  }

  test("S1 may be Absent when it is not pinned and no coupling welds it") {
    val t = Topology(2, Map(
      Body.S1 -> BodyRole.Absent,
      Body.C1 -> BodyRole.Input,
      Body.R1 -> BodyRole.Fixed,
      Body.S2 -> BodyRole.Free,
      Body.C2 -> BodyRole.Free,
      Body.R2 -> BodyRole.Output
    ))
    assert(!t.s1Present && t.s2Present)
    assertEquals(t.meshCount, 3)
  }

  test("a coupling may not weld an Absent body") {
    val roles = Topology.standardCompound.roles
      .updated(Body.S1, BodyRole.Free).updated(Body.C1, BodyRole.Input).updated(Body.R1, BodyRole.Fixed)
      .updated(Body.S1, BodyRole.Absent)
    intercept[IllegalArgumentException] {
      Topology(2, roles, Coupling.CarrierToCarrier, Coupling.SunToSun)
    }
  }

  test("PlanetStack requires CarrierToCarrier as the other coupling (ADR 0015 §4)") {
    val roles = Topology.standardCompound.roles
    for other <- Coupling.values.filterNot(c => c == Coupling.CarrierToCarrier || c == Coupling.PlanetStack) do
      intercept[IllegalArgumentException](Topology(2, roles, Coupling.PlanetStack, other))
      intercept[IllegalArgumentException](Topology(2, roles, other, Coupling.PlanetStack))
    // Either order is fine with the shared carrier.
    assert(Topology(2, roles, Coupling.PlanetStack, Coupling.CarrierToCarrier).stacked)
    assert(Topology(2, roles, Coupling.CarrierToCarrier, Coupling.PlanetStack).stacked)
  }

  test("stacked iff either coupling is PlanetStack") {
    assert(Topology.standardCompound.stacked && Topology.cdcpCompound.stacked && Topology.nssCompound.stacked)
    assert(!Topology.singleStage.stacked)
    assert(!Topology(2, Topology.standardCompound.roles, Coupling.CarrierToCarrier, Coupling.RingToSun).stacked)
  }

  test("withAbsentSuns removes unpinned suns and reports illegal requests as Left") {
    assertEquals(Topology.withAbsentSuns(Topology.cdcpCompound, true, true), Right(Topology.sunlessCdcpCompound))
    assertEquals(Topology.withAbsentSuns(Topology.standardCompound, false, true), Right(Topology.nssCompound))
    assert(Topology.withAbsentSuns(Topology.standardCompound, true, false).isLeft) // S1 is the Input
    assert(Topology.withAbsentSuns(Topology.singleStage, false, true).isLeft)
    assert(Topology.withAbsentSuns(Topology.singleStage, true, false).isLeft)       // only 2 bodies left for 3 roles
  }

  test("sun-less topologies count one mesh per sun-less stage") {
    assertEquals(Topology.standardCompound.meshCount, 4)
    assertEquals(Topology.nssCompound.meshCount, 3)
    assertEquals(Topology.sunlessCdcpCompound.meshCount, 2)
    assertEquals(Topology.singleStage.meshCount, 2)
  }

  test("constructor rejects single-stage topology with S2 or R2 roles") {
    intercept[IllegalArgumentException] {
      Topology(1, Map(
        Body.S1 -> BodyRole.Input,
        Body.C1 -> BodyRole.Output,
        Body.R1 -> BodyRole.Fixed,
        Body.S2 -> BodyRole.Free  // S2 doesn't exist in single-stage
      ))
    }
  }

  test("constructor rejects compound topology missing required body roles") {
    intercept[IllegalArgumentException] {
      Topology(2, Map(
        Body.S1 -> BodyRole.Input,
        Body.C1 -> BodyRole.Fixed,
        Body.R1 -> BodyRole.Output
        // missing S2, C2, R2
      ))
    }
  }

  test("Body.present removes the absent bodies from the stage count's body set") {
    assertEquals(Body.present(1, Set.empty),  Set(Body.S1, Body.C1, Body.R1))
    assertEquals(Body.present(2, Set.empty),  Set(Body.S1, Body.C1, Body.R1, Body.S2, Body.C2, Body.R2))
    assertEquals(Body.present(2, Set(Body.S2)), Set(Body.S1, Body.C1, Body.R1, Body.C2, Body.R2))
    assertEquals(Topology.nssCompound.presentBodies, Set(Body.S1, Body.C1, Body.R1, Body.C2, Body.R2))
  }

  test("legacyMode returns the canonical legacy Mode for the four legacy tuples") {
    assertEquals(Topology.standardCompound.legacyMode, Some(Mode.Standard))
    assertEquals(Topology.cdcpCompound.legacyMode,     Some(Mode.CDCP))
    assertEquals(Topology.nssCompound.legacyMode,      Some(Mode.NSS))
    assertEquals(Topology.singleStage.legacyMode,      Some(Mode.SingleStage))
  }

  test("legacyMode returns None for a novel tuple (carrier-output single-stage)") {
    // Single-stage with carrier-input + sun-output (overdrive) — not a legacy mode.
    val novel = Topology(1, Map(
      Body.C1 -> BodyRole.Input,
      Body.S1 -> BodyRole.Output,
      Body.R1 -> BodyRole.Fixed
    ))
    assertEquals(novel.legacyMode, None)
  }

  test("input / output / fixed accessors return the right bodies") {
    val t = Topology.standardCompound
    assertEquals(t.input,  Body.S1)
    assertEquals(t.output, Body.R2)
    assertEquals(t.fixed,  Body.R1)
    assertEquals(t.freeBodies, Set(Body.C1, Body.S2, Body.C2))
  }

  test("s1Present / s2Present") {
    assert( Topology.standardCompound.s2Present)
    assert( Topology.cdcpCompound.s2Present)
    assert(!Topology.nssCompound.s2Present)
    assert(!Topology.singleStage.s2Present) // there is no stage 2
    assert( Topology.nssCompound.s1Present)
    assert(!Topology.sunlessCdcpCompound.s1Present && !Topology.sunlessCdcpCompound.s2Present)
  }
