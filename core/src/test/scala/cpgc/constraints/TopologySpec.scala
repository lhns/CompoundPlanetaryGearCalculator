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

  test("constructor rejects Absent on bodies other than S2") {
    intercept[IllegalArgumentException] {
      Topology(2, Map(
        Body.S1 -> BodyRole.Absent,
        Body.C1 -> BodyRole.Input,
        Body.R1 -> BodyRole.Fixed,
        Body.S2 -> BodyRole.Output,
        Body.C2 -> BodyRole.Free,
        Body.R2 -> BodyRole.Free
      ))
    }
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

  test("Body.present matches each (stages, s2Present) combination") {
    assertEquals(Body.present(1, s2Present = true),  Set(Body.S1, Body.C1, Body.R1))
    assertEquals(Body.present(1, s2Present = false), Set(Body.S1, Body.C1, Body.R1))
    assertEquals(Body.present(2, s2Present = true),  Set(Body.S1, Body.C1, Body.R1, Body.S2, Body.C2, Body.R2))
    assertEquals(Body.present(2, s2Present = false), Set(Body.S1, Body.C1, Body.R1, Body.R2))
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

  test("s2Present is true for legacy modes that include S2 and false for NSS") {
    assert( Topology.standardCompound.s2Present)
    assert( Topology.cdcpCompound.s2Present)
    assert(!Topology.nssCompound.s2Present)
    assert( Topology.singleStage.s2Present) // s2 isn't part of the body set anyway
  }

  test("secondSunPresent alias agrees with s2Present") {
    for t <- Seq(Topology.standardCompound, Topology.cdcpCompound,
                 Topology.nssCompound, Topology.singleStage) do
      assertEquals(t.secondSunPresent, t.s2Present)
  }
