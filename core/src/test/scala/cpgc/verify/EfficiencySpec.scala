package cpgc.verify

import cpgc.constraints.{Body, BodyRole, Coupling, Topology}
import cpgc.physics.Efficiency
import munit.FunSuite

/** Golden and structural coverage for `Efficiency.overall(topology)` (ADR 0015
  * §7): 0.985 per mesh, 2 meshes per stage with a sun, 1 per sun-less stage.
  * The model is a coarse mesh-count estimate, so the tests pin its structure
  * and exact values rather than claiming dyno-grade accuracy.
  */
class EfficiencySpec extends FunSuite:

  private val Mesh = 0.985

  private val sunlessStage1 = Topology(2, Map(
    Body.S1 -> BodyRole.Absent, Body.C1 -> BodyRole.Input, Body.R1 -> BodyRole.Fixed,
    Body.S2 -> BodyRole.Free, Body.C2 -> BodyRole.Free, Body.R2 -> BodyRole.Output))

  test("golden: efficiency per canonical topology") {
    assertEqualsDouble(Efficiency.overall(Topology.singleStage),         math.pow(Mesh, 2), 1e-12)
    assertEqualsDouble(Efficiency.overall(Topology.standardCompound),    math.pow(Mesh, 4), 1e-12)
    assertEqualsDouble(Efficiency.overall(Topology.cdcpCompound),        math.pow(Mesh, 4), 1e-12)
    assertEqualsDouble(Efficiency.overall(Topology.nssCompound),         math.pow(Mesh, 3), 1e-12)
    assertEqualsDouble(Efficiency.overall(sunlessStage1),                math.pow(Mesh, 3), 1e-12)
    assertEqualsDouble(Efficiency.overall(Topology.sunlessCdcpCompound), math.pow(Mesh, 2), 1e-12)
  }

  test("coupling choice does not change the mesh count (suns present)") {
    val roles = Topology.standardCompound.roles
    for c <- Coupling.values.filterNot(c => c == Coupling.PlanetStack || c == Coupling.CarrierToCarrier) do
      assertEqualsDouble(Efficiency.overall(Topology(2, roles, Coupling.CarrierToCarrier, c)), math.pow(Mesh, 4), 1e-12, s"$c")
  }

  test("B2: an NSS-like novel topology (S2 absent) gets the 3-mesh efficiency, not Standard's 4") {
    val t = Topology(2, Map(
      Body.S1 -> BodyRole.Input, Body.R2 -> BodyRole.Output, Body.C1 -> BodyRole.Fixed,
      Body.R1 -> BodyRole.Free, Body.C2 -> BodyRole.Free, Body.S2 -> BodyRole.Absent))
    assertEqualsDouble(Efficiency.overall(t), math.pow(Mesh, 3), 1e-12)
  }

  test("efficiency is in (0, 1] and a sun-less stage never lowers it") {
    for t <- Seq(Topology.singleStage, Topology.standardCompound, Topology.cdcpCompound,
                 Topology.nssCompound, Topology.sunlessCdcpCompound, sunlessStage1) do
      val e = Efficiency.overall(t)
      assert(e > 0.0 && e <= 1.0, s"$e for $t")
    assert(Efficiency.overall(Topology.nssCompound) > Efficiency.overall(Topology.standardCompound))
    assert(Efficiency.overall(Topology.singleStage) > Efficiency.overall(Topology.standardCompound))
  }
