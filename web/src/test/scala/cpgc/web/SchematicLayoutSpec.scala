package cpgc.web

import cpgc.constraints.{Body, Coupling, Topology}
import cpgc.domain.{GearSet, Module, Stage, Teeth}
import munit.FunSuite

// DOM-free checks of the schematic's pure layout.
class SchematicLayoutSpec extends FunSuite:

  import SchematicLayout.*

  private def stage(sun: Int, planet: Int, n: Int) =
    Stage(GearSet(Teeth(sun), Teeth(planet), n), Module(2.0))

  test("every stage ring is scaled to the same outer radius"):
    val (cx1, cx2) = stageCenters(twoStage = true)
    assertEquals((cx1, cx2), (-cx2, cx2)) // symmetric about the origin
    assertEquals(stageCenters(twoStage = false), (0.0, 0.0))
    val a = stageLayout(stage(25, 11, 3), cx1)
    val b = stageLayout(stage(40, 14, 4), cx2)
    for (st, lay) <- Seq(stage(25, 11, 3) -> a, stage(40, 14, 4) -> b) do
      assertEqualsDouble(lay.scale * st.ringOuterDiameter / 2.0, TargetOuterR, 1e-9)
      assert(lay.ringOR < TargetOuterR)

  test("badges are anchored on the stage centre line"):
    val lay = stageLayout(stage(25, 11, 3), 110.0)
    assertEquals(Seq(lay.ringBadge._1, lay.sunBadge._1, lay.carrierBadge._1), Seq(110.0, 110.0, 110.0))
    assert(lay.ringBadge._2 < lay.sunBadge._2 && lay.sunBadge._2 < lay.carrierBadge._2)

  // ── Coupling marks and connectors ──

  private val presets = Seq(
    "standard" -> Topology.standardCompound, "cdcp" -> Topology.cdcpCompound,
    "sunless" -> Topology.sunlessCdcpCompound, "nss" -> Topology.nssCompound,
    "ring-to-sun" -> Topology.ringToSunCascade, "ravigneaux" -> Topology.ravigneaux,
    "simpson" -> Topology.simpson)

  test("standard-family presets: carriers amber (1), planets teal (2), suns and rings uncoloured"):
    for (name, t) <- presets.take(4) do
      val m = couplingMarks(t)
      assertEquals(m.bodies, Map(Body.C1 -> 1, Body.C2 -> 1), name)
      assertEquals(m.planets, Some(2), name)

  test("central welds colour exactly the two welded bodies, in coupling order"):
    assertEquals(couplingMarks(Topology.ringToSunCascade).bodies,
      Map(Body.C1 -> 1, Body.C2 -> 1, Body.R1 -> 2, Body.S2 -> 2))
    assertEquals(couplingMarks(Topology.ravigneaux).bodies,
      Map(Body.C1 -> 1, Body.S2 -> 1, Body.R1 -> 2, Body.R2 -> 2))
    assertEquals(couplingMarks(Topology.simpson).bodies,
      Map(Body.C1 -> 1, Body.R2 -> 1, Body.S1 -> 2, Body.S2 -> 2))
    assertEquals(couplingMarks(Topology.ravigneaux).planets, None)

  test("absent suns are never coloured; single stages have no couplings"):
    for (_, t) <- presets do
      assert(couplingMarks(t).bodies.keySet.subsetOf(t.presentBodies))
    assert(!couplingMarks(Topology.nssCompound).bodies.contains(Body.S2))
    assertEquals(couplingMarks(Topology.singleStage), CouplingMarks(Map.empty, None))

  private val lay1 = stageLayout(stage(25, 11, 3), -110.0)
  private val lay2 = stageLayout(stage(40, 14, 4), 110.0)

  private def onRim(p: (Double, Double), lay: StageLayout, b: Body): Boolean =
    math.abs(math.hypot(p._1 - lay.cx, p._2) - rimRadius(b, lay)) < 1e-9

  test("central connectors end on the welded bodies' rims, facing each other"):
    for (name, t) <- presets; (slot, c) <- Seq(1 -> t.firstCoupling, 2 -> t.secondCoupling) do
      val (b1, b2) = Coupling.bodyPair(c).getOrElse((Body.S1, Body.S2)) // PlanetStack: no connector below
      connector(c, slot, lay1, lay2).foreach { k =>
        if c == Coupling.CarrierToCarrier then
          assertEquals((k.from, k.to), ((lay1.cx, 0.0), (lay2.cx, 0.0)), name)
        else
          assert(onRim(k.from, lay1, b1), s"$name $c from")
          assert(onRim(k.to, lay2, b2), s"$name $c to")
          assert(k.from._1 > lay1.cx && k.to._1 < lay2.cx, s"$name $c faces the other stage")
          // clear of the title/banner band (y < -100) and of the stage labels below
          assert(Seq(k.from, k.ctrl, k.to).forall(p => p._2 > -100 && p._2 < 100), name)
      }

  test("slot 1 connectors run above the mid-line, slot 2 below; PlanetStack has none"):
    val up = connector(Coupling.RingToRing, 1, lay1, lay2).get
    val down = connector(Coupling.RingToRing, 2, lay1, lay2).get
    assert(up.from._2 < 0 && up.to._2 < 0 && down.from._2 > 0 && down.to._2 > 0)
    assertEquals(connector(Coupling.PlanetStack, 2, lay1, lay2), None)

  test("legend labels name the coupling"):
    assertEquals(couplingLegendLabel(Coupling.PlanetStack), "stacked planets (one part)")
    assertEquals(couplingLegendLabel(Coupling.RingToSun), "Ring 1 ↔ Sun 2")
