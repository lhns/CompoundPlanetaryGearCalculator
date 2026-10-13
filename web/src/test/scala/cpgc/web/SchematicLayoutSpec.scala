package cpgc.web

import cpgc.domain.{GearSet, Module, Stage, Teeth}
import munit.FunSuite

// DOM-free checks of the schematic's pure layout.
class SchematicLayoutSpec extends FunSuite:

  import SchematicLayout.*

  private def stage(sun: Int, planet: Int, n: Int) =
    Stage(GearSet(Teeth(sun), Teeth(planet), n), Module(2.0))

  private def degrees(rad: Double): Double = ((math.toDegrees(rad) % 360.0) + 360.0) % 360.0

  test("planets sit at k*360/N on the carrier circle, planet 0 at the top"):
    for n <- 2 to 6; cx <- Seq(0.0, -110.0, 110.0) do
      val lay = stageLayout(stage(25, 11, n), cx)
      assertEquals(lay.planets.size, n)
      lay.planets.zipWithIndex.foreach { case ((x, y), k) =>
        assertEqualsDouble(math.hypot(x - cx, y), lay.carrierR, 1e-9)
        val fromTop = math.atan2(y, x - cx) + math.Pi / 2.0
        assertEqualsDouble(degrees(fromTop - math.toRadians(k * 360.0 / n) + math.Pi) - 180.0, 0.0, 1e-6)
      }

  test("every stage ring is scaled to the same outer radius"):
    val (cx1, cx2) = stageCenters(twoStage = true)
    val a = stageLayout(stage(25, 11, 3), cx1)
    val b = stageLayout(stage(40, 14, 4), cx2)
    for (st, lay) <- Seq(stage(25, 11, 3) -> a, stage(40, 14, 4) -> b) do
      assertEqualsDouble(lay.scale * st.ringOuterDiameter / 2.0, TargetOuterR, 1e-9)
      assert(lay.ringOR < TargetOuterR)
      assertEqualsDouble(lay.ringPitchR, lay.mPx * st.set.ring.toDouble / 2.0, 1e-9)

  test("stage centres are symmetric for a compound and coincide for a single stage"):
    assertEquals(stageCenters(twoStage = true), (-110.0, 110.0))
    assertEquals(stageCenters(twoStage = false), (0.0, 0.0))

  test("badges are anchored on the stage centre line"):
    val lay = stageLayout(stage(25, 11, 3), 110.0)
    assertEquals(Seq(lay.ringBadge._1, lay.sunBadge._1, lay.carrierBadge._1), Seq(110.0, 110.0, 110.0))
    assert(lay.ringBadge._2 < lay.sunBadge._2 && lay.sunBadge._2 < lay.carrierBadge._2)

  test("gear outlines are closed paths"):
    assert(externalGearPath(0, 0, 12, 4.0, 0.0).startsWith("M "))
    assert(externalGearPath(0, 0, 12, 4.0, 0.0).endsWith("Z"))
    assert(internalGearPath(0, 0, 47, 4.0, 0.0).endsWith("Z"))
