package cpgc

import cpgc.constraints.{SearchConfig, TeethBounds}
import cpgc.domain.{Mode, Ratio}
import cpgc.kinematics.Kinematics
import munit.FunSuite

class KinematicsSpec extends FunSuite:

  // ── Ratio regression: ω_in / ω_out from the kinematics must match design.ratio.

  private val phi = 2.0 * math.Pi

  private def assertRatioMatches(modeName: String, cfg: SearchConfig): Unit =
    val designs = Calculator.search(cfg).take(20).toVector
    assert(designs.nonEmpty, s"$modeName: no designs found")
    designs.foreach { d =>
      val b = Kinematics.at(d, phi)
      val (ain, aout) = Kinematics.io(d, b)
      val observed = ain / aout
      val expected = d.ratio.value
      assert(
        math.abs(observed - expected) < 1e-9 * math.abs(expected).max(1.0),
        s"$modeName: observed $observed vs expected $expected for $d"
      )
    }

  test("single-stage ratio observed from kinematics matches design.ratio") {
    assertRatioMatches("SingleStage", SearchConfig(
      mode        = Mode.SingleStage,
      sunTeeth    = TeethBounds(10, 30),
      planetTeeth = TeethBounds(10, 18),
      ringTeeth   = TeethBounds(40, 80),
      planets     = TeethBounds(3, 3)
    ))
  }

  test("standard compound ratio observed from kinematics matches design.ratio") {
    assertRatioMatches("Standard", SearchConfig(
      mode        = Mode.Standard,
      sunTeeth    = TeethBounds(10, 24),
      planetTeeth = TeethBounds(10, 16),
      ringTeeth   = TeethBounds(40, 60),
      planets     = TeethBounds(3, 3)
    ))
  }

  test("CDCP ratio observed from kinematics matches design.ratio") {
    assertRatioMatches("CDCP", SearchConfig(
      mode        = Mode.CDCP,
      sunTeeth    = TeethBounds(10, 30),
      planetTeeth = TeethBounds(10, 16),
      ringTeeth   = TeethBounds(40, 60),
      planets     = TeethBounds(3, 3)
    ))
  }

  test("NSS ratio observed from kinematics matches design.ratio") {
    assertRatioMatches("NSS", SearchConfig(
      mode        = Mode.NSS,
      sunTeeth    = TeethBounds(10, 24),
      planetTeeth = TeethBounds(10, 16),
      ringTeeth   = TeethBounds(40, 60),
      planets     = TeethBounds(3, 3)
    ))
  }

  test("zero input ⇒ zero everywhere") {
    val cfg = SearchConfig(mode = Mode.SingleStage, sunTeeth = TeethBounds(10, 12),
      planetTeeth = TeethBounds(10, 12), ringTeeth = TeethBounds(34, 36), planets = TeethBounds(3, 3))
    val d = Calculator.search(cfg).head
    val b = Kinematics.at(d, 0.0)
    assertEqualsDouble(b.sun1, 0.0, 0.0)
    assertEqualsDouble(b.carrier, 0.0, 0.0)
    assertEqualsDouble(b.planet1Spin, 0.0, 0.0)
  }

  test("linearity: at(d, 2φ) == 2 · at(d, φ) for every body") {
    val cfg = SearchConfig(mode = Mode.Standard, sunTeeth = TeethBounds(10, 20),
      planetTeeth = TeethBounds(10, 14), ringTeeth = TeethBounds(40, 50), planets = TeethBounds(3, 3))
    val d = Calculator.search(cfg).head
    val b1 = Kinematics.at(d, 1.0)
    val b2 = Kinematics.at(d, 2.0)
    assertEqualsDouble(b2.sun1,        2 * b1.sun1,        1e-12)
    assertEqualsDouble(b2.carrier,     2 * b1.carrier,     1e-12)
    assertEqualsDouble(b2.planet1Spin, 2 * b1.planet1Spin, 1e-12)
    assertEqualsDouble(b2.ring2,       2 * b1.ring2,       1e-12)
  }
