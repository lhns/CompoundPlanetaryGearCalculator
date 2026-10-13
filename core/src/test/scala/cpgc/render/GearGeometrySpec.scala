package cpgc.render

import cpgc.Calculator
import cpgc.constraints.{SearchConfig, TeethBounds, Topology}
import cpgc.domain.Mode
import cpgc.render.GearGeometry.*
import munit.FunSuite

class GearGeometrySpec extends FunSuite:

  private def mod1(x: Double): Double = x - math.floor(x)

  // ── Mesh-phase invariants ───────────────────────────────────────────────────

  /** External-mesh phase: `f_s + f_p_inward ≡ 0.5 (mod 1)` for every planet,
    * with the per-planet `planetPhaseOffset`. */
  test("planetPhaseOffset yields f_s + f_p_inward = 0.5 for all planets") {
    val cfg = SearchConfig(
      topology        = Topology.fromMode(Mode.Standard),
      sunTeeth    = TeethBounds(10, 30),
      planetTeeth = TeethBounds(10, 20),
      ringTeeth   = TeethBounds(40, 80),
      planets     = TeethBounds(3, 4)
    )
    val designs = Calculator.search(cfg).take(10).toVector
    assert(designs.nonEmpty)

    designs.foreach { d =>
      val Ts = d.stage1.set.sun.toInt
      val Tp = d.stage1.set.planet.toInt
      val n  = d.stage1.set.numPlanets
      val twoPi = 2.0 * math.Pi
      val baseOrbit = -math.Pi / 2.0

      (0 until n).foreach { i =>
        val phi      = baseOrbit + twoPi * i / n
        val delta    = planetPhaseOffset(Ts, Tp, phi)
        // Sun body angle at the radial direction (sun rotation = 0): equals phi.
        val fs       = mod1(Ts * phi / twoPi)
        // Planet body angle at sun-contact (= π − δ_extra in planet frame).
        val fpInward = mod1(Tp * (math.Pi - delta) / twoPi)
        val sum      = mod1(fs + fpInward)
        assert(
          math.abs(sum - 0.5) < 1e-9 || math.abs(sum - 1.5) < 1e-9,
          s"design $d planet $i: f_s=$fs, f_p_inward=$fpInward, sum=$sum (want 0.5)"
        )
      }
    }
  }

  /** Internal-mesh phase: `f_r − f_p_outward ≡ 0.5 (mod 1)` for every planet,
    * with `ringPhaseOffset` applied to the ring rotation. */
  test("ringPhaseOffset yields f_r − f_p_outward = 0.5 for all planets") {
    val cfg = SearchConfig(
      topology        = Topology.fromMode(Mode.Standard),
      sunTeeth    = TeethBounds(10, 30),
      planetTeeth = TeethBounds(10, 20),
      ringTeeth   = TeethBounds(40, 80),
      planets     = TeethBounds(3, 4)
    )
    val designs = Calculator.search(cfg).take(10).toVector
    assert(designs.nonEmpty)

    designs.foreach { d =>
      val Ts = d.stage1.set.sun.toInt
      val Tp = d.stage1.set.planet.toInt
      val Tr = d.stage1.set.ring.toInt
      val n  = d.stage1.set.numPlanets
      val twoPi = 2.0 * math.Pi
      val baseOrbit = -math.Pi / 2.0

      // Ring rotation that fixes planet 0's mesh; assembly condition propagates.
      val deltaRing = ringPhaseOffset(Ts, Tp, Tr, baseOrbit)

      (0 until n).foreach { i =>
        val phi    = baseOrbit + twoPi * i / n
        val delta  = planetPhaseOffset(Ts, Tp, phi)
        val fr     = mod1(Tr * (phi - deltaRing) / twoPi)
        val fpOut  = mod1(Tp * (-delta) / twoPi)
        val diff   = mod1(fr - fpOut)
        assert(
          math.abs(diff - 0.5) < 1e-9,
          s"design $d planet $i: f_r=$fr, f_p_outward=$fpOut, diff=$diff (want 0.5)"
        )
      }
    }
  }
