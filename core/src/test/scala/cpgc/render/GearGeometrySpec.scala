package cpgc.render

import cpgc.Calculator
import cpgc.constraints.{SearchConfig, TeethBounds, Topology}
import cpgc.domain.Mode
import cpgc.render.GearGeometry.*
import munit.FunSuite

class GearGeometrySpec extends FunSuite:

  /** Tolerance for radial-touch tests (1e-9 = double-precision noise). */
  private val Tol = 1e-9

  // ── Pure formula tests ─────────────────────────────────────────────────────

  test("polygon outline at tooth tip = r_outer") {
    val r = gearRadii(teeth = 19, module = 1.0, internal = false)
    val y = polygonOutlineRadius(19, 1.0, internal = false, bodyAngle = 0.0)
    assertEqualsDouble(y, r.outer, Tol)
  }

  test("polygon outline at flank-at-pitch crossing = r_pitch") {
    val teeth  = 19
    val module = 1.0
    val r      = gearRadii(teeth, module, internal = false)
    val deltaPitch = math.Pi / (2.0 * teeth)
    // Right flank at pitch is at body angle (deltaPitch) from tooth centre.
    val y = polygonOutlineRadius(teeth, module, internal = false, bodyAngle = deltaPitch)
    assertEqualsDouble(y, r.pitch, 1e-6)
  }

  test("polygon outline in trough = r_root (external)") {
    val teeth  = 19
    val module = 1.0
    val r      = gearRadii(teeth, module, internal = false)
    // Trough centre is at body angle π / T (half pitch).
    val troughCenter = math.Pi / teeth
    val y = polygonOutlineRadius(teeth, module, internal = false, bodyAngle = troughCenter)
    assertEqualsDouble(y, r.root, Tol)
  }

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

  // ── Radial no-overlap property ──────────────────────────────────────────────

  /** At the radial direction (line from sun centre through planet centre), the
    * sun and planet polygons must not overlap: their outline radii from each
    * gear's centre must sum to AT MOST the centre distance. For involute
    * geometry the kinematic clearance is exactly 0.25·module, so the sum lies
    * in [C − 0.25m, C]. This test verifies that, for every planet of every
    * Standard design, no overlap exists at the radial contact.
    */
  test("sun↔planet polygons do not overlap at the radial contact") {
    val cfg = SearchConfig(
      topology        = Topology.fromMode(Mode.Standard),
      sunTeeth    = TeethBounds(10, 30),
      planetTeeth = TeethBounds(10, 20),
      ringTeeth   = TeethBounds(40, 80),
      planets     = TeethBounds(3, 4)
    )
    val designs = Calculator.search(cfg).take(20).toVector
    assert(designs.nonEmpty)

    designs.foreach { d =>
      val Ts = d.stage1.set.sun.toInt
      val Tp = d.stage1.set.planet.toInt
      val m  = 1.0
      val centerDist = m * (Ts + Tp) / 2.0
      val twoPi      = 2.0 * math.Pi
      val baseOrbit  = -math.Pi / 2.0
      val n          = d.stage1.set.numPlanets

      (0 until n).foreach { i =>
        val phi    = baseOrbit + twoPi * i / n
        val delta  = planetPhaseOffset(Ts, Tp, phi)
        val ySun   = polygonOutlineRadius(Ts, m, internal = false, bodyAngle = phi)
        val yPl    = polygonOutlineRadius(Tp, m, internal = false, bodyAngle = math.Pi - delta)
        val sum    = ySun + yPl
        assert(
          sum <= centerDist + 1e-6,
          f"design $d planet $i: ySun=$ySun%.4f yPl=$yPl%.4f sum=$sum%.4f " +
          f"exceeds centerDist=$centerDist%.4f by ${sum - centerDist}%.4f"
        )
      }
    }
  }

  /** Same property for the planet↔ring internal contact. */
  test("planet↔ring polygons do not overlap at the radial contact") {
    val cfg = SearchConfig(
      topology        = Topology.fromMode(Mode.Standard),
      sunTeeth    = TeethBounds(10, 30),
      planetTeeth = TeethBounds(10, 20),
      ringTeeth   = TeethBounds(40, 80),
      planets     = TeethBounds(3, 4)
    )
    val designs = Calculator.search(cfg).take(20).toVector
    assert(designs.nonEmpty)

    designs.foreach { d =>
      val Ts = d.stage1.set.sun.toInt
      val Tp = d.stage1.set.planet.toInt
      val Tr = d.stage1.set.ring.toInt
      val m  = 1.0
      val twoPi      = 2.0 * math.Pi
      val baseOrbit  = -math.Pi / 2.0
      val n          = d.stage1.set.numPlanets
      val rOrbit     = m * (Ts + Tp) / 2.0
      val rPitchRing = m * Tr / 2.0
      val deltaRing  = ringPhaseOffset(Ts, Tp, Tr, baseOrbit)

      (0 until n).foreach { i =>
        val phi    = baseOrbit + twoPi * i / n
        val delta  = planetPhaseOffset(Ts, Tp, phi)
        // Planet's outward extent at the radial direction.
        val yPlOut = polygonOutlineRadius(Tp, m, internal = false, bodyAngle = -delta)
        val planetReach = rOrbit + yPlOut       // world distance from origin
        // Ring's inner extent at the radial direction.
        val yRing = polygonOutlineRadius(Tr, m, internal = true, bodyAngle = phi - deltaRing)
        // For no overlap, planet's outward reach must not exceed ring's inner extent.
        assert(
          planetReach <= yRing + 1e-6,
          f"design $d planet $i: planetReach=$planetReach%.4f " +
          f"exceeds ringInner=$yRing%.4f by ${planetReach - yRing}%.4f"
        )
      }
    }
  }

  private def mod1(x: Double): Double =
    val r = x - math.floor(x)
    r
