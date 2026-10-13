package cpgc

import cpgc.render.GearGeometry
import cpgc.render.GearGeometry.{planetPhaseOffset, polygonOutlineRadius}
import munit.FunSuite

/** ⚠ DIAGNOSTIC ONLY. The residuals printed here are real: a non-zero residual
  *   at planets 1..N-1 means the design cannot be assembled with identical
  *   stepped planets. (An earlier version of this header called them artefacts
  *   of misusing `GearGeometry.planetPhaseOffset`; that was wrong. They are the
  *   cross term of ADR 0018, `((Tp1·Tr2 − Tp2·Tr1) / gcd(Tp1, Tp2)) % N == 0`,
  *   which the old `(S + R) % N` rule missed. Design B below, the NSS winner,
  *   fails it; design A, the Standard winner, passes.)
  *
  *   The exact tooth-phase oracle is `verify/AssemblyOracle.scala`, checked in
  *   `AssemblyConditionSpec`; derivation in
  *   `docs/verification/assembly-conditions.md`. This spec is kept as a visual
  *   probe of the per-planet fits the renderer applies, via
  *   `GearGeometry.planetPhaseOffset`, which returns the shortest visual
  *   rotation (wrapped to `[-π/Tp, π/Tp]`), not a planet's physical rotation.
  *
  * Empirical probe of whether the two specific designs the user observed are
  * actually assemblable under each topology.
  *
  *   - Design A: Standard search winner at target 53.12.
  *       Stage 1: S=19, P=17, R=53, N=3.
  *       Stage 2: S=19, P=14, R=47, N=3.
  *
  *   - Design B: NSS search winner at target 53.12.
  *       Stage 1: S=53, P=11, R=75, N=4.
  *       Stage 2: virtual S=72, P=14, R=100, N=4.
  *
  * The tests below compute the mesh-phase-invariant residuals at every planet
  * position under the relevant topology — closing the invariant at planet 0
  * by fitting the ring (and, for Standard, sun) rotation, then reading the
  * residuals at the other N−1 planets. Zero residual everywhere means the
  * design assembles symmetrically. Non-zero residual means it does not.
  *
  * No production code changes; this spec is a one-shot probe whose printed
  * values inform the next move on the filter rewrite.
  */
class SpacingExperimentSpec extends FunSuite:
  override def munitTestTransforms = super.munitTestTransforms ++ List(
    new TestTransform("diagnostic", t => t.withTags(t.tags + munit.Tag("diagnostic")))
  )

  private val twoPi     = 2.0 * math.Pi
  private val baseOrbit = -math.Pi / 2.0
  private val Tol       = 1e-9

  /** `x mod 1`, always non-negative. */
  private def mod1(x: Double): Double =
    x - math.floor(x)

  /** Distance from `x` to `target` on the unit circle, in `[-0.5, 0.5]`. */
  private def fracDiff(x: Double, target: Double): Double =
    val d = mod1(x - target)
    if d > 0.5 then d - 1.0 else d

  // ── Residual computations ──────────────────────────────────────────────────

  /** External-mesh (sun-planet) phase residual at planet `i`. Zero means
    * `f_s + f_p_inward ≡ 0.5 (mod 1)` holds. `deltaSun` is the sun's static
    * rotation; `deltaPlanet` is the planet body rotation at this position. */
  private def sunMeshResidual(
      Ts: Int, Tp: Int, phi: Double, deltaSun: Double, deltaPlanet: Double
  ): Double =
    val fs       = mod1(Ts * (phi - deltaSun) / twoPi)
    val fpInward = mod1(Tp * (math.Pi - deltaPlanet) / twoPi)
    fracDiff(mod1(fs + fpInward), 0.5)

  /** Internal-mesh (planet-ring) phase residual at planet `i`. Zero means
    * `f_r − f_p_outward ≡ 0.5 (mod 1)` holds. */
  private def ringMeshResidual(
      Tp: Int, Tr: Int, phi: Double, deltaRing: Double, deltaPlanet: Double
  ): Double =
    val fr       = mod1(Tr * (phi - deltaRing) / twoPi)
    val fpOutward = mod1(Tp * (-deltaPlanet) / twoPi)
    fracDiff(mod1(fr - fpOutward), 0.5)

  /** Fit `deltaRing` so the ring-mesh residual is exactly 0 at the given
    * (phi, deltaPlanet). */
  private def fitDeltaRing(Tp: Int, Tr: Int, phi: Double, deltaPlanet: Double): Double =
    // fr − fp_outward ≡ 0.5  ⇒  fr = fp_outward + 0.5  ⇒
    // Tr·(phi − deltaRing)/twoPi ≡ Tp·(−deltaPlanet)/twoPi + 0.5 (mod 1)
    // deltaRing = phi − (Tp·(−deltaPlanet)/twoPi + 0.5) · twoPi/Tr
    val fpOutward = mod1(Tp * (-deltaPlanet) / twoPi)
    phi - (fpOutward + 0.5) * twoPi / Tr

  /** Fit `deltaSun` so the sun-mesh residual is exactly 0 at the given
    * (phi, deltaPlanet). */
  private def fitDeltaSun(Ts: Int, Tp: Int, phi: Double, deltaPlanet: Double): Double =
    // fs + fp_inward ≡ 0.5  ⇒  fs = 0.5 − fp_inward  ⇒
    // Ts·(phi − deltaSun)/twoPi ≡ 0.5 − Tp·(π − deltaPlanet)/twoPi (mod 1)
    val fpInward = mod1(Tp * (math.Pi - deltaPlanet) / twoPi)
    phi - (0.5 - fpInward) * twoPi / Ts

  /** Compute the per-planet residuals for an NSS stage 2 driven by stage-1's
    * planet orientations. Returns the residual at each of the N planet
    * positions. Residual at planet 0 is 0 by construction (we fit deltaRing2
    * there); the rest reveal whether the design assembles. */
  private def nssStage2RingResiduals(
      Ts1: Int, Tp1: Int, Tp2: Int, Tr2: Int, N: Int
  ): Vector[Double] =
    // Clocking offset c between stage-1 and stage-2 planet tooth patterns is
    // free at assembly but uniform across shafts, so it just shifts every
    // planet's stage-2 body angle by the same constant. We absorb c into
    // deltaRing2 by fitting at planet 0 with c = 0; the residuals at planets
    // 1..N−1 are c-independent.
    val phi0     = baseOrbit
    val delta1_0 = planetPhaseOffset(Ts1, Tp1, phi0)
    val deltaRing2 = fitDeltaRing(Tp2, Tr2, phi0, delta1_0)
    (0 until N).toVector.map { i =>
      val phi    = baseOrbit + twoPi * i / N
      val delta1 = planetPhaseOffset(Ts1, Tp1, phi)
      ringMeshResidual(Tp2, Tr2, phi, deltaRing2, delta1)
    }

  /** Same as above but for a Standard stage 2 (planet meshes with both sun-2
    * and ring-2). Returns (sunResiduals, ringResiduals) per planet. */
  private def standardStage2Residuals(
      Ts1: Int, Tp1: Int, Ts2: Int, Tp2: Int, Tr2: Int, N: Int
  ): (Vector[Double], Vector[Double]) =
    val phi0       = baseOrbit
    val delta1_0   = planetPhaseOffset(Ts1, Tp1, phi0)
    val deltaSun2  = fitDeltaSun(Ts2, Tp2, phi0, delta1_0)
    val deltaRing2 = fitDeltaRing(Tp2, Tr2, phi0, delta1_0)
    val sunRes = (0 until N).toVector.map { i =>
      val phi    = baseOrbit + twoPi * i / N
      val delta1 = planetPhaseOffset(Ts1, Tp1, phi)
      sunMeshResidual(Ts2, Tp2, phi, deltaSun2, delta1)
    }
    val ringRes = (0 until N).toVector.map { i =>
      val phi    = baseOrbit + twoPi * i / N
      val delta1 = planetPhaseOffset(Ts1, Tp1, phi)
      ringMeshResidual(Tp2, Tr2, phi, deltaRing2, delta1)
    }
    (sunRes, ringRes)

  // ── The two observed designs ───────────────────────────────────────────────

  // Design A — Standard search winner.
  private val Ts1A = 19; private val Tp1A = 17; private val Tr1A = 53
  private val Ts2A = 19; private val Tp2A = 14; private val Tr2A = 47
  private val NA   = 3

  // Design B — NSS search winner (virtual S2 = R2 − 2·P2 = 72).
  private val Ts1B = 53; private val Tp1B = 11; private val Tr1B = 75
  private val Ts2vB = 72; private val Tp2B = 14; private val Tr2B = 100
  private val NB    = 4

  // ── Probe 1 — Design A under Standard (both stages independent Willis) ────

  test("Design A — Standard stage 2: both sun and ring residuals (compound coupled, diagnostic)") {
    val (sunRes, ringRes) = standardStage2Residuals(Ts1A, Tp1A, Ts2A, Tp2A, Tr2A, NA)
    val maxSun  = sunRes.map(math.abs).max
    val maxRing = ringRes.map(math.abs).max
    println(s"[Design A, Standard stage 2] sunRes=${sunRes.mkString(", ")}  max=$maxSun")
    println(s"[Design A, Standard stage 2] ringRes=${ringRes.mkString(", ")}  max=$maxRing")
    if maxSun < Tol && maxRing < Tol then
      println("[Design A, Standard stage 2] BOTH close → compound-assemblable in Standard")
    else
      println(s"[Design A, Standard stage 2] does NOT close — max sun=$maxSun max ring=$maxRing")
  }

  test("Design A — NSS stage 2: ring residual at planets 1..N-1 (stage-1's planet orientation forced)") {
    val ringRes = nssStage2RingResiduals(Ts1A, Tp1A, Tp2A, Tr2A, NA)
    val maxRing = ringRes.map(math.abs).max
    println(s"[Design A, NSS stage 2] ringRes=${ringRes.mkString(", ")} max|res|=$maxRing")
    // Diagnostic only — we don't assert pass/fail; we report. The plan calls
    // this the "live question". If maxRing < Tol, Design A IS NSS-assemblable.
    if maxRing < Tol then
      println("[Design A, NSS stage 2] residual closes → Design A IS NSS-assemblable")
    else
      println(s"[Design A, NSS stage 2] residual does NOT close → Design A is NOT NSS-assemblable (max=$maxRing)")
  }

  // ── Probe 2 — Design B under both topologies ───────────────────────────────

  test("Design B — NSS stage 2: ring residual (filter says accept; diagnostic only)") {
    val ringRes = nssStage2RingResiduals(Ts1B, Tp1B, Tp2B, Tr2B, NB)
    val maxRing = ringRes.map(math.abs).max
    println(s"[Design B, NSS stage 2] ringRes=${ringRes.mkString(", ")} max|res|=$maxRing")
    if maxRing < Tol then
      println("[Design B, NSS stage 2] residual closes → compound-NSS-assemblable")
    else
      println(s"[Design B, NSS stage 2] residual does NOT close (max=$maxRing) — filter accepts a non-assemblable design")
  }

  test("Design B — Standard interpretation (virtual S2 = 72 as real sun): both residuals at every planet") {
    val (sunRes, ringRes) = standardStage2Residuals(Ts1B, Tp1B, Ts2vB, Tp2B, Tr2B, NB)
    val maxSun  = sunRes.map(math.abs).max
    val maxRing = ringRes.map(math.abs).max
    println(s"[Design B, Standard stage 2] sunRes=${sunRes.mkString(", ")}")
    println(s"[Design B, Standard stage 2] ringRes=${ringRes.mkString(", ")}")
    if maxSun < Tol && maxRing < Tol then
      println(s"[Design B, Standard stage 2] both close → also Standard-assemblable")
    else
      println(s"[Design B, Standard stage 2] max sun=$maxSun max ring=$maxRing — NOT both close")
  }

  // ── Probe 3 — Print stage 1 closure for both (sanity check) ───────────────

  /** Sanity helper: residuals for a single planetary stage. */
  private def stage1RingResiduals(Ts: Int, Tp: Int, Tr: Int, N: Int): Vector[Double] =
    val phi0       = baseOrbit
    val delta1_0   = planetPhaseOffset(Ts, Tp, phi0)
    val deltaRing1 = fitDeltaRing(Tp, Tr, phi0, delta1_0)
    (0 until N).toVector.map { i =>
      val phi   = baseOrbit + twoPi * i / N
      val delta = planetPhaseOffset(Ts, Tp, phi)
      ringMeshResidual(Tp, Tr, phi, deltaRing1, delta)
    }

  test("Stage 1 ring residuals close for both designs (sanity: Willis holds)") {
    val resA = stage1RingResiduals(Ts1A, Tp1A, Tr1A, NA)
    val resB = stage1RingResiduals(Ts1B, Tp1B, Tr1B, NB)
    println(s"[Design A stage 1] ringRes=${resA.mkString(", ")} max=${resA.map(math.abs).max}")
    println(s"[Design B stage 1] ringRes=${resB.mkString(", ")} max=${resB.map(math.abs).max}")
    assert(resA.map(math.abs).max < Tol, s"Design A stage 1 Willis fails")
    assert(resB.map(math.abs).max < Tol, s"Design B stage 1 Willis fails")
  }

  // ── Probe 4 — Filter behaviour as assertions ──────────────────────────────

  test("Current filter behaviour: lock the observed accept/reject decisions") {
    // Design A.
    val stdA   = (Ts1A + Tr1A) % NA == 0 && (Ts2A + Tr2A) % NA == 0
    val nssA   = (Ts1A + Tr1A) % NA == 0 && Tr2A % NA == 0
    assertEquals(stdA, true,  s"Standard filter should accept Design A — (Ts1+Tr1)%$NA=${(Ts1A+Tr1A)%NA}, (Ts2+Tr2)%$NA=${(Ts2A+Tr2A)%NA}")
    assertEquals(nssA, false, s"NSS filter should reject Design A — Tr2%$NA=${Tr2A%NA}")

    // Design B.
    val stdB   = (Ts1B + Tr1B) % NB == 0 && (Ts2vB + Tr2B) % NB == 0
    val nssB   = (Ts1B + Tr1B) % NB == 0 && Tr2B % NB == 0
    assertEquals(stdB, true,  s"Standard filter should accept Design B (virtual S2) — Willis holds")
    assertEquals(nssB, true,  s"NSS filter should accept Design B")
  }

  // ── Probe 5 — Polygon non-overlap reality check ───────────────────────────
  //
  // Replicates the existing GearGeometrySpec non-overlap pattern for the
  // specific designs under both stage-2 topologies. Module = 1 throughout
  // since the inequality is module-scale-invariant.

  /** Stage-2 planet↔ring non-overlap given stage-1-forced planet orientation
    * at each position. Returns true iff non-overlap holds at every planet. */
  private def nssNonOverlap(
      Ts1: Int, Tp1: Int, Tp2: Int, Tr2: Int, N: Int
  ): Boolean =
    val m          = 1.0
    val rOrbit     = m * (Ts1 + Tp1) / 2.0  // stage-1's carrier radius — both stages share
    (0 until N).forall { i =>
      val phi    = baseOrbit + twoPi * i / N
      val delta1 = planetPhaseOffset(Ts1, Tp1, phi)
      // Stage-2 planet's outward extent at the radial direction (planet body angle = -delta1).
      val yPlOut      = polygonOutlineRadius(Tp2, m, internal = false, bodyAngle = -delta1)
      val planetReach = rOrbit + yPlOut
      val _           = phi  // not used in the simplified non-overlap inequality
      planetReach <= m * Tr2 / 2.0 + 1.25 * m + 1e-6 // ring outer radius is rPitch + 1.25m
    }

  test("Polygon non-overlap — designs A & B under both interpretations (printed only)") {
    val results = Seq(
      ("Design A NSS stage 2",      nssNonOverlap(Ts1A, Tp1A, Tp2A, Tr2A, NA)),
      ("Design B NSS stage 2",      nssNonOverlap(Ts1B, Tp1B, Tp2B, Tr2B, NB))
    )
    results.foreach { (label, ok) =>
      println(s"[non-overlap] $label: ${if ok then "OK" else "OVERLAP"}")
    }
    // Don't assert — these are sanity probes. If a real overlap shows up we
    // want to see it in the output and add a follow-up.
  }
