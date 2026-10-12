package cpgc.verify

import cpgc.constraints.{Body, BodyRole, Coupling, SearchConfig, TeethBounds, Topology}
import cpgc.domain.{GearSet, Mode, Teeth}
import cpgc.physics.PlanetSpacing
import cpgc.search.{Compounder, RoleSolver}
import munit.FunSuite
import scala.util.Random

/** Forward ↔ inverse round-trip closure — the asymmetry guard.
  *
  * For a random valid `(s1, s2)`:
  *   1. `T = RoleSolver.ratio(topology, s1, s2)` — the forward truth.
  *   2. feed `T` into `Compounder.stage2For(T, s1, cfg)`.
  *   3. assert the returned candidates include an `s2'` whose forward ratio is
  *      within `1e-6` of `T`.
  *
  * This pins the closed-form inverse paths (`stage2ForLegacy` for Standard /
  * CDCP, `stage2ForRingToSun`) against the forward solver at a tight tolerance.
  *
  * **Known constraint surfaced by this test (Edge-Case Catalogue items #6, #13):**
  * the closed-form inverse solvers build the stage-2 candidate with
  * `stage1.numPlanets`. For `PlanetStack` that is correct — a single stacked
  * pinion forces `N1 == N2`. For non-stacked couplings the stage-2 planet count
  * is independent, so the closed form can only surface candidates whose `N`
  * equals `N1`. The test therefore generates `(s1, s2)` with **matching N**,
  * which is the physically valid regime for `PlanetStack` and the in-coverage
  * regime for the `RingToSun` closed form.
  */
class RoundTripSpec extends FunSuite:

  private val Seed = 99L
  private val Tol  = 1e-6

  /** Wide bounds so the inverse search space comfortably contains `s2`. */
  private def cfgFor(mode: Mode, topo: Topology): SearchConfig =
    SearchConfig(
      mode        = mode,
      topology    = Some(topo),
      sunTeeth    = TeethBounds(6, 90),
      planetTeeth = TeethBounds(6, 40),
      ringTeeth   = TeethBounds(20, 200),
      planets     = TeethBounds(3, 6),
      sunPlanetFactor = 1e9 // effectively unbounded
    )

  /** A *physically buildable* even-spaced gear set with a given planet count
    * `n`: `(S + R) % n == 0`, `R = S + 2P`, and planets that don't overlap.
    * The overlap filter matters — the forward solver is purely kinematic and
    * will happily return a ratio for a gear set whose planets physically
    * collide, but the inverse solver (correctly) only surfaces buildable
    * designs. Feeding the round-trip non-buildable inputs would conflate
    * those two concerns. */
  private def randomEvenSpaced(rng: Random, n: Int): GearSet =
    var attempts = 0
    while attempts < 800 do
      val s = rng.between(8, 60)
      val p = rng.between(8, 28)
      val r = s + 2 * p
      val candidate = GearSet(Teeth(s), Teeth(p), n)
      if (s + r) % n == 0 && PlanetSpacing.planetsFitWithoutOverlap(candidate) then
        return candidate
      attempts += 1
    GearSet(Teeth(40), Teeth(10), n) // (40+60)%n==0 for n∈{4,5}; planets fit

  private def compound(second: Coupling): Topology =
    Topology(
      stages = 2,
      roles = Map(
        Body.S1 -> BodyRole.Input,
        Body.R2 -> BodyRole.Output,
        Body.R1 -> BodyRole.Fixed,
        Body.C1 -> BodyRole.Free,
        Body.S2 -> BodyRole.Free,
        Body.C2 -> BodyRole.Free
      ),
      firstCoupling  = Coupling.CarrierToCarrier,
      secondCoupling = second
    )

  /** (label, mode, topology) — the closed-form inverse paths under test. */
  private val cases: Vector[(String, Mode, Topology)] = Vector(
    ("Standard",  Mode.Standard, Topology.standardCompound),
    ("CDCP",      Mode.CDCP,     Topology.cdcpCompound),
    ("RingToSun", Mode.Standard, compound(Coupling.RingToSun))
  )

  for (label, mode, topo) <- cases do
    test(s"forward→inverse→forward round-trips for $label") {
      val rng = new Random(Seed)
      val cfg = cfgFor(mode, topo)
      var tested  = 0
      val misses  = scala.collection.mutable.ArrayBuffer.empty[String]

      for _ <- 1 to 400 do
        // Matching planet count for both stages — the physically valid regime
        // for PlanetStack and the in-coverage regime for the RingToSun closed
        // form (see class doc).
        val n  = rng.between(3, 6)
        val s1 = randomEvenSpaced(rng, n)
        val s2 = randomEvenSpaced(rng, n)
        RoleSolver.ratio(topo, s1, Some(s2)) match
          case None => () // degenerate (s1,s2) for this topology — skip
          case Some(t) =>
            val target = t.value
            // Skip targets the closed forms legitimately can't express.
            if math.abs(target) < 0.05 || math.abs(target - 1.0) < 0.05 then ()
            else
              tested += 1
              val candidates = Compounder.stage2For(target, s1, cfg).take(3000).toVector
              val closure = candidates.iterator
                .flatMap(s2c => RoleSolver.ratio(topo, s1, Some(s2c)).map(_.value))
                .exists(r => math.abs(r - target) <= Tol)
              if !closure then
                misses += f"target=$target%.8f s1=$s1 s2=$s2 candidates=${candidates.size}"

      assert(tested > 30, s"$label: too few round-trip cases exercised ($tested)")
      assert(
        misses.isEmpty,
        s"$label: ${misses.size}/$tested round-trips failed closure at tol=$Tol; first 8:\n" +
          misses.take(8).mkString("\n")
      )
    }
