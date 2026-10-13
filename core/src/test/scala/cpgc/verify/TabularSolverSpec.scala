package cpgc.verify

import cpgc.constraints.{Body, BodyRole, Coupling, Topology}
import cpgc.domain.{GearSet, Teeth}
import cpgc.search.RoleSolver
import munit.FunSuite
import scala.util.Random

/** N-version cross-check: the production `RoleSolver` (matrix / Gauss) vs the
  * independent `TabularSolver` (superposition method / substitution). They
  * share no code; for every in-coverage topology they must agree to 1e-9.
  *
  * A disagreement is a bug in one solver or the other — triage it into the
  * Edge-Case Catalogue in `docs/verification/correctness-strategy.md`.
  */
class TabularSolverSpec extends FunSuite:

  private val Seed = 1234L

  /** Standard role tuple (S1 in, R2 out, R1 fixed), shared carrier, parameterised second coupling. */
  private def withSecond(second: Coupling): Topology =
    Topology.standardCompound.copy(secondCoupling = second)

  /** All topologies inside the TabularSolver's coverage. */
  private val coveredTopologies: Vector[(String, Topology)] = Vector(
    "single"      -> Topology.singleStage,
    "standard"    -> Topology.standardCompound,
    "cdcp"        -> Topology.cdcpCompound,
    "nss"         -> Topology.nssCompound,
    "PlanetStack" -> withSecond(Coupling.PlanetStack),
    "RingToSun"   -> withSecond(Coupling.RingToSun),
    "SunToRing"   -> withSecond(Coupling.SunToRing),
    "RingToRing"  -> withSecond(Coupling.RingToRing),
    "SunToSun"    -> withSecond(Coupling.SunToSun),
    // Absent suns (ADR 0015 §2): the oracle solves a sun-less stage with its virtual sun.
    "sun-less CDCP (both absent)" -> Topology.sunlessCdcpCompound,
    "CDCP, S1 absent"             -> cdcpWith(absent = Set(Body.S1)),
    "CDCP, S2 absent"             -> cdcpWith(absent = Set(Body.S2)),
    "PS first, CC second"         -> Topology(2, Topology.standardCompound.roles, Coupling.PlanetStack, Coupling.CarrierToCarrier),
    "CC+RingToRing, S2 absent"    -> cdcpWith(Coupling.RingToRing, Set(Body.S2), Body.S1, Body.C1, Body.R2),
    "CC+RingToRing, S1 absent"    -> cdcpWith(Coupling.RingToRing, Set(Body.S1), Body.C1, Body.R1, Body.S2),
    "CC+SunToRing, S2 absent"     -> cdcpWith(Coupling.SunToRing, Set(Body.S2), Body.S1, Body.C1, Body.R1)
  )

  /** Shared-carrier compound with the given second coupling, absent suns and pins. */
  private def cdcpWith(
      second: Coupling = Coupling.PlanetStack,
      absent: Set[Body],
      in: Body = Body.C1, fixed: Body = Body.R1, out: Body = Body.R2
  ): Topology =
    ContractSupport.compound(in, out, fixed, Coupling.CarrierToCarrier, second, absent)

  private def randomGearSet(rng: Random): GearSet =
    GearSet(Teeth(rng.between(8, 60)), Teeth(rng.between(8, 30)), rng.between(3, 6))

  /** A sun-less stage is built from a virtual sun S = R − 2P, so we generate it
    * the way the search does. */
  private def randomSunlessStage(rng: Random): GearSet =
    val p = rng.between(8, 30)
    val r = rng.between(2 * p + 1, 2 * p + 50)
    GearSet(Teeth(r - 2 * p), Teeth(p), rng.between(3, 6))

  test("RoleSolver agrees with the independent TabularSolver across a random sweep") {
    val rng = new Random(Seed)
    val iterations = 1500 // × 16 topologies ≈ 24 000 combinations
    var checked = 0
    val disagreements = scala.collection.mutable.ArrayBuffer.empty[String]

    for _ <- 1 to iterations do
      val s1 = randomGearSet(rng)
      for (name, topo) <- coveredTopologies do
        val s2: Option[GearSet] = topo.stages match
          case 1                       => None
          case 2 if !topo.s2Present    => Some(randomSunlessStage(rng))
          case _                       => Some(randomGearSet(rng))

        val role    = RoleSolver.ratio(topo, s1, s2).map(_.value)
        val tabular = TabularSolver.ratio(topo, s1, s2)

        (role, tabular) match
          case (Some(a), Some(b)) =>
            checked += 1
            val rel = math.abs(a - b) / math.max(1.0, math.abs(a))
            if rel >= 1e-9 then
              disagreements += s"[$name] s1=$s1 s2=$s2 : RoleSolver=$a TabularSolver=$b (rel=$rel)"
          case (None, None) =>
            () // both reject — agreement
          case _ =>
            // One solver produced a ratio and the other didn't — a shape
            // disagreement worth triaging.
            disagreements += s"[$name] s1=$s1 s2=$s2 : RoleSolver=$role TabularSolver=$tabular (shape mismatch)"

    assert(checked > 15000, s"expected >15000 numeric comparisons, got $checked")
    assert(
      disagreements.isEmpty,
      s"${disagreements.size} disagreement(s) between the two solvers; first 5:\n" +
        disagreements.take(5).mkString("\n")
    )
  }

  test("TabularSolver returns None for independent-carrier compounds (out of coverage)") {
    val s1 = GearSet(Teeth(20), Teeth(15), 5)
    val s2 = GearSet(Teeth(24), Teeth(12), 3)
    val independentCarrier = Topology(
      stages = 2,
      roles = Map(
        Body.S1 -> BodyRole.Input,
        Body.C2 -> BodyRole.Output,
        Body.R1 -> BodyRole.Fixed,
        Body.C1 -> BodyRole.Free,
        Body.S2 -> BodyRole.Free,
        Body.R2 -> BodyRole.Free
      ),
      firstCoupling  = Coupling.CarrierToSun,
      secondCoupling = Coupling.RingToRing
    )
    assertEquals(TabularSolver.ratio(independentCarrier, s1, Some(s2)), None)
  }
