package cpgc.search

import cpgc.constraints.{Body, Coupling, Topology}
import cpgc.domain.{GearSet, Ratio}

/** Forward kinematic solver for a `Topology` over given tooth counts.
  *
  * Body index in the unknown vector:
  *   - single stage:  {S1, C1, R1}                  (3 unknowns)
  *   - NSS (no S2):    {S1, C1, R1, R2}              (4 unknowns; carrier shared, modelled as C1)
  *   - full compound:  {S1, C1, R1, S2, C2, R2}      (6 unknowns)
  *
  * The fundamental constraint equations, derived from each stage's two meshes
  * (external sun↔planet and internal planet↔ring) with the planet velocity
  * eliminated, are:
  *
  *   Willis stage 1:  Ts1·ωS1 + Tr1·ωR1 − (Ts1+Tr1)·ωC1 = 0
  *   Willis stage 2:  Ts2·ωS2 + Tr2·ωR2 − (Ts2+Tr2)·ωC2 = 0    (full compound)
  *
  * A full compound is closed by **two coupling rows** — `firstCoupling` and
  * `secondCoupling`. Every coupling except `PlanetStack` is a plain central-
  * shaft weld `ω_a − ω_b = 0` for the body pair from `Coupling.bodyPair`.
  * `PlanetStack` emits the planet-coupling row derived from equating the two
  * stages' planet-pinion velocities ωP1 = ωP2:
  *
  *   Tp2·(Tp1+Ts1)·ωC1 − Ts1·Tp2·ωS1 − Tp1·(Tp2+Ts2)·ωC2 + Ts2·Tp1·ωS2 = 0
  *
  * (With `CarrierToCarrier` also in effect, ωC1 = ωC2 and this reduces to the
  * classic 5-body planet-coupling row — the legacy compound is bit-identical.)
  *
  * For NSS (S2 absent) the planet-coupling identity eliminates ωP directly
  * from stage 2's surviving planet↔ring mesh, yielding one combined equation:
  *
  *   Tp2·Ts1·ωS1 − (Tp1·Tr2 + Tp2·Ts1)·ωC1 + Tp1·Tr2·ωR2 = 0
  *
  * NSS is hardcoded as shared-carrier + stacked-planet + no-S2; its
  * `firstCoupling` / `secondCoupling` fields are not consulted.
  *
  * **Degree-of-freedom accounting.** Planetary mechanisms are 2-DOF:
  *
  *   Single stage:  3 bodies − 1 Willis                       = 2 DOF
  *   NSS:           4 bodies − 1 Willis − 1 combined           = 2 DOF
  *   Full compound: 6 bodies − 2 Willis − 2 coupling rows      = 2 DOF
  *
  * Pinning the **Input** body's velocity removes one DOF; pinning the
  * **Fixed** body's velocity to zero removes the other.
  */
object RoleSolver:

  /** Angular velocities of every body present in the topology. Bodies that
    * are Absent return 0.0 (they have no physical rotation). The returned
    * map always covers `Body.values` in entirety, with 0.0 for Absent bodies.
    */
  final case class Velocities(values: Map[Body, Double]):
    def get(b: Body): Double = values.getOrElse(b, 0.0)
    def omegaS1: Double = get(Body.S1)
    def omegaC1: Double = get(Body.C1)
    def omegaR1: Double = get(Body.R1)
    def omegaS2: Double = get(Body.S2)
    def omegaC2: Double = get(Body.C2)
    def omegaR2: Double = get(Body.R2)
    /** Stage-2 carrier velocity, falling back to C1 when C2 isn't a distinct
      * body (NSS / single-stage model it as the shared C1). */
    def omegaC2OrC1: Double = if values.contains(Body.C2) then get(Body.C2) else get(Body.C1)

  /** Solve for every body's angular velocity given `omegaIn` at the Input
    * body and 0 at the Fixed body. Returns `None` when the linear system is
    * singular (rare; happens when the chosen topology is kinematically
    * under-determined at these tooth counts).
    */
  def velocities(
      topology: Topology,
      stage1: GearSet,
      stage2: Option[GearSet],
      omegaIn: Double
  ): Option[Velocities] = solveSystem(topology, stage1, stage2, omegaIn).map(_._1)

  private def solveSystem(
      topology: Topology,
      stage1: GearSet,
      stage2: Option[GearSet],
      omegaIn: Double
  ): Option[(Velocities, Double)] =
    val s2Present = topology.s2Present
    val present = topology.stages match
      case 1 => Vector(Body.S1, Body.C1, Body.R1)
      case 2 => if s2Present then Vector(Body.S1, Body.C1, Body.R1, Body.S2, Body.C2, Body.R2)
                else                Vector(Body.S1, Body.C1, Body.R1, Body.R2)
      case n => throw new IllegalArgumentException(s"unsupported stages=$n")
    val n = present.length
    val idx: Map[Body, Int] = present.zipWithIndex.toMap

    val a = Array.ofDim[Double](n, n)
    val b = new Array[Double](n)
    var row = 0

    val ts1 = stage1.sun.toDouble
    val tp1 = stage1.planet.toDouble
    val tr1 = stage1.ring.toDouble

    // Row: Willis stage 1.
    a(row)(idx(Body.S1)) =  ts1
    a(row)(idx(Body.R1)) =  tr1
    a(row)(idx(Body.C1)) = -(ts1 + tr1)
    row += 1

    // Emit one closure row for the given coupling. Every coupling except
    // PlanetStack is a plain `ω_a − ω_b = 0`; PlanetStack is the planet-
    // coupling row from equating ωP1 = ωP2 across both stages.
    def emitCouplingRow(c: Coupling, ts2: Double, tp2: Double): Unit =
      c match
        case Coupling.PlanetStack =>
          a(row)(idx(Body.C1)) =  tp2 * (tp1 + ts1)
          a(row)(idx(Body.S1)) = -ts1 * tp2
          a(row)(idx(Body.C2)) = -tp1 * (tp2 + ts2)
          a(row)(idx(Body.S2)) =  ts2 * tp1
        case other =>
          val (ba, bb) = Coupling.bodyPair(other).getOrElse(
            throw new IllegalStateException(s"coupling $other has no body pair")
          )
          a(row)(idx(ba)) =  1.0
          a(row)(idx(bb)) = -1.0
      row += 1

    // Compound rows.
    if topology.stages == 2 then
      // A 2-stage topology with no stage-2 gear set can't be solved — return
      // `None` rather than throwing. This happens transiently in the UI when
      // the topology signal flips to compound before a fresh (compound) design
      // arrives. The solver is *total*: any input it can't solve yields `None`.
      val s2 = stage2 match
        case Some(g) => g
        case None    => return None
      val tp2 = s2.planet.toDouble
      val tr2 = s2.ring.toDouble
      if s2Present then
        val ts2 = s2.sun.toDouble
        // Row: Willis stage 2 (own carrier C2).
        a(row)(idx(Body.S2)) =  ts2
        a(row)(idx(Body.R2)) =  tr2
        a(row)(idx(Body.C2)) = -(ts2 + tr2)
        row += 1
        // Two coupling closure rows.
        emitCouplingRow(topology.firstCoupling, ts2, tp2)
        emitCouplingRow(topology.secondCoupling, ts2, tp2)
      else
        // NSS: single combined stage-2 constraint (sun absent, ωP eliminated
        // via stage-1 mesh identity). Carrier is shared — modelled as C1.
        a(row)(idx(Body.S1)) =  tp2 * ts1
        a(row)(idx(Body.R2)) =  tp1 * tr2
        a(row)(idx(Body.C1)) = -(tp1 * tr2 + tp2 * ts1)
        row += 1

    // Pin rows: Input = omegaIn, Fixed = 0.
    a(row)(idx(topology.input)) = 1.0
    b(row) = omegaIn
    row += 1
    a(row)(idx(topology.fixed)) = 1.0
    b(row) = 0.0
    row += 1

    // Internal consistency: the number of equations built must match the
    // number of unknowns. If it doesn't, the topology is structurally
    // inconsistent — return `None` rather than throwing, keeping the solver
    // total. (Should be unreachable for any `Topology` that passed its own
    // construction invariants.)
    if row != n then return None

    LinAlg.solveDetailed(a, b).map { case (x, minPivot) =>
      val velocityMap = idx.map { case (body, i) => body -> x(i) }
      (Velocities(velocityMap), minPivot)
    }

  /** Forward ratio `ω_in / ω_out`. Returns `None` for kinematically singular
    * topologies (the linear solver returned None) or for a vanishing output
    * velocity (infinite reduction). */
  def ratio(topology: Topology, stage1: GearSet, stage2: Option[GearSet]): Option[Ratio] =
    velocities(topology, stage1, stage2, omegaIn = 1.0).flatMap { v =>
      val out = v.get(topology.output)
      if math.abs(out) < 1e-15 then None else Some(Ratio(1.0 / out))
    }

  /** Ratio + a flag indicating whether the linear system was well-conditioned.
    * `wellConditioned == false` means the smallest pivot during Gauss
    * elimination dropped below 1e-6 — the ratio is still well-defined
    * mathematically but its numeric value may be noisier than usual. Callers
    * can surface this as a `Validity.Warning` rather than silently presenting
    * a wobbly number. */
  def ratioWithConditioning(
      topology: Topology, stage1: GearSet, stage2: Option[GearSet]
  ): Option[(Ratio, Boolean)] =
    velocitiesDetailed(topology, stage1, stage2, omegaIn = 1.0).flatMap { case (v, minPivot) =>
      val out = v.get(topology.output)
      if math.abs(out) < 1e-15 then None
      else Some((Ratio(1.0 / out), minPivot >= 1e-6))
    }

  /** Like `velocities` but also returns the smallest pivot magnitude. */
  def velocitiesDetailed(
      topology: Topology, stage1: GearSet, stage2: Option[GearSet], omegaIn: Double
  ): Option[(Velocities, Double)] =
    solveSystem(topology, stage1, stage2, omegaIn)
