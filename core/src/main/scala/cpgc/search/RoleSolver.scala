package cpgc.search

import cpgc.constraints.{Body, Coupling, Topology}
import cpgc.domain.{GearSet, Ratio}

/** Forward kinematic solver for a `Topology` over given tooth counts (ADR 0015 §3).
  *
  * Unknowns, in this order (see `Topology.bodySlot`): the bodies that are
  * present among `S1, C1, R1, S2, C2, R2`, then the planet velocities `P1`
  * (and `P2` for a compound). An Absent sun is simply not an unknown and has no
  * mesh row; its stage keeps the ring mesh only.
  *
  * Rows, per stage with sun `S`, planet `P`, carrier `C`, ring `R`:
  *
  *   sun mesh (only if the sun is present):  Ts·(ωS − ωC) + Tp·(ωP − ωC) = 0
  *   ring mesh:                               Tr·(ωR − ωC) − Tp·(ωP − ωC) = 0
  *
  * then two coupling rows (`PlanetStack` is `ωP1 − ωP2 = 0`, any other coupling
  * a weld `ω_a − ω_b = 0`), then the pins (Input = `omegaIn`, Fixed = 0).
  *
  * **Degree-of-freedom accounting.** With `a` absent suns the compound has
  * `6 − a + 2` unknowns and `(4 − a) + 2 + 2` rows; a single stage has
  * `3 + 1` unknowns and `2 + 2` rows. The counts match for every legal
  * topology, so the system is square. A degenerate tuple gives a singular
  * system and `None`; the solver is total and never throws.
  */
object RoleSolver:

  /** Angular velocities of every body and of both planets. Bodies that are
    * Absent (or don't exist) return 0.0. For a single stage `omegaP2 == omegaP1`. */
  final case class Velocities(values: Map[Body, Double], omegaP1: Double, omegaP2: Double):
    def get(b: Body): Double = values.getOrElse(b, 0.0)
    def omegaS1: Double = get(Body.S1)
    def omegaC1: Double = get(Body.C1)
    def omegaR1: Double = get(Body.R1)
    def omegaS2: Double = get(Body.S2)
    def omegaC2: Double = get(Body.C2)
    def omegaR2: Double = get(Body.R2)
    /** Stage-2 carrier velocity, falling back to C1 for a single stage. */
    def omegaC2OrC1: Double = if values.contains(Body.C2) then get(Body.C2) else get(Body.C1)

  /** Solve for every velocity given `omegaIn` at the Input body and 0 at the
    * Fixed body. `None` when the system is singular or a compound is given no
    * stage-2 gear set. */
  def velocities(
      topology: Topology,
      stage1: GearSet,
      stage2: Option[GearSet],
      omegaIn: Double
  ): Option[Velocities] = velocitiesDetailed(topology, stage1, stage2, omegaIn).map(_._1)

  /** Like `velocities` but also returns the smallest pivot magnitude. */
  def velocitiesDetailed(
      topology: Topology, stage1: GearSet, stage2: Option[GearSet], omegaIn: Double
  ): Option[(Velocities, Double)] =
    val x = solveRaw(topology, stage1, stage2, omegaIn)
    if x == null then None
    else
      val n = topology.unknowns
      val vals = topology.presentBodies.iterator.map(b => b -> x(topology.bodySlot(b.ordinal))).toMap
      val p1 = x(topology.p1Slot)
      val p2 = if topology.p2Slot >= 0 then x(topology.p2Slot) else p1
      Some((Velocities(vals, p1, p2), x(n)))

  /** Forward ratio `ω_in / ω_out`. `None` for a singular system or a vanishing
    * output velocity (infinite reduction). */
  def ratio(topology: Topology, stage1: GearSet, stage2: Option[GearSet]): Option[Ratio] =
    val x = solveRaw(topology, stage1, stage2, 1.0)
    if x == null then None
    else
      val out = x(topology.bodySlot(topology.output.ordinal))
      if math.abs(out) >= 1e-15 then Some(Ratio.unsafe(1.0 / out)) else None

  /** Ratio + a flag saying whether the elimination was well-conditioned
    * (smallest pivot >= 1e-6; otherwise the value may be numerically noisy). */
  def ratioWithConditioning(
      topology: Topology, stage1: GearSet, stage2: Option[GearSet]
  ): Option[(Ratio, Boolean)] =
    val x = solveRaw(topology, stage1, stage2, 1.0)
    if x == null then None
    else
      val out = x(topology.bodySlot(topology.output.ordinal))
      if math.abs(out) >= 1e-15 then Some((Ratio.unsafe(1.0 / out), x(topology.unknowns) >= 1e-6))
      else None

  /** Assemble and solve. Returns the solution vector (length `unknowns + 1`,
    * the last element being the smallest pivot) or `null` when unsolvable. Two
    * array allocations per call; no maps. */
  private def solveRaw(
      t: Topology, g1: GearSet, g2: Option[GearSet], omegaIn: Double
  ): Array[Double] =
    val compound = t.stages == 2
    if compound && g2.isEmpty then return null
    val n   = t.unknowns
    val m   = new Array[Double](n * n)
    val r   = new Array[Double](n + 1)
    val sl  = t.bodySlot

    var row = meshRows(m, n, 0, sl(Body.S1.ordinal), sl(Body.R1.ordinal), sl(Body.C1.ordinal), t.p1Slot,
      g1.sun.toDouble, g1.planet.toDouble, g1.ring.toDouble)

    if compound then
      val s2 = g2.get
      row = meshRows(m, n, row, sl(Body.S2.ordinal), sl(Body.R2.ordinal), sl(Body.C2.ordinal), t.p2Slot,
        s2.sun.toDouble, s2.planet.toDouble, s2.ring.toDouble)
      row = couplingRow(m, n, row, t.firstCoupling, t)
      row = couplingRow(m, n, row, t.secondCoupling, t)

    m(row * n + sl(t.input.ordinal)) = 1.0
    r(row) = omegaIn
    row += 1
    m(row * n + sl(t.fixed.ordinal)) = 1.0
    row += 1

    // Unreachable for a Topology that passed its invariants; stay total anyway.
    if row != n then return null
    val minPivot = LinAlg.solveFlat(m, r, n)
    if minPivot < 0.0 then null
    else
      r(n) = minPivot
      r

  /** Mesh rows of one stage starting at `row0`; returns the next free row.
    * `sunSlot < 0` means the sun is absent (no sun mesh). */
  private def meshRows(
      m: Array[Double], n: Int, row0: Int,
      sunSlot: Int, ringSlot: Int, carrierSlot: Int, planetSlot: Int,
      ts: Double, tp: Double, tr: Double
  ): Int =
    var row = row0
    if sunSlot >= 0 then
      m(row * n + sunSlot)     = ts
      m(row * n + planetSlot)  = tp
      m(row * n + carrierSlot) = -(ts + tp)
      row += 1
    m(row * n + ringSlot)    = tr
    m(row * n + planetSlot)  = -tp
    m(row * n + carrierSlot) = tp - tr
    row + 1

  /** One coupling row: `PlanetStack` is `ωP1 − ωP2 = 0`, any other coupling a
    * plain weld of its body pair. */
  private def couplingRow(m: Array[Double], n: Int, row: Int, c: Coupling, t: Topology): Int =
    c match
      case Coupling.PlanetStack =>
        m(row * n + t.p1Slot) = 1.0
        m(row * n + t.p2Slot) = -1.0
      case other =>
        val (a, b) = Coupling.bodyPair(other).get
        m(row * n + t.bodySlot(a.ordinal)) = 1.0
        m(row * n + t.bodySlot(b.ordinal)) = -1.0
    row + 1
