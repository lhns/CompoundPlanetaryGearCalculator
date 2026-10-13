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
  *
  * The search hot path uses a `Workspace`, which owns the matrix and
  * right-hand-side arrays and reuses them for every solve (no allocation per
  * call). The functions below create a throw-away workspace per call.
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

  /** Reusable solver state for one topology. Not thread-safe: use one per
    * thread / per search. After a successful `solve`, `x(i)` reads the
    * solution and `minPivot` the smallest pivot. */
  final class Workspace(t: Topology):
    private val compound = t.stages == 2
    private val n   = t.unknowns
    private val m   = new Array[Double](n * n)
    private val r   = new Array[Double](n)
    private val sl  = t.bodySlot
    private val s1S = sl(Body.S1.ordinal); private val r1S = sl(Body.R1.ordinal); private val c1S = sl(Body.C1.ordinal)
    private val s2S = sl(Body.S2.ordinal); private val r2S = sl(Body.R2.ordinal); private val c2S = sl(Body.C2.ordinal)
    // Coupling rows: columns of the +1 and -1 entries.
    private val cpA = Array(couplingCol(t.firstCoupling, true), couplingCol(t.secondCoupling, true))
    private val cpB = Array(couplingCol(t.firstCoupling, false), couplingCol(t.secondCoupling, false))
    private val inSlot  = sl(t.input.ordinal)
    private val fixSlot = sl(t.fixed.ordinal)
    private val outSlot = sl(t.output.ordinal)
    private var pivot = 0.0

    private def couplingCol(c: Coupling, first: Boolean): Int =
      if !compound then -1
      else if c == Coupling.PlanetStack then (if first then t.p1Slot else t.p2Slot)
      else
        val (a, b) = Coupling.bodyPair(c).get
        sl((if first then a else b).ordinal)

    /** Solve for `omegaIn` at the Input body and 0 at the Fixed body. `g2` is
      * ignored for a single stage; `null` for a compound makes this return
      * false. Returns false for a singular system. */
    def solve(g1: GearSet, g2: GearSet, omegaIn: Double): Boolean =
      if compound && g2 == null then return false
      java.util.Arrays.fill(m, 0.0)
      java.util.Arrays.fill(r, 0.0)
      var row = mesh(0, s1S, r1S, c1S, t.p1Slot, g1)
      if compound then
        row = mesh(row, s2S, r2S, c2S, t.p2Slot, g2)
        var k = 0
        while k < 2 do
          m(row * n + cpA(k)) = 1.0
          m(row * n + cpB(k)) = -1.0
          row += 1
          k += 1
      m(row * n + inSlot) = 1.0
      r(row) = omegaIn
      row += 1
      m(row * n + fixSlot) = 1.0
      row += 1
      if row != n then return false // unreachable for a legal Topology; stay total
      pivot = LinAlg.solveFlat(m, r, n)
      pivot >= 0.0

    /** Mesh rows of one stage starting at `row0`; returns the next free row. */
    private def mesh(row0: Int, sunSlot: Int, ringSlot: Int, carrierSlot: Int, planetSlot: Int, g: GearSet): Int =
      val ts = g.sun.toDouble; val tp = g.planet.toDouble; val tr = g.ring.toDouble
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

    def x(slot: Int): Double = r(slot)
    def output: Double = r(outSlot)
    def minPivot: Double = pivot

    /** Ratio `1 / ω_out` after a successful solve, or NaN for a vanishing output. */
    def ratioOrNaN: Double =
      val out = r(outSlot)
      if math.abs(out) >= 1e-15 then 1.0 / out else Double.NaN

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
    val w = new Workspace(topology)
    if !w.solve(stage1, stage2.orNull, omegaIn) then None
    else
      val vals = topology.presentBodies.iterator.map(b => b -> w.x(topology.bodySlot(b.ordinal))).toMap
      val p1 = w.x(topology.p1Slot)
      val p2 = if topology.p2Slot >= 0 then w.x(topology.p2Slot) else p1
      Some((Velocities(vals, p1, p2), w.minPivot))

  /** Forward ratio `ω_in / ω_out`. `None` for a singular system or a vanishing
    * output velocity (infinite reduction). */
  def ratio(topology: Topology, stage1: GearSet, stage2: Option[GearSet]): Option[Ratio] =
    val w = new Workspace(topology)
    if !w.solve(stage1, stage2.orNull, 1.0) then None
    else
      val q = w.ratioOrNaN
      if q.isNaN then None else Some(Ratio.unsafe(q))

  /** Ratio + a flag saying whether the elimination was well-conditioned
    * (smallest pivot >= 1e-6; otherwise the value may be numerically noisy). */
  def ratioWithConditioning(
      topology: Topology, stage1: GearSet, stage2: Option[GearSet]
  ): Option[(Ratio, Boolean)] =
    val w = new Workspace(topology)
    if !w.solve(stage1, stage2.orNull, 1.0) then None
    else
      val q = w.ratioOrNaN
      if q.isNaN then None else Some((Ratio.unsafe(q), w.minPivot >= 1e-6))
