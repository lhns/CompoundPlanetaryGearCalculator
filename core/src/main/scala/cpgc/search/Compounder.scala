package cpgc.search

import cpgc.constraints.{SearchConfig, Topology}
import cpgc.domain.*
import cpgc.physics.Efficiency

/** Builds designs (gear sets + modules + ratio) for `cfg.topology`.
  *
  * The ratio comes from the single source of truth, `RoleSolver`; every rule
  * (assembly, stacking, efficiency) derives from the topology (ADR 0015). A
  * single stage is the same solver with `stage2 = None`.
  *
  * Candidate checks live in `Evaluator`, which the search engine reuses for
  * every pair so the hot path allocates nothing; `compound` / `single` are the
  * one-shot wrappers used by tests and callers.
  */
object Compounder:

  /** First-stage module (ADR 0016): the OD bounds the ring's outside, so
    * `m1 = (OD − 2·ringWall) / (Tr1 + 2.5)`. `None` when that is not a positive
    * finite number (never throws). */
  def firstModule(cfg: SearchConfig, s1: GearSet): Option[Module] =
    val m = firstModuleValue(cfg, s1)
    if m > 0.0 && m.isFinite then Some(Module.unsafe(m)) else None

  private def firstModuleValue(cfg: SearchConfig, s1: GearSet): Double =
    (cfg.firstStageOD - 2.0 * cfg.ringWall) / (s1.ring.toDouble + 2.5)

  /** Modules for both stages. A stacked pinion bridges both stages, so the
    * centre distance must match: `m2 = m1 · (S1 + P1) / (S2 + P2)` (virtual
    * suns for sun-less stages). Otherwise `m2 = m1`. */
  def matchModules(
      cfg: SearchConfig, s1: GearSet, s2: GearSet, stacked: Boolean
  ): Option[(Module, Module)] =
    firstModule(cfg, s1).flatMap { m1 =>
      if !stacked then Some((m1, m1))
      else
        val m2 = secondModuleValue(m1.value, s1, s2)
        if m2 > 0.0 && m2.isFinite then Some((m1, Module.unsafe(m2))) else None
    }

  private def secondModuleValue(m1: Double, s1: GearSet, s2: GearSet): Double =
    m1 * (s1.sun.toDouble + s1.planet.toDouble) / (s2.sun.toDouble + s2.planet.toDouble)

  private def gcd(a: Int, b: Int): Int = if b == 0 then math.abs(a) else gcd(b, a % b)

  /** Cross term of the stacked-pinion assembly rule (ADR 0018):
    * `((Tp1·Tr2 − Tp2·Tr1) / gcd(Tp1, Tp2)) % N == 0`. Needed when every planet
    * is the same part; uses ring teeth, so virtual suns are irrelevant. */
  def stackedCrossOk(s1: GearSet, s2: GearSet): Boolean =
    val p1 = s1.planet.toInt; val p2 = s2.planet.toInt
    ((p1.toLong * s2.ring.toInt - p2.toLong * s1.ring.toInt) / gcd(p1, p2)) % s1.numPlanets == 0

  /** Planet assembly rule (equal spacing). Every stage whose sun is present
    * needs `(S + R) % N == 0`; a sun-less stage has no such condition
    * (ADR 0015 §5). Stacked topologies with identical pinions (the default,
    * `phasedPinions = false`) also need the cross term (ADR 0018). Callers
    * guarantee `N1 == N2` for stacked topologies. */
  def satisfiesSpacing(
      topology: Topology, phasedPinions: Boolean, s1: GearSet, s2: Option[GearSet]
  ): Boolean =
    (!topology.s1Present || s1.evenSpacing) &&
      s2.forall(g => !topology.s2Present || g.evenSpacing) &&
      (phasedPinions || !topology.stacked || s2.forall(stackedCrossOk(s1, _)))

  /** Reusable candidate checker for one config. After `accept` returns true,
    * `ratio`, `wellConditioned`, `module1` and `module2` describe the candidate.
    * Not thread-safe; allocation-free per call. */
  final class Evaluator(cfg: SearchConfig):
    private val topology = cfg.topology
    private val ws       = new RoleSolver.Workspace(topology)
    var ratio: Double            = 0.0
    var wellConditioned: Boolean = true
    var module1: Double          = 0.0
    var module2: Double          = 0.0

    /** Check stacking, assembly, modules and solvability of the pair
      * (`s2 == null` for a single stage). Cheap rejects come first. */
    def accept(s1: GearSet, s2: GearSet): Boolean =
      if topology.stacked && s2 != null && s2.numPlanets != s1.numPlanets then return false
      if (topology.s1Present && !s1.evenSpacing) || (s2 != null && topology.s2Present && !s2.evenSpacing) then return false
      if s2 != null && !cfg.phasedPinions && topology.stacked && !stackedCrossOk(s1, s2) then return false
      val m1 = firstModuleValue(cfg, s1)
      if !(m1 > 0.0 && m1.isFinite) || m1 < cfg.minModule then return false
      var m2 = m1
      if s2 != null && topology.stacked then
        m2 = secondModuleValue(m1, s1, s2)
        if !(m2 > 0.0 && m2.isFinite) then return false
      if m2 < cfg.minModule then return false
      if !ws.solve(s1, s2, 1.0) then return false
      val q = ws.ratioOrNaN
      if q.isNaN then return false
      ratio = q
      wellConditioned = ws.minPivot >= 1e-6
      module1 = m1
      module2 = m2
      true

  /** The design for a candidate `Evaluator.accept` approved. */
  def design(cfg: SearchConfig, s1: GearSet, s2: Option[GearSet], ev: Evaluator): CompoundDesign =
    design(cfg, s1, s2, ev.ratio, ev.wellConditioned, ev.module1, ev.module2)

  /** Same, from a captured evaluator result (lets the engine defer materialisation). */
  def design(
      cfg: SearchConfig, s1: GearSet, s2: Option[GearSet],
      ratio: Double, wellConditioned: Boolean, module1: Double, module2: Double
  ): CompoundDesign =
    val topology = cfg.topology
    val r = Ratio.unsafe(ratio)
    val validity = s2 match
      case Some(g) => classifyDegeneracies(topology, s1, g, ratio, wellConditioned)
      case None    => if wellConditioned then Validity.Ok else Validity.Warning(NearSingular)
    CompoundDesign(
      stage1           = Stage(s1, Module.unsafe(module1), cfg.ringWall),
      stage2           = s2.map(g => Stage(g, Module.unsafe(module2), cfg.ringWall)),
      topology         = topology,
      ratio            = r,
      efficiency       = Efficiency.overall(topology),
      spacingOk        = true,  // gated by `accept`
      profileShiftHint = None,  // standard-module designs match exactly
      validity         = validity
    )

  /** Pair two enumerated stages into a compound design. */
  def compound(cfg: SearchConfig, s1: GearSet, s2: GearSet): Option[CompoundDesign] =
    val ev = new Evaluator(cfg)
    if cfg.topology.stages == 2 && ev.accept(s1, s2) then Some(design(cfg, s1, Some(s2), ev)) else None

  /** Build a single-stage design (topology with `stages == 1`). */
  def single(cfg: SearchConfig, s: GearSet): Option[CompoundDesign] =
    val ev = new Evaluator(cfg)
    if cfg.topology.stages == 1 && ev.accept(s, null) then Some(design(cfg, s, None, ev)) else None

  private val NearSingular = "near-singular kinematics — ratio numerically noisy"

  /** Detect compound-as-degenerate cases by perturbing one stage's gear counts
    * and re-solving. The compound is **stage-N-decoupled** if perturbing that
    * stage's gears leaves the ratio unchanged: stage N is along for the ride.
    * This runtime probe is the decoupling signal (B11: the symbolic body-subset
    * check was removed). Trivial passthrough is `|ratio − 1| < ε`.
    *
    * Priority: stage-2 decoupled, stage-1 decoupled, trivial passthrough,
    * near-singular conditioning, Ok. One extra `RoleSolver.ratio` per check.
    */
  private def classifyDegeneracies(
      topology: Topology, s1: GearSet, s2: GearSet,
      ratio: Double, wellConditioned: Boolean
  ): Validity =
    val eps = 1e-9

    // Bump the planet by one (the ring recomputes via r = s + 2p).
    def perturb(g: GearSet): GearSet = GearSet(g.sun, Teeth(g.planet.toInt + 1), g.numPlanets)

    def ratioOf(a: GearSet, b: GearSet): Option[Double] =
      RoleSolver.ratio(topology, a, Some(b)).map(_.value)

    val s2Decoupled = ratioOf(s1, perturb(s2)).exists(r2 => math.abs(r2 - ratio) < eps)
    if s2Decoupled then
      return Validity.Warning("stage-2 decoupled — ratio doesn't depend on stage-2 gears (single-stage equivalent)")

    val s1Decoupled = ratioOf(perturb(s1), s2).exists(r2 => math.abs(r2 - ratio) < eps)
    if s1Decoupled then
      return Validity.Warning("stage-1 decoupled — ratio doesn't depend on stage-1 gears")

    if math.abs(ratio - 1.0) < eps then
      return Validity.Warning("trivial passthrough — output speed equals input")

    if !wellConditioned then Validity.Warning(NearSingular) else Validity.Ok
