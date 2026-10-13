package cpgc.search

import cpgc.constraints.{Body, Coupling, SearchConfig, Topology}
import cpgc.domain.*
import cpgc.physics.{Efficiency, PlanetSpacing}

/** Builds designs (gear sets + modules + ratio) for `cfg.topology`.
  *
  * The ratio comes from the single source of truth, `RoleSolver.ratio`; every
  * rule (spacing, stacking, efficiency) derives from the topology (ADR 0015).
  * A single stage is the same solver with `stage2 = None`.
  */
object Compounder:

  /** First-stage module (ADR 0016): the OD bounds the ring's outside, so
    * `m1 = (OD − 2·ringWall) / (Tr1 + 2.5)`. `None` when that is not a positive
    * finite number (never throws). */
  def firstModule(cfg: SearchConfig, s1: GearSet): Option[Module] =
    val m = (cfg.firstStageOD - 2.0 * cfg.ringWall) / (s1.ring.toDouble + 2.5)
    if m > 0.0 && m.isFinite then Some(Module.unsafe(m)) else None

  /** Modules for both stages. A stacked pinion bridges both stages, so the
    * centre distance must match: `m2 = m1 · (S1 + P1) / (S2 + P2)` (virtual
    * suns for sun-less stages). Otherwise `m2 = m1`. */
  def matchModules(
      cfg: SearchConfig, s1: GearSet, s2: GearSet, stacked: Boolean
  ): Option[(Module, Module)] =
    firstModule(cfg, s1).flatMap { m1 =>
      if !stacked then Some((m1, m1))
      else
        val c2c1 = s1.sun.toDouble + s1.planet.toDouble
        val c2c2 = s2.sun.toDouble + s2.planet.toDouble
        val m2   = m1.value * c2c1 / c2c2
        if m2 > 0.0 && m2.isFinite then Some((m1, Module.unsafe(m2))) else None
    }

  /** Planet-spacing rule (ADR 0015 §5): every stage whose sun is present needs
    * `(S + R) % N == 0`; a sun-less stage has no condition of its own. */
  def satisfiesSpacing(topology: Topology, s1: GearSet, s2: Option[GearSet]): Boolean =
    (!topology.s1Present || s1.evenSpacing) &&
      s2.forall(g => !topology.s2Present || g.evenSpacing)

  /** Build a design for `cfg.topology`; `None` if it fails stacking, spacing,
    * module, or has no finite ratio. */
  private def build(cfg: SearchConfig, s1: GearSet, s2: Option[GearSet]): Option[CompoundDesign] =
    val topology = cfg.topology
    if topology.stacked && s2.exists(_.numPlanets != s1.numPlanets) then None
    else if !satisfiesSpacing(topology, s1, s2) then None
    else
      val modules: Option[(Module, Option[Module])] = s2 match
        case Some(g) => matchModules(cfg, s1, g, topology.stacked).map((a, b) => (a, Some(b)))
        case None    => firstModule(cfg, s1).map((_, None))
      modules.flatMap { (m1, m2) =>
        if m1.value < cfg.minModule || m2.exists(_.value < cfg.minModule) then None
        else
          RoleSolver.ratioWithConditioning(topology, s1, s2).map { (r, wellConditioned) =>
            val validity =
              s2.fold(if wellConditioned then Validity.Ok else Validity.Warning(NearSingular))(
                classifyDegeneracies(topology, s1, _, r.value, wellConditioned))
            CompoundDesign(
              stage1           = Stage(s1, m1, cfg.ringWall),
              stage2           = for g <- s2; m <- m2 yield Stage(g, m, cfg.ringWall),
              topology         = topology,
              ratio            = r,
              efficiency       = Efficiency.overall(topology),
              spacingOk        = true,  // gated above
              profileShiftHint = None,  // standard-module designs match exactly
              validity         = validity
            )
          }
      }

  /** Pair two enumerated stages into a compound design. */
  def compound(cfg: SearchConfig, s1: GearSet, s2: GearSet): Option[CompoundDesign] =
    if cfg.topology.stages == 2 then build(cfg, s1, Some(s2)) else None

  /** Build a single-stage design (topology with `stages == 1`). */
  def single(cfg: SearchConfig, s: GearSet): Option[CompoundDesign] =
    if cfg.topology.stages == 1 then build(cfg, s, None) else None

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

  /** Given a stage-1 candidate and a target ratio, derive stage-2 candidates
    * with a ratio close to `target`. Canonical Standard / NSS / CDCP use a
    * closed form; `CarrierToCarrier + RingToSun` with the standard roles has its
    * own; everything else falls back to a windowed brute force. (ADR 0017
    * replaces all of this with the exhaustive search in milestone B.)
    *
    * Closed forms (S1 in or C1 in, R1 fixed, R2 out, shared carrier, stacked):
    *   Standard / NSS: Tr2 = T·Ts1·Tr1·Tp2 / (Tp1·(Ts1·(T−1) − Tr1))
    *   CDCP:           Tr2 = T·Tr1·Tp2 / (Tp1·(T − 1))
    * For each Tp2 in the planet range, round Tr2, derive Ts2 = Tr2 − 2·Tp2 and
    * check the ranges, planet fit and the topology's spacing rules.
    */
  def stage2For(target: Double, stage1: GearSet, cfg: SearchConfig): Iterator[GearSet] =
    val topology = cfg.topology
    if topology.stages != 2 then Iterator.empty
    else if topology.legacyMode.isDefined then stage2ForCanonical(target, stage1, cfg)
    else if topology.firstCoupling == Coupling.CarrierToCarrier &&
      topology.secondCoupling == Coupling.RingToSun &&
      topology.s1Present && topology.s2Present &&
      topology.input == Body.S1 && topology.output == Body.R2 && topology.fixed == Body.R1
    then stage2ForRingToSun(target, stage1, cfg)
    else stage2ForBruteForce(target, stage1, cfg)

  /** Stage-2 gear set `(Ts2, Tp2)` if it passes the pool rules of stage 2. */
  private def stage2Candidate(cfg: SearchConfig, stage1: GearSet, ts2: Int, tp2: Int): Option[GearSet] =
    val topology = cfg.topology
    val sunOk =
      if topology.s2Present then
        ts2 >= cfg.sunTeeth.min && ts2 <= cfg.sunTeeth.max &&
          tp2.toDouble < cfg.sunPlanetFactor * ts2.toDouble
      else ts2 >= 1
    if !sunOk then None
    else
      val set = GearSet(Teeth(ts2), Teeth(tp2), stage1.numPlanets)
      if !PlanetSpacing.planetsFitWithoutOverlap(set, cfg.planetClearance) then None
      else if !satisfiesSpacing(topology, stage1, Some(set)) then None
      else Some(set)

  /** Closed-form inverse for `CarrierToCarrier + RingToSun` with the standard
    * roles: `T = Tr2·(Ts1 + Tr1) / ((Ts2 + Tr2)·Ts1)` with `Ts2 = Tr2 − 2·Tp2`
    * gives `Tr2 = 2·T·Ts1·Tp2 / (Ts1·(2T − 1) − Tr1)`. */
  private def stage2ForRingToSun(
      target: Double, stage1: GearSet, cfg: SearchConfig
  ): Iterator[GearSet] =
    val ts1 = stage1.sun.toDouble
    val tr1 = stage1.ring.toDouble
    val denom = ts1 * (2.0 * target - 1.0) - tr1
    if denom == 0.0 then Iterator.empty
    else
      cfg.planetTeeth.range.iterator.flatMap { tp2 =>
        val tr2 = math.round(2.0 * target * ts1 * tp2 / denom).toInt
        if tr2 < cfg.ringTeeth.min || tr2 > cfg.ringTeeth.max then Iterator.empty
        else stage2Candidate(cfg, stage1, tr2 - 2 * tp2, tp2).iterator
      }

  /** Closed-form inverse for the canonical Standard / NSS / CDCP. */
  private def stage2ForCanonical(
      target: Double, stage1: GearSet, cfg: SearchConfig
  ): Iterator[GearSet] =
    val ts1 = stage1.sun.toDouble
    val tp1 = stage1.planet.toDouble
    val tr1 = stage1.ring.toDouble
    val cdcp = cfg.topology.input == Body.C1
    if target == 0.0 || tp1 == 0.0 then Iterator.empty
    else
      val denom      = if cdcp then tp1 * (target - 1.0) else tp1 * (ts1 * (target - 1.0) - tr1)
      val numerCommon = if cdcp then target * tr1 else target * ts1 * tr1
      if denom == 0.0 then Iterator.empty
      else
        cfg.planetTeeth.range.iterator.flatMap { tp2 =>
          val tr2 = math.round(numerCommon * tp2 / denom).toInt
          if tr2 < cfg.ringTeeth.min || tr2 > cfg.ringTeeth.max then Iterator.empty
          else stage2Candidate(cfg, stage1, tr2 - 2 * tp2, tp2).iterator
        }

  /** Windowed brute force for topologies with no closed form: every stage-2
    * candidate whose forward ratio is within ±10 % (+0.5) of `target`. */
  private def stage2ForBruteForce(
      target: Double, stage1: GearSet, cfg: SearchConfig
  ): Iterator[GearSet] =
    val tolerance = math.abs(target) * 0.10 + 0.5
    Enumerator.stage2Sets(cfg).iterator.flatMap { s2 =>
      RoleSolver.ratio(cfg.topology, stage1, Some(s2)) match
        case Some(r) if math.abs(r.value - target) < tolerance => Iterator.single(s2)
        case _                                                  => Iterator.empty
    }
