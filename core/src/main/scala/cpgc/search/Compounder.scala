package cpgc.search

import cpgc.constraints.{Coupling, SearchConfig, Topology}
import cpgc.domain.*
import cpgc.physics.{Efficiency, PlanetSpacing}

/** Pairs single-stage gear sets into compound designs and computes their ratios.
  *
  * The compound ratio is computed by the **single source of truth**,
  * `RoleSolver.ratio` (the linear kinematic solver). The legacy
  * `compoundRatio` closed form was deleted — a dormant second formula that
  * the production path never used was a guaranteed source of drift
  * (Edge-Case Catalogue #9). Callers that want a forward ratio go through
  * `RoleSolver.ratio(Topology.fromMode(mode), s1, Some(s2))`. */
object Compounder:

  /** Single-stage reduction (sun input, ring fixed, carrier output). */
  def singleStageRatio(s: GearSet): Ratio =
    Ratio(1.0 / s.sunCarrierRatio)

  /** Choose the modules for both stages.
    *
    *  - For the **stacked-planet** coupling (the classic compound), a single
    *    physical pinion bridges both stages, so the carrier center-to-center
    *    distance must be identical: `m2 = m1 · (c2c1 / c2c2)`.
    *  - For any other coupling (rings or suns welded on a shared shaft, no
    *    inter-stage planet sharing), each stage's planets are independent and
    *    the modules can differ freely. We default to `m2 = m1` so the user's
    *    `firstStageOD` still anchors stage 1; stage 2's ring diameter is then
    *    `m1 · (r2 + 2)` which simply floats.
    */
  def matchModules(cfg: SearchConfig, s1: GearSet, s2: GearSet): (Module, Module) =
    matchModules(cfg, s1, s2, Coupling.PlanetStack)

  def matchModules(
      cfg: SearchConfig, s1: GearSet, s2: GearSet, coupling: Coupling
  ): (Module, Module) =
    val m1 = cfg.firstStageOD / (s1.ring.toDouble + 2.0)
    if Coupling.stacksPlanets(coupling) then
      val c2c1 = s1.sun.toDouble + s1.planet.toDouble
      val c2c2 = s2.sun.toDouble + s2.planet.toDouble
      // Guard the division: `c2c2` is zero only for a degenerate stage 2 with
      // no sun and no planet teeth (unreachable for a valid GearSet, but the
      // method is public). Fall back to `m2 = m1` rather than emit Inf/NaN.
      // (Edge-Case Catalogue #8.)
      val m2 = if c2c2 > 0.0 then m1 * (c2c1 / c2c2) else m1
      (Module(m1), Module(m2))
    else
      (Module(m1), Module(m1))

  /** Build a `CompoundDesign` for two enumerated stages, applying module matching,
    * efficiency and profile-shift hints. Returns `None` if the design fails
    * minimum-module or singular-ratio constraints.
    */
  def compound(cfg: SearchConfig, s1: GearSet, s2: GearSet): Option[CompoundDesign] =
    val topology = cfg.effectiveTopology
    // A stacked-planet compound has a single physical pinion bridging both
    // stages, so the planet counts MUST match. Without this guard the solver
    // would happily return a kinematically-valid but physically-impossible
    // ratio for N1 ≠ N2. (Edge-Case Catalogue #6.)
    val stackedPlanetMismatch =
      Coupling.stacksPlanets(topology.secondCoupling) && s1.numPlanets != s2.numPlanets
    if stackedPlanetMismatch then None
    else if !satisfiesSpacing(cfg.mode, topology.secondCoupling, s1, Some(s2)) then None
    else
      // Route the ratio computation through `RoleSolver` so novel topologies
      // get their actual topology-respecting ratio rather than the Standard
      // fallback that `cfg.mode` would imply.
      RoleSolver.ratioWithConditioning(topology, s1, Some(s2)).flatMap { case (r, wellConditioned) =>
        val (m1, m2) = matchModules(cfg, s1, s2, topology.secondCoupling)
        if m1.value < cfg.minModule || m2.value < cfg.minModule then None
        else
          val stage1 = Stage(s1, m1)
          val stage2 = Stage(s2, m2)
          val eff = Efficiency.overall(s1, Some(s2), cfg.mode, topology.secondCoupling)
          val validity = classifyDegeneracies(topology, s1, s2, r.value, wellConditioned)
          Some(CompoundDesign(
            stage1           = stage1,
            stage2           = Some(stage2),
            mode             = cfg.mode,
            ratio            = r,
            efficiency       = eff,
            // By construction (satisfiesModeSpacing gated entry above), the
            // mode-required spacing holds; CDCP doesn't require any.
            spacingOk        = true,
            profileShiftHint = None, // standard-module designs match exactly
            validity         = validity
          ))
      }

  /** Detect compound-as-degenerate cases by perturbing one stage's gear counts
    * and re-solving. The compound is **stage-N-decoupled** if perturbing that
    * stage's gears leaves the ratio unchanged — stage N is along for the ride
    * and doesn't influence the output. Trivial passthrough is the
    * `|ratio − 1| < ε` case (output is the input speed regardless of gears).
    *
    * Priority order (most informative first):
    *   stage-2 decoupled → stage-1 decoupled → trivial passthrough →
    *   near-singular conditioning → Ok.
    *
    * One extra `RoleSolver.ratio` per stage check; ≈ µs per design.
    */
  private def classifyDegeneracies(
      topology: Topology, s1: GearSet, s2: GearSet,
      ratio: Double, wellConditioned: Boolean
  ): Validity =
    val eps = 1e-9

    /** A valid perturbation of a `GearSet`: bump `planet` by ±1 (the ring
      * recomputes automatically via `r = s + 2p`). Returns `None` if the
      * perturbed set wouldn't be solvable (e.g. planet teeth would go to ≤ 0). */
    inline def perturb(g: GearSet): Option[GearSet] =
      val p = g.planet.toInt
      if p >= 8 then Some(GearSet(g.sun, Teeth(p + 1), g.numPlanets))
      else if p >= 2 then Some(GearSet(g.sun, Teeth(p + 1), g.numPlanets))
      else None

    inline def ratioOf(a: GearSet, b: GearSet): Option[Double] =
      RoleSolver.ratio(topology, a, Some(b)).map(_.value)

    val s2Decoupled = perturb(s2).flatMap(ratioOf(s1, _)).exists(r2 => math.abs(r2 - ratio) < eps)
    if s2Decoupled then
      return Validity.Warning("stage-2 decoupled — ratio doesn't depend on stage-2 gears (single-stage equivalent)")

    val s1Decoupled = perturb(s1).flatMap(s1p => ratioOf(s1p, s2)).exists(r2 => math.abs(r2 - ratio) < eps)
    if s1Decoupled then
      return Validity.Warning("stage-1 decoupled — ratio doesn't depend on stage-1 gears")

    if math.abs(ratio - 1.0) < eps then
      return Validity.Warning("trivial passthrough — output speed equals input")

    if !wellConditioned then Validity.Warning("near-singular kinematics — ratio numerically noisy")
    else Validity.Ok

  /** Per-mode planet-spacing requirement:
    *
    *   Standard:    both stages require `(S + R) % N == 0`.
    *   NSS:         only stage 1 (stage 2 has no sun-planet mesh forcing the
    *                Willis-style two-mesh assembly condition; the remaining
    *                planet-ring mesh is automatically consistent with the
    *                stage-1-imposed orientation).
    *   CDCP:        neither stage. With the sun-carrier coupling on the
    *                input side, the sun-planet mesh phase isn't constrained
    *                across planet positions.
    *   SingleStage: the single stage requires `(S + R) % N == 0`.
    *
    * Applied here rather than in the enumerator so the same single-stage
    * candidate pool serves every mode. */
  private def satisfiesModeSpacing(
      mode: Mode, s1: GearSet, s2: Option[GearSet]
  ): Boolean =
    satisfiesSpacing(mode, Coupling.PlanetStack, s1, s2)

  /** Coupling-aware planet-spacing requirement. For the classic stacked-planet
    * compound the historical per-mode rules apply (Standard, NSS, CDCP, Single).
    * For any non-stacked coupling (rings or suns welded across stages, no
    * shared planet pinion), each stage's planets are independent and the
    * intrinsic `(S+R) % N == 0` rule applies to **both** stages — same as
    * Standard, regardless of mode. CDCP's relaxation is specifically about the
    * sun-carrier coupling decoupling stage 2's phasing; that argument only
    * holds for the stacked-planet family.
    */
  private def satisfiesSpacing(
      mode: Mode, coupling: Coupling, s1: GearSet, s2: Option[GearSet]
  ): Boolean =
    if Coupling.stacksPlanets(coupling) then mode match
      case Mode.CDCP        => true
      case Mode.NSS         => s1.evenSpacing
      case Mode.Standard    => s1.evenSpacing && s2.exists(_.evenSpacing)
      case Mode.SingleStage => s1.evenSpacing
    else
      // Novel coupling: both stages' mesh phases are independent and each
      // needs intrinsic even-spacing. Single-stage doesn't reach this branch.
      s1.evenSpacing && s2.forall(_.evenSpacing)

  /** Given a stage-1 candidate and a target ratio, **algebraically solve** for
    * the integer stage-2 tooth counts that produce a compound ratio close to
    * `target`. Replaces the O(K²) brute-force pair enumeration with an O(K · N)
    * inverse search.
    *
    * For Standard/NSS modes (sun-driven, ring-1 fixed):
    *   Tr2 = T·Ts1·Tr1·Tp2 / (Tp1·(Ts1·(T−1) − Tr1))
    *
    * For CDCP (carrier-driven, sunCarrier = 1):
    *   Tr2 = T·Tr1·Tp2 / (Tp1·(T − 1))
    *
    * For each Tp2 in the planet-teeth range we solve for Tr2, round to the
    * nearest integer, derive Ts2 = Tr2 − 2·Tp2, and check that all three fall
    * within the configured ranges and satisfy the mode's spacing rules.
    */
  def stage2For(target: Double, stage1: GearSet, cfg: SearchConfig): Iterator[GearSet] =
    val topology = cfg.effectiveTopology
    // Dispatcher: legacy four modes use the closed-form solver; the novel
    // RingToSun coupling has its own derived closed form (`stage2ForRingToSun`);
    // everything else falls through to brute-force. See ADR 0014.
    (topology.legacyMode, topology.secondCoupling) match
      case (Some(_), _) =>
        stage2ForLegacy(target, stage1, cfg)
      case (None, Coupling.RingToSun) =>
        stage2ForRingToSun(target, stage1, cfg)
      case (None, _) =>
        stage2ForBruteForce(target, stage1, cfg, topology)

  /** Closed-form inverse solver for the `RingToSun` coupling with the standard
    * role tuple (S1=Input, R2=Output, R1=Fixed). Derivation in the plan:
    *
    *   T = Tr2·(Ts1 + Tr1) / ((Ts2 + Tr2)·Ts1)
    *   with Ts2 = Tr2 − 2·Tp2 →  Tr2 = 2·T·Ts1·Tp2 / (Ts1·(2T − 1) − Tr1)
    *
    * Only valid when the role tuple matches the standard arrangement. For any
    * other (Input, Output, Fixed) tuple we fall back to brute force —
    * detected by checking the topology has the standard role assignments.
    */
  private def stage2ForRingToSun(
      target: Double, stage1: GearSet, cfg: SearchConfig
  ): Iterator[GearSet] =
    val topology = cfg.effectiveTopology
    val standardRoles =
      topology.input == cpgc.constraints.Body.S1 &&
      topology.output == cpgc.constraints.Body.R2 &&
      topology.fixed == cpgc.constraints.Body.R1
    if !standardRoles then return stage2ForBruteForce(target, stage1, cfg, topology)
    val ts1 = stage1.sun.toDouble
    val tr1 = stage1.ring.toDouble
    val denom = ts1 * (2.0 * target - 1.0) - tr1
    if denom == 0.0 then Iterator.empty
    else
      cfg.planetTeeth.range.iterator.flatMap { tp2 =>
        val tr2Real = 2.0 * target * ts1 * tp2 / denom
        val tr2     = math.round(tr2Real).toInt
        if tr2 < cfg.ringTeeth.min || tr2 > cfg.ringTeeth.max then Iterator.empty
        else
          val ts2 = tr2 - 2 * tp2
          if ts2 < cfg.sunTeeth.min || ts2 > cfg.sunTeeth.max then Iterator.empty
          else if tp2.toDouble >= cfg.sunPlanetFactor * ts2.toDouble then Iterator.empty
          else
            val set = GearSet(Teeth(ts2), Teeth(tp2), stage1.numPlanets)
            if !PlanetSpacing.planetsFitWithoutOverlap(set) then Iterator.empty
            else if !satisfiesSpacing(cfg.mode, Coupling.RingToSun, stage1, Some(set)) then Iterator.empty
            else Iterator.single(set)
      }

  /** Closed-form inverse solver for the four legacy topologies. Same code as
    * before the dispatcher landed — the formulas are derived in the doc
    * comment on the legacy `stage2For`. */
  private def stage2ForLegacy(target: Double, stage1: GearSet, cfg: SearchConfig): Iterator[GearSet] =
    val ts1 = stage1.sun.toDouble
    val tp1 = stage1.planet.toDouble
    val tr1 = stage1.ring.toDouble
    if target == 0.0 || tp1 == 0.0 then Iterator.empty
    else
      val denom = cfg.mode match
        case Mode.CDCP                        => tp1 * (target - 1.0)
        case Mode.Standard | Mode.NSS         => tp1 * (ts1 * (target - 1.0) - tr1)
        case Mode.SingleStage                 => 0.0 // unused — single stage takes no stage2
      if denom == 0.0 then Iterator.empty
      else
        val numerCommon = cfg.mode match
          case Mode.CDCP                        => target * tr1
          case Mode.Standard | Mode.NSS         => target * ts1 * tr1
          case Mode.SingleStage                 => 0.0
        cfg.planetTeeth.range.iterator.flatMap { tp2 =>
          val tr2Real = numerCommon * tp2 / denom
          val tr2     = math.round(tr2Real).toInt
          if tr2 < cfg.ringTeeth.min || tr2 > cfg.ringTeeth.max then Iterator.empty
          else
            val ts2 = tr2 - 2 * tp2
            // Standard / NSS / CDCP all derive the same `ts2 = tr2 − 2·tp2`
            // via the geometric `r = s + 2p` invariant. The sun-range check
            // applies for Standard (sun is a real driving body); CDCP and
            // NSS skip it (sun is free / absent). Per-stage Willis is
            // applied by `satisfiesModeSpacing` inside `compound()`.
            val sunInRange = cfg.mode match
              case Mode.Standard => ts2 >= cfg.sunTeeth.min && ts2 <= cfg.sunTeeth.max
              case _             => ts2 >= 1                                 // Teeth invariant
            if !sunInRange then Iterator.empty
            else if cfg.mode == Mode.Standard && tp2.toDouble >= cfg.sunPlanetFactor * ts2.toDouble then Iterator.empty
            else
              val set = GearSet(Teeth(ts2), Teeth(tp2), stage1.numPlanets)
              if !PlanetSpacing.planetsFitWithoutOverlap(set) then Iterator.empty
              // Mirror `Compounder.compound`'s per-mode spacing rule so callers
              // reading `stage2For` directly see only candidates that would also
              // survive `compound()`.
              else if !satisfiesModeSpacing(cfg.mode, stage1, Some(set)) then Iterator.empty
              else Iterator.single(set)
        }

  /** Brute-force inverse solver for novel topology tuples (no closed form
    * registered). Enumerates the full stage-2 search space and picks the
    * candidate `(P2, S2)` whose `RoleSolver.ratio` is closest to `target` for
    * each `P2`. Bounded by the existing `Enumerator.gearSets` filter chain
    * (which already respects sun/planet/ring ranges, sunPlanetFactor, and
    * planet-fit), so the iterator length is at most the size of the
    * single-stage search space — well under `ComputeCap`.
    *
    * Performance: ≈ K calls per stage-2 candidate where K is the number of
    * gear sets in the configured ranges; in practice 50–200 ms for typical
    * bounds. Roughly 10–50× slower than the closed-form path.
    */
  private def stage2ForBruteForce(
      target:   Double,
      stage1:   GearSet,
      cfg:      SearchConfig,
      topology: Topology
  ): Iterator[GearSet] =
    // Sample at most a few hundred stage-2 candidates and keep those whose
    // forward ratio comes within a generous tolerance of the target. The
    // top-K filtering is left to `Scorer` downstream.
    val tolerance = math.abs(target) * 0.10 + 0.5  // relative + small absolute
    Enumerator.gearSets(cfg).iterator.flatMap { s2 =>
      RoleSolver.ratio(topology, stage1, Some(s2)) match
        case Some(r) if math.abs(r.value - target) < tolerance => Iterator.single(s2)
        case _                                                  => Iterator.empty
    }

  /** Build a single-stage design. */
  def single(cfg: SearchConfig, s: GearSet): Option[CompoundDesign] =
    if !satisfiesModeSpacing(Mode.SingleStage, s, None) then None
    else
      val m = Module(cfg.firstStageOD / (s.ring.toDouble + 2.0))
      if m.value < cfg.minModule then None
      else
        val stage = Stage(s, m)
        Some(CompoundDesign(
          stage1           = stage,
          stage2           = None,
          mode             = Mode.SingleStage,
          ratio            = singleStageRatio(s),
          efficiency       = Efficiency.overall(s, None, Mode.SingleStage),
          spacingOk        = true,
          profileShiftHint = None
        ))
