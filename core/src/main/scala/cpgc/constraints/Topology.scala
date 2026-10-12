package cpgc.constraints

import cpgc.domain.Mode

/** Generalised role-assignment topology of a planetary or compound-planetary
  * gearbox. Each `Body` present in the train is assigned a `BodyRole`; the
  * forward solver derives every body's angular velocity from the resulting
  * linear system.
  *
  * Construction-time invariants (`require` clauses):
  *   1. `stages` is 1 or 2.
  *   2. Every body present (per `Body.present(stages, s2Present)`) has a role.
  *   3. No extra bodies have roles assigned.
  *   4. `BodyRole.Absent` is permitted only on `Body.S2`.
  *   5. Exactly one Input, one Output, one Fixed.
  *
  * These guarantee the topology is *structurally* valid. They do not guarantee
  * *solvability* — rare degenerate tuples may produce a singular linear system
  * at certain tooth counts; those surface as `None` from the forward solver.
  *
  * The legacy `Mode` enum remains available via `legacyMode` for backward
  * compatibility with code that hasn't been migrated to the role model.
  */
final case class Topology(
    stages: Int,
    roles: Map[Body, BodyRole],
    firstCoupling: Coupling = Coupling.CarrierToCarrier,
    secondCoupling: Coupling = Coupling.PlanetStack
):
  require(stages == 1 || stages == 2, s"stages must be 1 or 2, got $stages")

  // A compound is formed by *two* inter-stage welds. Two identical welds is
  // one constraint, not two — the kinematic system would be under-determined.
  // Single-stage topologies don't use the coupling fields, so they're exempt.
  // (Edge-Case Catalogue #7.)
  require(
    stages == 1 || firstCoupling != secondCoupling,
    s"compound couplings must differ — got firstCoupling == secondCoupling == $firstCoupling"
  )

  private val absent: Set[Body] =
    roles.collect { case (b, BodyRole.Absent) => b }.toSet
  require(
    absent.subsetOf(Set(Body.S2)),
    s"only S2 may be Absent, got Absent on: ${absent.mkString(", ")}"
  )

  private val expectedPresent: Set[Body] =
    Body.present(stages, s2Present = !absent.contains(Body.S2))
  require(
    expectedPresent.forall(roles.contains),
    s"missing role for present body: expected ${expectedPresent.mkString(", ")}, got ${roles.keys.mkString(", ")}"
  )
  require(
    roles.keySet.subsetOf(expectedPresent ++ absent),
    s"extra body assignment: roles=${roles.keys.mkString(", ")} but only ${expectedPresent.mkString(", ")} are present"
  )

  private val inputCount  = roles.values.count(_ == BodyRole.Input)
  private val outputCount = roles.values.count(_ == BodyRole.Output)
  private val fixedCount  = roles.values.count(_ == BodyRole.Fixed)
  require(inputCount  == 1, s"exactly one Input required, got $inputCount")
  require(outputCount == 1, s"exactly one Output required, got $outputCount")
  require(fixedCount  == 1, s"exactly one Fixed required, got $fixedCount")

  def input:  Body = roles.collectFirst { case (b, BodyRole.Input)  => b }.get
  def output: Body = roles.collectFirst { case (b, BodyRole.Output) => b }.get
  def fixed:  Body = roles.collectFirst { case (b, BodyRole.Fixed)  => b }.get
  def freeBodies: Set[Body] = roles.collect { case (b, BodyRole.Free) => b }.toSet

  /** True iff stage 2's sun gear physically exists in this topology. */
  def s2Present: Boolean = !absent.contains(Body.S2)

  /** Legacy alias retained for the duration of the role-assignment phasing. */
  def secondSunPresent: Boolean = s2Present

  /** Best-effort translation to the legacy `Mode` enum. Returns the matching
    * `Mode` for the four canonical role tuples, `None` for any of the 86 new
    * tuples. Callers that branch on Mode should be migrated to read `roles`
    * directly. */
  def legacyMode: Option[Mode] = Topology.toLegacyModeOpt(this)

  /** Defaulting legacy-Mode accessor used by call sites that haven't been
    * migrated yet (CLI URL parsing, schematic header banner). Returns
    * `Mode.Standard` for any novel tuple that has no legacy equivalent.
    * New code must use `legacyMode` or read `roles` directly. */
  @deprecated("use legacyMode or read roles directly", "round 4 D1")
  def mode: Mode = legacyMode.getOrElse(Mode.Standard)

  /** Legacy view of "which side drives the compound". S1-input → Sun-driven;
    * C-input → Carrier-driven; anything else → Sun (default for novel tuples).
    * Replaced by `input` (which returns a `Body`) in D4. */
  @deprecated("use input: Body instead", "round 4 D1")
  def inputSide: Topology.InputSide = input match
    case Body.S1 => Topology.InputSide.Sun
    case Body.C1 => Topology.InputSide.Carrier
    case _       => Topology.InputSide.Sun

  /** Build a new topology by changing the legacy `InputSide`, preserving
    * `stages` and `secondSunPresent`. */
  @deprecated("use the role-based constructor", "round 4 D1")
  def withInputSide(side: Topology.InputSide): Topology =
    Topology(stages, side, secondSunPresent)

  /** Build a new topology by toggling `secondSunPresent`, preserving the rest. */
  @deprecated("use the role-based constructor", "round 4 D1")
  def withSecondSunPresent(present: Boolean): Topology =
    Topology(stages, inputSide, present)

  /** Build a new topology by changing the stage count, preserving the rest. */
  @deprecated("use the role-based constructor", "round 4 D1")
  def withStages(n: Int): Topology =
    Topology(n, inputSide, secondSunPresent)

object Topology:

  /** Legacy compound-input enumeration retained for the duration of D1-D3 so
    * App.scala and Schematic.scala compile. D4 rewrites the form panel to use
    * `Body`-keyed role pickers and this enum disappears. */
  @deprecated("use Body + BodyRole instead", "round 4 D1")
  enum InputSide:
    case Sun, Carrier

  /** Legacy alias for code that imported `Topology.Input`. Exposed as both a
    * value (`Topology.Input.Sun`) and a type (`x: Topology.Input`). */
  @deprecated("use Topology.InputSide; D4 removes this entirely", "round 4 D1")
  val Input: InputSide.type = InputSide
  @deprecated("use Topology.InputSide", "round 4 D1")
  type Input = InputSide

  /** Legacy 3-arg constructor — `Topology(stages, InputSide, secondSunPresent)`
    * — used by App.scala until D4 lands the role-picker UI. Maps to one of the
    * four canonical legacy topologies. */
  @deprecated("use the role-based constructor", "round 4 D1")
  def apply(stages: Int, input: InputSide, secondSunPresent: Boolean): Topology =
    (stages, input, secondSunPresent) match
      case (1, _,                  _)     => singleStage
      case (2, InputSide.Sun,      true)  => standardCompound
      case (2, InputSide.Sun,      false) => nssCompound
      case (2, InputSide.Carrier,  _)     => cdcpCompound
      case (n, _,                  _)     =>
        throw new IllegalArgumentException(s"Unsupported legacy topology: stages=$n")

  /** Sun-driven Standard compound: S1 in, R2 out, R1 fixed, carriers shared,
    * planets stacked. C2 is Free — kinematically tied to C1 by the
    * `CarrierToCarrier` first-coupling. */
  val standardCompound: Topology = Topology(
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
    secondCoupling = Coupling.PlanetStack
  )

  /** Carrier-driven compound (CDCP): C1 in, R2 out, R1 fixed, S1 + S2 free.
    * C2 is Free — tied to C1 (the Input) by the `CarrierToCarrier` coupling,
    * so stage 2's carrier also turns at input speed. */
  val cdcpCompound: Topology = Topology(
    stages = 2,
    roles = Map(
      Body.C1 -> BodyRole.Input,
      Body.R2 -> BodyRole.Output,
      Body.R1 -> BodyRole.Fixed,
      Body.S1 -> BodyRole.Free,
      Body.S2 -> BodyRole.Free,
      Body.C2 -> BodyRole.Free
    ),
    firstCoupling  = Coupling.CarrierToCarrier,
    secondCoupling = Coupling.PlanetStack
  )

  /** NSS (no second sun): S1 in, R2 out, R1 fixed, C free, S2 absent. */
  val nssCompound: Topology = Topology(
    stages = 2,
    roles = Map(
      Body.S1 -> BodyRole.Input,
      Body.R2 -> BodyRole.Output,
      Body.R1 -> BodyRole.Fixed,
      Body.C1 -> BodyRole.Free,
      Body.S2 -> BodyRole.Absent
    )
  )

  /** Single-stage reduction: sun in, carrier out, ring fixed. */
  val singleStage: Topology = Topology(
    stages = 1,
    roles = Map(
      Body.S1 -> BodyRole.Input,
      Body.C1 -> BodyRole.Output,
      Body.R1 -> BodyRole.Fixed
    )
  )

  val default: Topology = standardCompound

  /** Map a legacy `Mode` onto its canonical `Topology`. */
  def fromMode(m: Mode): Topology = m match
    case Mode.Standard    => standardCompound
    case Mode.CDCP        => cdcpCompound
    case Mode.NSS         => nssCompound
    case Mode.SingleStage => singleStage

  /** Reverse lookup: legacy `Mode` for canonical tuples, `None` for the 86
    * other configurations. */
  def toLegacyModeOpt(t: Topology): Option[Mode] = t match
    case `standardCompound` => Some(Mode.Standard)
    case `cdcpCompound`     => Some(Mode.CDCP)
    case `nssCompound`      => Some(Mode.NSS)
    case `singleStage`      => Some(Mode.SingleStage)
    case _                  => None

  /** Total-form alias used by callers that previously hard-coded a `Mode`. */
  def toMode(t: Topology): Mode = toLegacyModeOpt(t).getOrElse(
    throw new IllegalArgumentException(
      s"topology $t has no legacy Mode equivalent; migrate caller to use roles directly"
    )
  )

  /** True when this compound topology is effectively a single-stage mechanism
    * — Input, Output, and Fixed all live in one stage's body subset, so the
    * other stage's gear counts don't affect the output velocity.
    *
    * Concretely:
    *
    *  - Compound with S2 present: stage-1 bodies are `{S1, C, R1}`, stage-2
    *    bodies are `{S2, C, R2}`. If all three pinned bodies live in either
    *    subset, the other stage is free-running and decoupled.
    *  - NSS (S2 absent): the only stage-2-exclusive body is `R2`. If `R2`
    *    isn't pinned, stage 2 is along for the ride.
    *  - Single-stage topologies are never "decoupled" in this sense (there
    *    is no second stage to decouple from).
    *
    * Used by the UI to surface a hint when the user picks a tuple that's
    * single-stage equivalent — they get redundant results in compound mode,
    * and Single stage is the clearer choice. */
  // NOTE on novel couplings: for the 5 `Coupling` presets, this body-subset
  // check correctly catches decoupling at the topology level (the body set
  // {S1, C, R1, S2, R2} is shared across all 5). Coupling-induced
  // singularities (e.g. RingToRing with R1 fixed → R2=0 always) surface
  // separately via `RoleSolver.ratio` returning `None`, which the search
  // pipeline filters out — the user sees the "No design matches" placeholder
  // rather than a hint. A future rank-based check could distinguish these
  // two failure modes, but they're equivalent from the user's perspective.
  def isStageDecoupled(t: Topology): Boolean =
    if t.stages == 1 then false
    else
      val rolesUsed = Set(t.input, t.output, t.fixed)
      if t.s2Present then
        // For the shared-carrier compound family (every legacy mode + every
        // current preset), C1 is logically the carrier of *both* stages, so
        // it appears in both subsets — exactly as `Body.C` did pre-split.
        val stage1 = Set(Body.S1, Body.C1, Body.R1)
        val stage2 = Set(Body.S2, Body.C1, Body.R2)
        rolesUsed.subsetOf(stage1) || rolesUsed.subsetOf(stage2)
      else
        // NSS: S2 absent, R2 is the only stage-2-exclusive body.
        !rolesUsed.contains(Body.R2)
