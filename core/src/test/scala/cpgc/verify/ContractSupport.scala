package cpgc.verify

import cpgc.constraints.*
import cpgc.domain.*
import cpgc.physics.PlanetSpacing
import cpgc.search.{Enumerator, RoleSolver}

import scala.util.Random

/** Shared fixtures for the contract / completeness / cross-path specs.
  *
  * `Case` = a named topology plus the way the product builds a `SearchConfig`
  * for it: the web UI form (`mode = legacyMode.getOrElse(Standard)` +
  * `topology = Some(t)`) or the plain CLI form (`SearchConfig(mode = m)`).
  */
object ContractSupport:

  final case class Case(name: String, topology: Topology, viaTopology: Boolean):
    def config(base: SearchConfig): SearchConfig =
      if viaTopology then
        base.copy(mode = topology.legacyMode.getOrElse(Mode.Standard), topology = Some(topology))
      else
        base.copy(mode = topology.legacyMode.get, topology = None)

  private def comp(
      in: Body, out: Body, fix: Body, first: Coupling, second: Coupling, s2Absent: Boolean = false
  ): Topology =
    val bodies =
      if s2Absent then Set(Body.S1, Body.C1, Body.R1, Body.R2)
      else Set(Body.S1, Body.C1, Body.R1, Body.S2, Body.C2, Body.R2)
    val base = bodies.map(b => b -> BodyRole.Free).toMap
    val roles = base + (in -> BodyRole.Input) + (out -> BodyRole.Output) + (fix -> BodyRole.Fixed) ++
      (if s2Absent then Map(Body.S2 -> BodyRole.Absent) else Map.empty)
    Topology(2, roles, first, second)

  private def single(in: Body, out: Body, fix: Body): Topology =
    val roles = Map(Body.S1 -> BodyRole.Free, Body.C1 -> BodyRole.Free, Body.R1 -> BodyRole.Free) +
      (in -> BodyRole.Input) + (out -> BodyRole.Output) + (fix -> BodyRole.Fixed)
    Topology(1, roles)

  import Body.*
  import Coupling.*

  val legacy: Vector[(String, Topology)] = Vector(
    "std"    -> Topology.standardCompound,
    "cdcp"   -> Topology.cdcpCompound,
    "nss"    -> Topology.nssCompound,
    "single" -> Topology.singleStage
  )

  val novel: Vector[(String, Topology)] = Vector(
    "PS+CC std-roles"            -> comp(S1, R2, R1, PlanetStack, CarrierToCarrier),
    "PS+RingToSun std-roles"     -> comp(S1, R2, R1, PlanetStack, RingToSun),
    "CC+RingToSun std-roles"     -> comp(S1, R2, R1, CarrierToCarrier, RingToSun),
    "SunToCarrier+RingToSun"     -> comp(S1, R2, R1, SunToCarrier, RingToSun),
    "CC+SunToRing std-roles"     -> comp(S1, R2, R1, CarrierToCarrier, SunToRing),
    "CC+RingToRing C1fixed"      -> comp(S1, R2, C1, CarrierToCarrier, RingToRing),
    "CC+SunToSun std-roles"      -> comp(S1, R2, R1, CarrierToCarrier, SunToSun),
    "NSS-like (C1 fixed)"        -> comp(S1, R2, C1, CarrierToCarrier, PlanetStack, s2Absent = true),
    "CDCP-like (S1 fixed)"       -> comp(C1, R2, S1, CarrierToCarrier, PlanetStack),
    "single S1->R1 C1fixed"      -> single(S1, R1, C1),
    "single C1->S1 R1fixed"      -> single(C1, S1, R1)
  )

  val cases: Vector[Case] =
    legacy.map((n, t) => Case(s"$n[topology]", t, viaTopology = true)) ++
      legacy.map((n, t) => Case(s"$n[mode-only]", t, viaTopology = false)) ++
      novel.map((n, t) => Case(n, t, viaTopology = true))

  // ── spec rules ────────────────────────────────────────────────────────────

  def stacked(t: Topology): Boolean =
    t.stages == 2 && (t.firstCoupling == PlanetStack || t.secondCoupling == PlanetStack)

  /** The intended spacing rule (see task statement). */
  def specSpacingOk(t: Topology, s1: GearSet, s2: Option[GearSet]): Boolean =
    if t.stages == 1 then s1.evenSpacing
    else if stacked(t) && t.input == C1 then true // CDCP relaxation
    else if !t.s2Present then s1.evenSpacing       // NSS relaxation
    else s1.evenSpacing && s2.forall(_.evenSpacing)

  // ── random configs ────────────────────────────────────────────────────────

  def randomConfig(rng: Random, withTargets: Boolean): SearchConfig =
    val sMin = rng.between(6, 11)
    val sMax = sMin + rng.between(2, 7)
    val pMin = rng.between(6, 10)
    val pMax = pMin + rng.between(0, 4)
    val rLo  = sMin + 2 * pMin
    val rHi  = sMax + 2 * pMax
    val rMin = rLo + rng.between(0, 4)
    val rMax = math.max(rMin, rHi - rng.between(0, 4))
    val (nLo, nHi) = Vector((3, 3), (3, 4), (2, 4), (4, 5))(rng.nextInt(4))
    val targets =
      if !withTargets then Seq.empty
      else Seq.fill(rng.between(1, 3)) {
        val mag = 1.5 + rng.nextDouble() * 28.0
        Ratio(if rng.nextInt(5) == 0 then -mag else mag)
      }
    SearchConfig(
      sunTeeth = TeethBounds(sMin, sMax),
      planetTeeth = TeethBounds(pMin, pMax),
      ringTeeth = TeethBounds(rMin, rMax),
      planets = TeethBounds(nLo, nHi),
      minModule = Vector(0.5, 1.0, 1.5)(rng.nextInt(3)),
      firstStageOD = Vector(60.0, 100.0, 150.0)(rng.nextInt(3)),
      sunPlanetFactor = Vector(2.0, 3.0)(rng.nextInt(2)),
      targets = targets,
      topPerTarget = 5
    )

  def cfgSummary(c: SearchConfig): String =
    s"sun=${c.sunTeeth.min}..${c.sunTeeth.max} planet=${c.planetTeeth.min}..${c.planetTeeth.max} " +
      s"ring=${c.ringTeeth.min}..${c.ringTeeth.max} N=${c.planets.min}..${c.planets.max} " +
      s"minMod=${c.minModule} OD=${c.firstStageOD} spf=${c.sunPlanetFactor} " +
      s"targets=${c.targets.map(_.value)} mode=${c.mode} topo=${if c.topology.isDefined then "Some" else "None"}"

  type Key = (Int, Int, Int, Int, Int, Int, Int)

  /** Teeth + planet count of both stages. */
  def key(d: CompoundDesign): Key =
    val s1 = d.stage1.set
    d.stage2 match
      case Some(st) =>
        (s1.sun.toInt, s1.planet.toInt, s1.numPlanets, st.set.sun.toInt, st.set.planet.toInt, st.set.numPlanets, 1)
      case None => (s1.sun.toInt, s1.planet.toInt, s1.numPlanets, 0, 0, 0, 0)

  def fmt(d: CompoundDesign): String =
    val a = d.stage1.set
    val b = d.stage2.map(s => s"S2=${s.set.sun.toInt} P2=${s.set.planet.toInt} N2=${s.set.numPlanets} m2=${s.module.value}")
    s"S1=${a.sun.toInt} P1=${a.planet.toInt} N1=${a.numPlanets} m1=${d.stage1.module.value} ${b.getOrElse("(no stage2)")} ratio=${d.ratio.value} eff=${d.efficiency}"

  // ── reference (spec-based) exhaustive enumeration ─────────────────────────

  /** Every valid design per the intended rules, independent of `Compounder`.
    * Returns (s1, s2, ratio). Includes identical-stage pairs. */
  def referenceDesigns(cfg: SearchConfig): Vector[(GearSet, Option[GearSet], Double)] =
    val t = cfg.effectiveTopology
    val pool = Enumerator.gearSets(cfg).toVector
    def modOk(m: Double) = m >= cfg.minModule
    if t.stages == 1 then
      for
        a <- pool
        if specSpacingOk(t, a, None)
        if modOk(cfg.firstStageOD / (a.ring.toDouble + 2.0))
        r <- RoleSolver.ratio(t, a, None).toVector
      yield (a, None, r.value)
    else
      for
        a <- pool
        b <- pool
        if !stacked(t) || a.numPlanets == b.numPlanets
        if specSpacingOk(t, a, Some(b))
        m1 = cfg.firstStageOD / (a.ring.toDouble + 2.0)
        m2 = if stacked(t) then m1 * (a.sun.toInt + a.planet.toInt).toDouble / (b.sun.toInt + b.planet.toInt) else m1
        if modOk(m1) && modOk(m2)
        r <- RoleSolver.ratio(t, a, Some(b)).toVector
      yield (a, Some(b), r.value)

  def err(target: Double, ratio: Double): Double = math.abs(math.abs(target) - math.abs(ratio))

  def planetsFit(s: GearSet): Boolean = PlanetSpacing.planetsFitWithoutOverlap(s)
