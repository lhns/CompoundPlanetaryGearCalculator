package cpgc.verify

import cpgc.constraints.*
import cpgc.domain.*
import cpgc.physics.PlanetSpacing
import cpgc.search.{Enumerator, RoleSolver}

import scala.util.Random

/** Shared fixtures for the contract / completeness / cross-path specs.
  *
  * `Case` = a named topology. `SearchConfig.topology` is the single source of
  * truth (ADR 0015), so the CLI form (`Mode` alias) and the web form
  * (explicit topology) both reduce to `base.copy(topology = t)`.
  */
object ContractSupport:

  final case class Case(name: String, topology: Topology):
    def config(base: SearchConfig): SearchConfig = base.copy(topology = topology)

  private def comp(
      in: Body, out: Body, fix: Body, first: Coupling, second: Coupling, absent: Set[Body] = Set.empty
  ): Topology =
    val pins = Map(in -> BodyRole.Input, out -> BodyRole.Output, fix -> BodyRole.Fixed)
    val roles = Body.values.map { b =>
      b -> (if absent.contains(b) then BodyRole.Absent else pins.getOrElse(b, BodyRole.Free))
    }.toMap
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
    "CC+RingToSun std-roles"     -> comp(S1, R2, R1, CarrierToCarrier, RingToSun),
    "SunToCarrier+RingToSun"     -> comp(S1, R2, R1, SunToCarrier, RingToSun),
    "CC+SunToRing std-roles"     -> comp(S1, R2, R1, CarrierToCarrier, SunToRing),
    "CC+RingToRing C1fixed"      -> comp(S1, R2, C1, CarrierToCarrier, RingToRing),
    "CC+SunToSun std-roles"      -> comp(S1, R2, R1, CarrierToCarrier, SunToSun),
    "NSS-like (C1 fixed)"        -> comp(S1, R2, C1, CarrierToCarrier, PlanetStack, Set(S2)),
    "CDCP-like (S1 fixed)"       -> comp(C1, R2, S1, CarrierToCarrier, PlanetStack),
    "CDCP sun-less (both)"       -> Topology.sunlessCdcpCompound,
    "CDCP S1-absent"             -> comp(C1, R2, R1, CarrierToCarrier, PlanetStack, Set(S1)),
    "single S1->R1 C1fixed"      -> single(S1, R1, C1),
    "single C1->S1 R1fixed"      -> single(C1, S1, R1)
  )

  val cases: Vector[Case] =
    (legacy.map((n, t) => Case(s"$n[topology]", t)) ++ novel.map((n, t) => Case(n, t)))

  // ── spec rules ────────────────────────────────────────────────────────────

  def stacked(t: Topology): Boolean =
    t.stages == 2 && (t.firstCoupling == PlanetStack || t.secondCoupling == PlanetStack)

  private def gcd(a: Int, b: Int): Int = if b == 0 then math.abs(a) else gcd(b, a % b)

  /** The intended assembly rule (ADR 0015 §5, ADR 0018): every stage whose sun
    * is present needs `(S + R) % N == 0`; a sun-less stage has no such
    * condition; stacked identical pinions (`phased = false`) also need
    * `((Tp1·Tr2 − Tp2·Tr1) / gcd(Tp1, Tp2)) % N == 0`. */
  def specSpacingOk(t: Topology, phased: Boolean, s1: GearSet, s2: Option[GearSet]): Boolean =
    (!t.sunPresent(1) || s1.evenSpacing) && s2.forall(g => !t.sunPresent(2) || g.evenSpacing) &&
      (phased || !stacked(t) || s2.forall { g =>
        val p1 = s1.planet.toInt; val p2 = g.planet.toInt
        ((p1 * g.ring.toInt - p2 * s1.ring.toInt) / gcd(p1, p2)) % s1.numPlanets == 0
      })

  /** Intended first-stage module (ADR 0016), `None` when not positive finite. */
  def specModule1(cfg: SearchConfig, s1: GearSet): Option[Double] =
    val m = (cfg.firstStageOD - 2.0 * cfg.ringWall) / (s1.ring.toDouble + 2.5)
    if m > 0.0 && m.isFinite then Some(m) else None

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
      topPerTarget = 5,
      phasedPinions = rng.nextInt(3) == 0
    )

  def cfgSummary(c: SearchConfig): String =
    s"sun=${c.sunTeeth.min}..${c.sunTeeth.max} planet=${c.planetTeeth.min}..${c.planetTeeth.max} " +
      s"ring=${c.ringTeeth.min}..${c.ringTeeth.max} N=${c.planets.min}..${c.planets.max} " +
      s"phased=${c.phasedPinions} minMod=${c.minModule} OD=${c.firstStageOD} wall=${c.ringWall} clr=${c.planetClearance} spf=${c.sunPlanetFactor} " +
      s"targets=${c.targets.map(_.value)}"

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
    val t = cfg.topology
    val pool1 = Enumerator.stage1Sets(cfg).toVector
    def modOk(m: Double) = m >= cfg.minModule
    if t.stages == 1 then
      for
        a <- pool1
        if specSpacingOk(t, cfg.phasedPinions, a, None)
        m1 <- specModule1(cfg, a).toVector
        if modOk(m1)
        r <- RoleSolver.ratio(t, a, None).toVector
      yield (a, None, r.value)
    else
      val pool2 = Enumerator.stage2Sets(cfg).toVector
      for
        a <- pool1
        b <- pool2
        if !stacked(t) || a.numPlanets == b.numPlanets
        if specSpacingOk(t, cfg.phasedPinions, a, Some(b))
        m1 <- specModule1(cfg, a).toVector
        m2 = if stacked(t) then m1 * (a.sun.toInt + a.planet.toInt).toDouble / (b.sun.toInt + b.planet.toInt) else m1
        if modOk(m1) && modOk(m2)
        r <- RoleSolver.ratio(t, a, Some(b)).toVector
      yield (a, Some(b), r.value)

  def err(target: Double, ratio: Double): Double = math.abs(math.abs(target) - math.abs(ratio))

  def planetsFit(s: GearSet, cfg: SearchConfig): Boolean =
    PlanetSpacing.planetsFitWithoutOverlap(s, cfg.planetClearance)
