package cpgc.web

import cpgc.constraints.{Body, BodyRole, Coupling, SearchConfig, TeethBounds, Topology}
import cpgc.domain.{Mode, Ratio}

/** Which of the two inter-stage weld dropdowns a coupling edit comes from. */
enum CouplingSlot:
  case First, Second

/** Raw form state (what the inputs hold), before it becomes a `SearchConfig`. */
final case class FormValues(
    topology: Topology,
    sunMin: Int, sunMax: Int,
    planetMin: Int, planetMax: Int,
    ringMin: Int, ringMax: Int,
    planetsMin: Int, planetsMax: Int,
    od: Double,
    minModule: Double,
    sunPlanetFactor: String,
    target: String
)

/** Pure, DOM-free UI logic (parsing, topology edits, config building) so it can
  * be unit-tested without a browser. `App` only wires these into Laminar. */
object UiLogic:

  def parseDouble(s: String): Option[Double] =
    s.trim.replace(',', '.').toDoubleOption

  /** Extract a single URL parameter value (e.g. `target=53.12` → `Some("53.12")`)
    * from a query string such as `window.location.search`. */
  def urlParam(query: String, name: String): Option[String] =
    val q = query.stripPrefix("?")
    q.split('&').iterator
      .map(_.split('=').toList)
      .collectFirst { case k :: v :: Nil if k == name => v }

  /** Parse a `lo-hi` range string into `(lo, hi)`. Returns `None` if either
    * part isn't an int. Used for `?sun=10-40` style bounds in the URL. */
  def parseRange(s: String): Option[(Int, Int)] =
    s.split('-').toList match
      case lo :: hi :: Nil =>
        for l <- lo.toIntOption; h <- hi.toIntOption yield (l, h)
      case _ => None

  def initialTopology(query: String): Topology =
    val q = query
    val m =
      if q.contains("mode=single") then Mode.SingleStage
      else if q.contains("mode=cdcp") then Mode.CDCP
      else if q.contains("mode=nss")  then Mode.NSS
      else Mode.Standard
    // `?coupling1=` and `?coupling2=` accept any `Coupling` case name; the
    // legacy `?coupling=` flag still sets just the second coupling.
    def couplingParam(name: String): Option[Coupling] =
      urlParam(query, name).flatMap(v => Coupling.values.find(_.toString == v))
    val first  = couplingParam("coupling1").getOrElse(Coupling.CarrierToCarrier)
    val second = couplingParam("coupling2")
      .orElse(couplingParam("coupling"))
      .getOrElse(Coupling.PlanetStack)
    val canonical = Topology.fromMode(m)
    // A malformed URL (e.g. `?coupling1=X&coupling2=X`) must not crash startup —
    // fall back to the mode's canonical topology if the coupling pair is invalid.
    val base =
      try canonical.copy(firstCoupling = first, secondCoupling = second)
      catch case _: IllegalArgumentException => canonical
    // Optional `?input=`, `?output=`, `?fixed=` role overrides (body names) —
    // lets shareable URLs encode a full novel topology.
    def bodyParam(name: String): Option[Body] =
      urlParam(query, name).flatMap(v => Body.values.find(_.toString == v))
    (bodyParam("input"), bodyParam("output"), bodyParam("fixed")) match
      case (Some(in), Some(out), Some(fx)) if Set(in, out, fx).size == 3 =>
        val pinned = Map(in -> BodyRole.Input, out -> BodyRole.Output, fx -> BodyRole.Fixed)
        val roles = base.roles.map { (b, r) =>
          b -> pinned.getOrElse(b, if r == BodyRole.Absent then BodyRole.Absent else BodyRole.Free)
        }
        try Topology(base.stages, roles, base.firstCoupling, base.secondCoupling)
        catch case _: IllegalArgumentException => base
      case _ => base

  /** Initial form values from a query string:
    *   ?sun=10-40&planet=10-18&ring=40-120&planets=3-4&od=150&minmod=0.5&target=53.12
    * Falls back to the defaults when a param is absent or unparseable. */
  def initialForm(query: String): FormValues =
    val sunR     = urlParam(query, "sun").flatMap(parseRange).getOrElse((10, 40))
    val planetR  = urlParam(query, "planet").flatMap(parseRange).getOrElse((10, 18))
    val ringR    = urlParam(query, "ring").flatMap(parseRange).getOrElse((40, 120))
    val planetsR = urlParam(query, "planets").flatMap(parseRange).getOrElse((3, 4))
    FormValues(
      topology        = initialTopology(query),
      sunMin = sunR._1, sunMax = sunR._2,
      planetMin = planetR._1, planetMax = planetR._2,
      ringMin = ringR._1, ringMax = ringR._2,
      planetsMin = planetsR._1, planetsMax = planetsR._2,
      od              = urlParam(query, "od").flatMap(_.toDoubleOption).getOrElse(150.0),
      minModule       = urlParam(query, "minmod").flatMap(_.toDoubleOption).getOrElse(0.5),
      sunPlanetFactor = urlParam(query, "spfact").getOrElse(""),
      target          = urlParam(query, "target").getOrElse("53.12")
    )

  /** Target-ratio text → ratios. Comma-separated; each piece is parsed with
    * `parseDouble` and unparseable pieces are dropped. */
  def parseTargets(s: String): Seq[Ratio] =
    s.split(',').toList.flatMap(p => parseDouble(p)).map(Ratio.apply)

  /** Normalise an inverted (lo > hi) pair before passing it to TeethBounds, so
    * the user can drag/type freely without tripping the `max >= min` invariant. */
  def bounds(lo: Int, hi: Int): TeethBounds =
    val l = math.min(lo, hi)
    val h = math.max(lo, hi)
    TeethBounds(l, h)

  def buildConfig(f: FormValues, topPerTarget: Int): SearchConfig =
    SearchConfig(
      topology        = f.topology,
      sunTeeth        = bounds(f.sunMin, f.sunMax),
      planetTeeth     = bounds(f.planetMin, f.planetMax),
      ringTeeth       = bounds(f.ringMin, f.ringMax),
      planets         = bounds(f.planetsMin, f.planetsMax),
      firstStageOD    = f.od,
      minModule       = f.minModule,
      sunPlanetFactor = parseDouble(f.sunPlanetFactor).getOrElse(1e9),
      topPerTarget    = topPerTarget,
      targets         = parseTargets(f.target)
    )

  // ── Gearbox presets ────────────────────────────────────────────────────────

  /** Ring-to-sun cascade: classic shared carrier + `R1 ≡ S2`. */
  val ringToSunCascadeTopology: Topology = Topology(
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
    secondCoupling = Coupling.RingToSun
  )

  /** Ravigneaux family: `C1 ≡ S2` + `R1 ≡ R2`. */
  val ravigneauxTopology: Topology = Topology(
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

  /** Simpson family: `C1 ≡ R2` + `S1 ≡ S2`. */
  val simpsonTopology: Topology = Topology(
    stages = 2,
    roles = Map(
      Body.S1 -> BodyRole.Input,
      Body.C2 -> BodyRole.Output,
      Body.R1 -> BodyRole.Fixed,
      Body.C1 -> BodyRole.Free,
      Body.S2 -> BodyRole.Free,
      Body.R2 -> BodyRole.Free
    ),
    firstCoupling  = Coupling.CarrierToRing,
    secondCoupling = Coupling.SunToSun
  )

  /** Map a preset → its canonical `Topology`. */
  def presetTopology(p: GearboxPreset): Topology = p match
    case GearboxPreset.SingleStage       => Topology.singleStage
    case GearboxPreset.SunDriven         => Topology.standardCompound
    case GearboxPreset.CarrierDriven     => Topology.cdcpCompound
    case GearboxPreset.NoSecondSun       => Topology.nssCompound
    case GearboxPreset.RingToSunCascade  => ringToSunCascadeTopology
    case GearboxPreset.Ravigneaux        => ravigneauxTopology
    case GearboxPreset.Simpson           => simpsonTopology
    case GearboxPreset.Custom            => Topology.standardCompound // unused: picker doesn't apply Custom

  /** Reverse-lookup: which preset (if any) does this `Topology` match? */
  def topologyPreset(t: Topology): GearboxPreset =
    if      t == Topology.singleStage      then GearboxPreset.SingleStage
    else if t == Topology.standardCompound then GearboxPreset.SunDriven
    else if t == Topology.cdcpCompound     then GearboxPreset.CarrierDriven
    else if t == Topology.nssCompound      then GearboxPreset.NoSecondSun
    else if t == ringToSunCascadeTopology  then GearboxPreset.RingToSunCascade
    else if t == ravigneauxTopology        then GearboxPreset.Ravigneaux
    else if t == simpsonTopology           then GearboxPreset.Simpson
    else                                        GearboxPreset.Custom

  // ── Topology edits ─────────────────────────────────────────────────────────

  /** Move `body` to `newRole`, displacing whoever previously held `newRole`
    * into `body`'s previous role. Invariants of `Topology` are preserved, and
    * the user's chosen `firstCoupling` / `secondCoupling` are carried through
    * (a role change must not silently reset the coupling). */
  def assignRole(t: Topology, body: Body, newRole: BodyRole): Topology =
    if t.roles.get(body).contains(newRole) then t
    else
      val previousOwnRole = t.roles(body)
      val previousHolder  = t.roles.collectFirst { case (b, r) if r == newRole => b }
      val updated = previousHolder match
        case Some(b) if b != body =>
          t.roles.updated(body, newRole).updated(b, previousOwnRole)
        case _ =>
          t.roles.updated(body, newRole)
      Topology(t.stages, updated, t.firstCoupling, t.secondCoupling)

  /** Toggle S2 between `Absent` and `Free`. If S2 currently holds a pin role
    * (Input / Output / Fixed), the role is reassigned to any Free body first
    * before flipping to Absent — keeps invariants. */
  def setS2Present(t: Topology, present: Boolean): Topology =
    if t.stages != 2 then t
    else if present then
      t.roles.get(Body.S2) match
        case Some(BodyRole.Absent) =>
          // S2 + R2 come back; carry the user's coupling choice through.
          Topology(2, t.roles.updated(Body.S2, BodyRole.Free), t.firstCoupling, t.secondCoupling)
        case _ => t // already present
    else
      val s2CurrentRole = t.roles.getOrElse(Body.S2, BodyRole.Free)
      if s2CurrentRole == BodyRole.Absent then t
      else
        // If S2 holds a pin role, transfer it to a Free body before going Absent.
        val withS2DePinned: Topology = s2CurrentRole match
          case BodyRole.Free | BodyRole.Absent => t
          case pinRole =>
            t.roles.collectFirst { case (b, BodyRole.Free) if b != Body.S2 => b } match
              case Some(freeBody) =>
                Topology(2, t.roles.updated(freeBody, pinRole).updated(Body.S2, BodyRole.Free),
                         t.firstCoupling, t.secondCoupling)
              case None =>
                // No Free body to take S2's pin role — can't make S2 absent
                // without breaking the one-pin-each invariant. No-op rather
                // than throw (a throw here would poison the topology Var).
                return t
        Topology(2, withS2DePinned.roles.updated(Body.S2, BodyRole.Absent),
                 withS2DePinned.firstCoupling, withS2DePinned.secondCoupling)

  /** Switch between single-stage and compound. We reset to the canonical
    * `singleStage` / `standardCompound` rather than try to preserve roles —
    * the body set changes, so role preservation isn't generally meaningful. */
  def setStageCount(t: Topology, n: Int): Topology = n match
    case 1 => Topology.singleStage
    case 2 => if t.stages == 2 then t else Topology.standardCompound
    case _ => t

  /** Replace one of the two inter-stage welds. */
  def setCoupling(t: Topology, slot: CouplingSlot, c: Coupling): Topology = slot match
    case CouplingSlot.First  => t.copy(firstCoupling = c)
    case CouplingSlot.Second => t.copy(secondCoupling = c)

  /** True when the two coupling dropdowns are inactive. */
  def couplingSelectorsDisabled(t: Topology): Boolean = t.stages != 2

  /** Whether option `c` of the `slot` dropdown is greyed out:
    *  - same weld as the other dropdown (would make couplings identical);
    *  - PlanetStack, unless the other dropdown is CarrierToCarrier (a single
    *    stacked pinion physically requires the carriers to be coupled — see
    *    Edge-Case Catalogue #19). */
  def couplingOptionDisabled(t: Topology, slot: CouplingSlot, c: Coupling): Boolean =
    val other = slot match
      case CouplingSlot.First  => t.secondCoupling
      case CouplingSlot.Second => t.firstCoupling
    other == c || (c == Coupling.PlanetStack && other != Coupling.CarrierToCarrier)
