package cpgc.web

import cpgc.constraints.{Body, BodyRole, Coupling, SearchConfig, TeethBounds, Topology}
import cpgc.domain.Ratio
import cpgc.search.Scorer.SortBy

/** Which of the two inter-stage weld dropdowns a coupling edit comes from. */
enum CouplingSlot:
  case First, Second

/** Named gearbox families. Each maps to a complete `Topology`; `Custom` is the
  * derived state of a topology that matches no family (never selectable). */
enum GearboxPreset:
  case SingleStage
  case SunDriven       // Standard compound: stacked planet, sun in, R2 out
  case CarrierDriven   // CDCP: stacked planet, carrier in, R2 out
  case SunlessCdcp     // CDCP without suns
  case NoSecondSun     // NSS: stacked planet, no S2
  case RingToSunCascade
  case Ravigneaux
  case Simpson
  case Custom

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
    target: String,
    phasedPinions: Boolean = false,
    ringWall: Double = SearchConfig.DefaultRingWall,
    clearance: Double = SearchConfig.DefaultPlanetClearance
)

/** Pure, DOM-free UI logic (parsing, topology edits, config building, URL
  * state) so it can be unit-tested without a browser. `App` only wires these
  * into Laminar. */
object UiLogic:

  // ── Numbers ────────────────────────────────────────────────────────────────

  private val NumberRe = """[+-]?(\d+([.,]\d*)?|[.,]\d+)([eE][+-]?\d+)?""".r

  /** Strict decimal parse: accepts a decimal comma, rejects `1d`, `0x1p3`,
    * `1,000.5` and the like. May return Infinity (`1e999`); see `parseFinite`. */
  def parseDouble(s: String): Option[Double] =
    val t = s.trim
    if NumberRe.matches(t) then t.replace(',', '.').toDoubleOption else None

  /** Like `parseDouble` but only finite results. */
  def parseFinite(s: String): Option[Double] = parseDouble(s).filter(_.isFinite)

  /** Should a text input be rewritten to show `current`? Not when its text
    * already parses to that value (the user is typing, e.g. "0."). */
  def shouldRewrite(text: String, current: Double): Boolean = !parseFinite(text).contains(current)
  def shouldRewrite(text: String, current: Int): Boolean    = !text.trim.toIntOption.contains(current)

  // ── URL parameters ─────────────────────────────────────────────────────────

  private def hexVal(c: Char): Int = Character.digit(c, 16)

  /** Percent-decode (UTF-8); `+` is a space. Malformed escapes stay literal. */
  def percentDecode(s: String): String =
    if !s.contains('%') && !s.contains('+') then s
    else
      val out = new java.io.ByteArrayOutputStream()
      def put(str: String): Unit =
        val b = str.getBytes("UTF-8")
        out.write(b, 0, b.length)
      var i = 0
      while i < s.length do
        val c = s.charAt(i)
        if c == '%' && i + 2 < s.length && hexVal(s.charAt(i + 1)) >= 0 && hexVal(s.charAt(i + 2)) >= 0 then
          out.write(hexVal(s.charAt(i + 1)) * 16 + hexVal(s.charAt(i + 2)))
          i += 3
        else if c == '+' then { out.write(' '); i += 1 }
        else if Character.isHighSurrogate(c) && i + 1 < s.length then { put(s.substring(i, i + 2)); i += 2 }
        else { put(c.toString); i += 1 }
      new String(out.toByteArray, "UTF-8")

  /** Percent-encode everything except unreserved characters and `,`. */
  def percentEncode(s: String): String =
    val sb = new StringBuilder
    for b <- s.getBytes("UTF-8") do
      val v = b & 0xff
      val c = v.toChar
      if (v < 128 && c.isLetterOrDigit) || "-._~,".indexOf(c) >= 0 then sb += c
      else sb ++= f"%%$v%02X"
    sb.toString

  /** Query string → decoded name/value pairs. */
  private def pairs(query: String): Vector[(String, String)] =
    query.stripPrefix("?").split('&').toVector.filter(_.nonEmpty).map { kv =>
      val i = kv.indexOf('=')
      if i < 0 then (percentDecode(kv), "") else (percentDecode(kv.take(i)), percentDecode(kv.drop(i + 1)))
    }

  /** One URL parameter value by exact name, percent-decoded
    * (`?target=53%2C12` → `Some("53,12")`). The first occurrence wins. */
  def urlParam(query: String, name: String): Option[String] =
    pairs(query).collectFirst { case (k, v) if k == name => v }

  private val RangeRe = """(-?\d+)(?:-(-?\d+))?""".r

  /** Parse `lo-hi`, or a single value `n` as the degenerate range `(n, n)`. */
  def parseRange(s: String): Option[(Int, Int)] = s.trim match
    case RangeRe(lo, null) => lo.toIntOption.map(l => (l, l))
    case RangeRe(lo, hi)   => for l <- lo.toIntOption; h <- hi.toIntOption yield (l, h)
    case _                 => None

  private def modeParam(v: String): Option[Topology] = v.trim.toLowerCase match
    case "standard"                 => Some(Topology.standardCompound)
    case "single"                   => Some(Topology.singleStage)
    case "cdcp"                     => Some(Topology.cdcpCompound)
    case "nss"                      => Some(Topology.nssCompound)
    case "cdcp-sunless" | "sunless" => Some(Topology.sunlessCdcpCompound)
    case _                          => None

  /** `?mode=` names a base layout; `coupling1/2`, `input/output/fixed` (all
    * three) and `absent=S1,S2` then override it. An override that does not
    * give a legal topology is skipped. */
  def initialTopology(query: String): Topology =
    val base = urlParam(query, "mode").flatMap(modeParam).getOrElse(Topology.standardCompound)
    def tryBuild(t: => Topology, fallback: Topology): Topology =
      try t catch case _: IllegalArgumentException => fallback

    def couplingParam(name: String): Option[Coupling] =
      urlParam(query, name).flatMap(v => Coupling.values.find(_.toString == v))
    val c1 = couplingParam("coupling1")
    // the legacy `?coupling=` flag still sets just the second coupling
    val c2 = couplingParam("coupling2").orElse(couplingParam("coupling"))
    val withCouplings =
      if base.stages != 2 || (c1.isEmpty && c2.isEmpty) then base
      else tryBuild(base.copy(firstCoupling = c1.getOrElse(base.firstCoupling),
                              secondCoupling = c2.getOrElse(base.secondCoupling)), base)

    def bodyParam(name: String): Option[Body] =
      urlParam(query, name).flatMap(v => Body.values.find(_.toString == v))
    val absentParam: Option[Set[Body]] = urlParam(query, "absent").map(
      _.split(',').iterator.map(_.trim).flatMap(n => Body.values.find(_.toString == n)).toSet)

    val pinned: Option[Map[Body, BodyRole]] = (bodyParam("input"), bodyParam("output"), bodyParam("fixed")) match
      case (Some(in), Some(out), Some(fx)) if Set(in, out, fx).size == 3 =>
        Some(Map(in -> BodyRole.Input, out -> BodyRole.Output, fx -> BodyRole.Fixed))
      case _ => None

    if pinned.isEmpty && absentParam.isEmpty then withCouplings
    else
      def wasAbsent(b: Body) = withCouplings.roles.get(b).contains(BodyRole.Absent)
      def absent(b: Body)    = absentParam.fold(wasAbsent(b))(_.contains(b))
      val roles = withCouplings.roles.map { (b, r) =>
        b -> pinned.flatMap(_.get(b)).getOrElse {
          if (b == Body.S1 || b == Body.S2) && absent(b) then BodyRole.Absent
          else if pinned.isEmpty && r != BodyRole.Absent then r // only `absent` given: keep the pins
          else BodyRole.Free
        }
      }
      tryBuild(withCouplings.copy(roles = roles), withCouplings)

  def initialSort(query: String): SortBy =
    urlParam(query, "sort").flatMap(SortBy.parse).getOrElse(SortBy.BestFit)

  /** Initial form values from a query string:
    *   ?sun=10-40&planet=10-18&ring=40-120&planets=3-4&od=150&minmod=0.5&target=53.12
    *   &ringwall=2&clearance=0.5&phased=1
    * Falls back to the defaults when a param is absent or unparseable. */
  def initialForm(query: String): FormValues =
    def range(name: String, d: (Int, Int)) = urlParam(query, name).flatMap(parseRange).getOrElse(d)
    def num(name: String, d: Double)       = urlParam(query, name).flatMap(parseFinite).getOrElse(d)
    val sunR     = range("sun", (10, 40))
    val planetR  = range("planet", (10, 18))
    val ringR    = range("ring", (40, 120))
    val planetsR = range("planets", (3, 4))
    FormValues(
      topology        = initialTopology(query),
      sunMin = sunR._1, sunMax = sunR._2,
      planetMin = planetR._1, planetMax = planetR._2,
      ringMin = ringR._1, ringMax = ringR._2,
      planetsMin = planetsR._1, planetsMax = planetsR._2,
      od              = num("od", 150.0),
      minModule       = num("minmod", 0.5),
      sunPlanetFactor = urlParam(query, "spfact").getOrElse(""),
      target          = urlParam(query, "target").getOrElse("53.12"),
      phasedPinions   = urlParam(query, "phased").exists(v => v == "1" || v == "true"),
      ringWall        = num("ringwall", SearchConfig.DefaultRingWall),
      clearance       = num("clearance", SearchConfig.DefaultPlanetClearance)
    )

  private def fmtNum(d: Double): String =
    if d == math.rint(d) && math.abs(d) < 1e15 then d.toLong.toString else d.toString

  /** Query string (without `?`) that `initialForm` / `initialTopology` /
    * `initialSort` read back to the same state: the URL mirrors the page. */
  def toQuery(f: FormValues, sort: SortBy): String =
    val t = f.topology
    val topo: Vector[(String, String)] =
      t.legacyMode match
        case Some(m) => Vector("mode" -> (if t.stages == 1 then "single" else m.toString.toLowerCase))
        case None =>
          val base =
            if t == Topology.sunlessCdcpCompound then Vector("mode" -> "cdcp-sunless")
            else Vector("mode" -> (if t.stages == 1 then "single" else "standard"))
          base ++
            (if t.stages == 2 then Vector("coupling1" -> t.firstCoupling.toString, "coupling2" -> t.secondCoupling.toString)
             else Vector.empty) ++
            Vector("input" -> t.input.toString, "output" -> t.output.toString, "fixed" -> t.fixed.toString,
                   "absent" -> Body.values.filter(b => t.roles.get(b).contains(BodyRole.Absent)).mkString(","))
    val rest = Vector(
      "sun" -> s"${f.sunMin}-${f.sunMax}",
      "planet" -> s"${f.planetMin}-${f.planetMax}",
      "ring" -> s"${f.ringMin}-${f.ringMax}",
      "planets" -> s"${f.planetsMin}-${f.planetsMax}",
      "od" -> fmtNum(f.od),
      "minmod" -> fmtNum(f.minModule),
      "ringwall" -> fmtNum(f.ringWall),
      "clearance" -> fmtNum(f.clearance),
      "target" -> f.target,
      "spfact" -> f.sunPlanetFactor,
      "sort" -> sort.toString.toLowerCase
    ) ++ (if f.phasedPinions then Vector("phased" -> "1") else Vector.empty)
    (topo ++ rest).map((k, v) => s"$k=${percentEncode(v)}").mkString("&")

  // ── Targets ────────────────────────────────────────────────────────────────

  /** Target text → (accepted ratios, dropped pieces). `;` or whitespace
    * separates targets; a comma between digits is a decimal comma
    * (`53,12` = 53.12); "100, 53.12" is two targets. Non-finite or malformed
    * pieces are dropped. */
  def parseTargetsDetailed(s: String): (Seq[Ratio], Seq[String]) =
    val pieces = s.split("[;\\s]+").toList.map(_.stripPrefix(",").stripSuffix(",")).filter(_.nonEmpty)
    val parsed = pieces.map(p => p -> parseFinite(p))
    (parsed.collect { case (_, Some(v)) => Ratio(v) }, parsed.collect { case (p, None) => p })

  def parseTargets(s: String): Seq[Ratio] = parseTargetsDetailed(s)._1

  /** Hint for target pieces that were ignored, if any. */
  def targetHint(s: String): Option[String] =
    val dropped = parseTargetsDetailed(s)._2
    Option.when(dropped.nonEmpty)(s"Ignored (not a finite number): ${dropped.mkString(", ")}")

  // ── Config ─────────────────────────────────────────────────────────────────

  /** Normalise an inverted (lo > hi) pair before passing it to TeethBounds, so
    * the user can drag/type freely without tripping the `max >= min` invariant. */
  def bounds(lo: Int, hi: Int): TeethBounds =
    TeethBounds(math.min(lo, hi), math.max(lo, hi))

  private def boundsChecked(name: String, lo: Int, hi: Int): Either[String, TeethBounds] =
    val l = math.min(lo, hi)
    if l < 2 then Left(s"$name must be at least 2") else Right(TeethBounds(l, math.max(lo, hi)))

  /** Validate the form and build the search configuration. Never throws. */
  def buildConfig(f: FormValues, topPerTarget: Int): Either[String, SearchConfig] =
    def positive(name: String, v: Double): Either[String, Double] =
      if v.isFinite && v > 0 then Right(v) else Left(s"$name must be a number greater than 0")
    def nonNeg(name: String, v: Double): Either[String, Double] =
      if v.isFinite && v >= 0 then Right(v) else Left(s"$name must be a number, 0 or more")
    val factor: Either[String, Double] =
      if f.sunPlanetFactor.trim.isEmpty then Right(1e9)
      else parseFinite(f.sunPlanetFactor).filter(_ > 0)
        .toRight("Max planet/sun ratio must be a number greater than 0 (or blank)")
    for
      sun     <- boundsChecked("Sun teeth", f.sunMin, f.sunMax)
      planet  <- boundsChecked("Planet teeth", f.planetMin, f.planetMax)
      ring    <- boundsChecked("Ring teeth", f.ringMin, f.ringMax)
      planets <- boundsChecked("# planets", f.planetsMin, f.planetsMax)
      od      <- positive("First-stage OD", f.od)
      mm      <- nonNeg("Min module", f.minModule)
      wall    <- nonNeg("Ring wall", f.ringWall)
      clr     <- nonNeg("Planet clearance", f.clearance)
      _       <- Either.cond(od > 2 * wall, (), "First-stage OD must be larger than twice the ring wall")
      spf     <- factor
    yield SearchConfig(
      topology        = f.topology,
      sunTeeth        = sun,
      planetTeeth     = planet,
      ringTeeth       = ring,
      planets         = planets,
      firstStageOD    = od,
      minModule       = mm,
      sunPlanetFactor = spf,
      topPerTarget    = topPerTarget,
      targets         = parseTargets(f.target),
      ringWall        = wall,
      planetClearance = clr,
      phasedPinions   = f.phasedPinions
    )

  // ── Search bookkeeping ─────────────────────────────────────────────────────

  /** A finished, untruncated run that kept every design it found (`found <= k`)
    * can be re-ranked by another criterion without searching again. */
  def rerankable(done: Boolean, truncated: Boolean, found: Long, k: Int): Boolean =
    done && !truncated && found <= k

  /** Pill label; without a target the engine ranks "best fit" by |ratio|. */
  def sortLabel(by: SortBy, hasTargets: Boolean): String = by match
    case SortBy.BestFit    => if hasTargets then "Best fit" else "Lowest ratio (default)"
    case SortBy.Ratio      => "Ratio"
    case SortBy.Efficiency => "Efficiency"
    case SortBy.Simplicity => "Simplicity"

  def sortTooltip(by: SortBy, hasTargets: Boolean): String = by match
    case SortBy.BestFit =>
      if hasTargets then "Closest match to your target ratio (sign ignored)."
      else "No target given, so designs are ranked by absolute ratio, smallest first."
    case SortBy.Ratio      => "Smallest absolute reduction ratio first. Pick this to browse from low to high reduction."
    case SortBy.Efficiency => "Most mechanically efficient designs first. Pick this to minimise transmission losses."
    case SortBy.Simplicity => "Fewest total teeth first. Pick this to favour compact, printable, cheaper designs."

  /** Efficiency as a percentage: the one unit used in the table and schematic. */
  def fmtEfficiency(e: Double): String = f"${e * 100}%.1f%%"

  // ── Gearbox presets ────────────────────────────────────────────────────────

  /** Presets the user can pick (everything but the derived `Custom`). */
  val selectablePresets: Seq[GearboxPreset] = GearboxPreset.values.toSeq.filter(_ != GearboxPreset.Custom)

  /** Map a preset → its canonical `Topology`. `Custom` has none. */
  def presetTopology(p: GearboxPreset): Topology = p match
    case GearboxPreset.SingleStage       => Topology.singleStage
    case GearboxPreset.SunDriven         => Topology.standardCompound
    case GearboxPreset.CarrierDriven     => Topology.cdcpCompound
    case GearboxPreset.SunlessCdcp       => Topology.sunlessCdcpCompound
    case GearboxPreset.NoSecondSun       => Topology.nssCompound
    case GearboxPreset.RingToSunCascade  => Topology.ringToSunCascade
    case GearboxPreset.Ravigneaux        => Topology.ravigneaux
    case GearboxPreset.Simpson           => Topology.simpson
    case GearboxPreset.Custom            => Topology.standardCompound // unused: not selectable

  /** Reverse-lookup: which preset (if any) does this `Topology` match? */
  def topologyPreset(t: Topology): GearboxPreset =
    selectablePresets.find(p => presetTopology(p) == t).getOrElse(GearboxPreset.Custom)

  // ── Topology edits ─────────────────────────────────────────────────────────

  private def reason(e: IllegalArgumentException): String =
    Option(e.getMessage).getOrElse("invalid topology").stripPrefix("requirement failed: ")

  /** Move `body` to `newRole`, displacing whoever previously held `newRole`
    * into `body`'s previous role. Invariants of `Topology` are preserved, and
    * the user's couplings are carried through. Total: an unknown or absent
    * body, or an illegal result, leaves the topology unchanged. */
  def assignRole(t: Topology, body: Body, newRole: BodyRole): Topology =
    t.roles.get(body) match
      case None | Some(BodyRole.Absent) => t
      case Some(`newRole`)              => t
      case Some(previousOwnRole) =>
        val previousHolder = t.roles.collectFirst { case (b, r) if r == newRole => b }
        val updated = previousHolder match
          case Some(b) if b != body => t.roles.updated(body, newRole).updated(b, previousOwnRole)
          case _                    => t.roles.updated(body, newRole)
        try Topology(t.stages, updated, t.firstCoupling, t.secondCoupling)
        catch case _: IllegalArgumentException => t

  /** Make stage `stage`'s sun present or absent. A pinned sun first hands its
    * pin role to a free body. `Left(reason)` when that is impossible (no
    * free body, the sun is welded by a coupling, single stage, ...). */
  def setSunPresent(t: Topology, stage: Int, present: Boolean): Either[String, Topology] =
    val sun = if stage == 1 then Body.S1 else Body.S2
    t.roles.get(sun) match
      case None => Left("a single-stage gearbox has no second sun")
      case Some(role) =>
        if present then
          if role != BodyRole.Absent then Right(t)
          else Right(Topology(t.stages, t.roles.updated(sun, BodyRole.Free), t.firstCoupling, t.secondCoupling))
        else if role == BodyRole.Absent then Right(t)
        else
          val pinned = role != BodyRole.Free
          // Free bodies that could take over the pin, nearest to the usual drive line first.
          val takers: Seq[Option[Body]] =
            if !pinned then Seq(None)
            else Seq(Body.C1, Body.C2, Body.R1, Body.R2, Body.S1, Body.S2)
              .filter(b => b != sun && t.roles.get(b).contains(BodyRole.Free)).map(Some(_))
          if takers.isEmpty then
            Left(s"${Body.label(sun)} is the ${role.toString.toLowerCase} body and no free body can take over")
          else
            var last = "invalid topology"
            val it = takers.iterator
            var res: Option[Topology] = None
            while res.isEmpty && it.hasNext do
              val roles0 = it.next().fold(t.roles)(b => t.roles.updated(b, role))
              try res = Some(Topology(t.stages, roles0.updated(sun, BodyRole.Absent), t.firstCoupling, t.secondCoupling))
              catch case e: IllegalArgumentException => last = reason(e)
            res.toRight(last)

  /** Why the stage-`stage` sun toggle cannot be flipped (None when it can). */
  def sunToggleDisabledReason(t: Topology, stage: Int): Option[String] =
    val sun     = if stage == 1 then Body.S1 else Body.S2
    val present = t.roles.get(sun).exists(_ != BodyRole.Absent)
    setSunPresent(t, stage, !present).left.toOption

  /** Toggle S2; unchanged when impossible. */
  def setS2Present(t: Topology, present: Boolean): Topology =
    setSunPresent(t, 2, present).getOrElse(t)

  /** Switch between single-stage and compound. We reset to the canonical
    * `singleStage` / `standardCompound` rather than try to preserve roles —
    * the body set changes, so role preservation isn't generally meaningful. */
  def setStageCount(t: Topology, n: Int): Topology = n match
    case 1 => Topology.singleStage
    case 2 => if t.stages == 2 then t else Topology.standardCompound
    case _ => t

  /** Replace one of the two inter-stage welds. `Left(reason)` when the choice
    * is not allowed: the same weld as the other slot, `PlanetStack` without a
    * `CarrierToCarrier` partner, or a weld on an absent sun. Moving the
    * partner of `PlanetStack` away from `CarrierToCarrier` deselects
    * `PlanetStack`: the displaced shared-carrier weld takes its place. */
  def trySetCoupling(t: Topology, slot: CouplingSlot, c: Coupling): Either[String, Topology] =
    val (mine, other) = slot match
      case CouplingSlot.First  => (t.firstCoupling, t.secondCoupling)
      case CouplingSlot.Second => (t.secondCoupling, t.firstCoupling)
    if t.stages != 2 then Left("a single-stage gearbox has no inter-stage welds")
    else if c == mine then Right(t)
    else if c == other then Left("already used by the other coupling")
    else if c == Coupling.PlanetStack && other != Coupling.CarrierToCarrier then
      Left("stacked planets need the shared carrier (Carrier 1 ↔ Carrier 2) as the other coupling")
    else
      val newOther = if other == Coupling.PlanetStack && c != Coupling.CarrierToCarrier then mine else other
      val (f, s) = slot match
        case CouplingSlot.First  => (c, newOther)
        case CouplingSlot.Second => (newOther, c)
      try Right(t.copy(firstCoupling = f, secondCoupling = s))
      catch case e: IllegalArgumentException => Left(reason(e))

  /** `trySetCoupling`, unchanged on rejection. */
  def setCoupling(t: Topology, slot: CouplingSlot, c: Coupling): Topology =
    trySetCoupling(t, slot, c).getOrElse(t)

  /** True when the two coupling dropdowns are inactive. Couplings matter for
    * every compound, with or without a second sun (ADR 0015). */
  def couplingSelectorsDisabled(t: Topology): Boolean = t.stages != 2

  /** Whether option `c` of the `slot` dropdown is greyed out. */
  def couplingOptionDisabled(t: Topology, slot: CouplingSlot, c: Coupling): Boolean =
    trySetCoupling(t, slot, c).isLeft

  /** Why option `c` is greyed out (for the option's tooltip). */
  def couplingOptionReason(t: Topology, slot: CouplingSlot, c: Coupling): Option[String] =
    trySetCoupling(t, slot, c).left.toOption
