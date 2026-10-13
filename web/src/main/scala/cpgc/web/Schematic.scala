package cpgc.web

import com.raquo.laminar.api.L.*
import com.raquo.laminar.api.L.svg as s
import cpgc.constraints.{Body, BodyRole, Coupling, Topology}
import cpgc.domain.{CompoundDesign, Mode, Stage}
import cpgc.kinematics.{BodyAngles, Kinematics}
import cpgc.render.GearGeometry
import cpgc.render.GearGeometry.{planetPhaseOffset, ringPhaseOffset, gearRadii as coreGearRadii}

/** Animated SVG schematic. Static layout is rebuilt only when the selected
  * design changes; the per-frame `phase` signal only updates `transform`
  * attributes — Laminar updates strings in place, not the whole SVG.
  */
object Schematic:

  private val Stage1Ring   = "#118ab2"
  private val Stage1Sun    = "#ffd166"
  private val Stage1Planet = "#06d6a0"
  private val Stage2Ring   = "#9d4edd"
  private val Stage2Sun    = "#ff9f1c"
  private val Stage2Planet = "#73d2de"
  private val CarrierColor = "#8b949e"
  private val CouplingColor = "#f0883e" // amber accent for novel inter-stage welds
  // Theme-aware colours come from the page's CSS variables (index.html).
  private val TextColor    = "var(--svg-text)"
  private val SubText      = "var(--svg-sub)"
  private val InputColor   = "var(--role-input)"   // green accent — input body
  private val OutputColor  = "var(--role-output)"  // blue accent — output body
  private val FixedColor   = "var(--role-fixed)"   // red accent — grounded body
  private val HubFill      = "var(--svg-hub)"

  /** Which role a body is highlighted for. */
  private enum Role:
    case Input, Output, Fixed, Plain

  /** Role of each central body, derived once per render. */
  private final case class Roles(
      stage1Sun:     Role,
      stage1Carrier: Role,
      stage1Ring:    Role,
      stage2Sun:     Role,
      stage2Carrier: Role,
      stage2Ring:    Role
  )

  private def rolesFor(t: Topology): Roles =
    def renderRole(b: Body): Role = t.roles.get(b) match
      case Some(BodyRole.Input)  => Role.Input
      case Some(BodyRole.Output) => Role.Output
      case Some(BodyRole.Fixed)  => Role.Fixed
      case _                     => Role.Plain
    Roles(
      stage1Sun     = renderRole(Body.S1),
      stage1Carrier = renderRole(Body.C1),
      stage1Ring    = renderRole(Body.R1),
      stage2Sun     = renderRole(Body.S2),
      stage2Carrier = renderRole(Body.C2),
      stage2Ring    = renderRole(Body.R2)
    )

  private def roleStrokeColor(role: Role, fallback: String): String = role match
    case Role.Input  => InputColor
    case Role.Output => OutputColor
    case Role.Fixed  => FixedColor
    case Role.Plain  => fallback

  private def roleStrokeWidth(role: Role, base: Double): Double = role match
    case Role.Plain => base
    case _          => base * 2.0

  def apply(
      designSignal:           Signal[Option[CompoundDesign]],
      phaseSignal:            Signal[Double],
      searchStateSignal:      Signal[SearchState],
      searchProgressSignal:   Signal[Int],
      searchFractionSignal:   Signal[Double]
  ): SvgElement =
    s.svg(
      s.viewBox := "-220 -160 440 320",
      s.width   := "100%",
      s.height  := "100%",
      svg.preserveAspectRatio := "xMidYMid meet",
      defs(),
      children <-- designSignal
        .combineWith(searchStateSignal, searchProgressSignal, searchFractionSignal)
        .map {
          // A design always renders with the topology it was found for (d.topology).
          case (Some(d), _,                       _, _) => renderDesign(d, phaseSignal)
          case (None,    SearchState.NoResults,   _, _) => Vector(placeholderNoResults())
          case (None,    SearchState.Invalid,     _, _) => Vector(placeholderInvalid())
          case (None,    _,                       n, f) => Vector(placeholderSearching(n, f))
        }
    )

  private def defs(): SvgElement =
    s.defs(
      s.radialGradient(
        s.idAttr := "sun-glow",
        s.stop(s.offsetAttr := "0%",   s.stopColor := "#fff5b5", s.stopOpacity := "0.55"),
        s.stop(s.offsetAttr := "60%",  s.stopColor := "#ffd166", s.stopOpacity := "0.35"),
        s.stop(s.offsetAttr := "100%", s.stopColor := "#ffd166", s.stopOpacity := "0.0")
      )
    )

  /** SVG `class` attribute (Laminar's `cls` is HTML-only by default for SVG
    * elements; we bind it explicitly here for the shimmer keyframe hook). */
  private val svgClassAttr =
    svg.svgAttr[String]("class", com.raquo.laminar.codecs.StringAsIsCodec, namespace = None)

  /** Width of the progress-bar track in viewBox units. The track spans
    * x ∈ [-TrackHalf, +TrackHalf], y = -12, height = 4. */
  private val TrackHalf = 160.0
  private val TrackY    = -12.0
  private val TrackH    = 4.0

  private def placeholderSearching(found: Int, elapsedFraction: Double): SvgElement =
    val label =
      if found > 0 then s"Searching designs… ($found found)"
      else "Searching designs…"
    val fillW = (2.0 * TrackHalf) * math.max(0.0, math.min(1.0, elapsedFraction))
    // Unique clipPath id so the shimmer is constrained to the track.
    val clipId = nextClipId()
    s.g(
      // Clip the shimmer rect to the track rectangle so it doesn't overflow
      // when its transform slides it past the track edges.
      s.defs(
        svg.clipPathTag(
          s.idAttr := clipId,
          s.rect(
            s.x := f"${-TrackHalf}%.1f", s.y := f"$TrackY%.1f",
            s.width := f"${2.0 * TrackHalf}%.1f", s.height := f"$TrackH%.1f"
          )
        )
      ),
      // Track (background)
      s.rect(
        s.x := f"${-TrackHalf}%.1f", s.y := f"$TrackY%.1f",
        s.width := f"${2.0 * TrackHalf}%.1f", s.height := f"$TrackH%.1f",
        s.fill := "var(--svg-track)"
      ),
      // Elapsed-fraction fill
      s.rect(
        s.x := f"${-TrackHalf}%.1f", s.y := f"$TrackY%.1f",
        s.width := f"$fillW%.1f", s.height := f"$TrackH%.1f",
        s.fill := "#58a6ff", s.fillOpacity := "0.85"
      ),
      // Shimmer overlay (CSS-animated translate). Clipped to track.
      s.g(
        clipPathAttr := s"url(#$clipId)",
        s.rect(
          svgClassAttr := "cpgc-shimmer",
          s.x := f"${-TrackHalf}%.1f", s.y := f"$TrackY%.1f",
          s.width := "60", s.height := f"$TrackH%.1f",
          s.fill := "#ffffff", s.fillOpacity := "0.35"
        )
      ),
      // Count label below the bar
      s.text(
        s.x := "0", s.y := "20",
        s.textAnchor := "middle",
        s.fill := SubText, s.fontSize := "14",
        s.fontFamily := "ui-sans-serif, system-ui",
        label
      )
    )

  private def placeholderInvalid(): SvgElement =
    s.g(
      s.text(
        s.x := "0", s.y := "0", s.textAnchor := "middle",
        s.fill := TextColor, s.fontSize := "14",
        s.fontFamily := "ui-sans-serif, system-ui",
        "Fix the highlighted input to search."
      )
    )

  private def placeholderNoResults(): SvgElement =
    s.g(
      s.text(
        s.x := "0", s.y := "-8", s.textAnchor := "middle",
        s.fill := TextColor, s.fontSize := "14",
        s.fontFamily := "ui-sans-serif, system-ui",
        "No design matches these constraints."
      ),
      s.text(
        s.x := "0", s.y := "16", s.textAnchor := "middle",
        s.fill := SubText, s.fontSize := "11",
        s.fontFamily := "ui-sans-serif, system-ui",
        "Try widening a tooth-count range or clearing the target ratio."
      )
    )

  private def renderDesign(
      d: CompoundDesign,
      phaseSignal: Signal[Double]
  ): Vector[SvgElement] =
    val topology = d.topology
    val twoStage = d.stage2.isDefined
    val (cx1, cx2) = if twoStage then (-110.0, 110.0) else (0.0, 0.0)

    val angles: Signal[BodyAngles] = phaseSignal.map(Kinematics.at(d, _))
    val roles = rolesFor(topology)

    val s1 = stageGroup(d.stage1, cx1, "Stage 1", Stage1Sun, Stage1Planet, Stage1Ring,
                        angles, stageIndex = 1, showSun = topology.s1Present,
                        sunRole = roles.stage1Sun,
                        carrierRole = roles.stage1Carrier,
                        ringRole = roles.stage1Ring)
    val s2 = d.stage2.map(st2 =>
      stageGroup(st2, cx2, "Stage 2", Stage2Sun, Stage2Planet, Stage2Ring,
                 angles, stageIndex = 2,
                 showSun = topology.s2Present,
                 sunRole = roles.stage2Sun,
                 carrierRole = roles.stage2Carrier,
                 ringRole = roles.stage2Ring))

    // Both couplings are drawn: firstCoupling on the lower label row, second
    // on the upper one, so a compound with e.g. {CarrierToCarrier, PlanetStack}
    // shows two stacked annotations.
    val divider =
      if twoStage then
        couplingConnector(topology.firstCoupling,  cx1, cx2, labelY = 142) ++
        couplingConnector(topology.secondCoupling, cx1, cx2, labelY = 132)
      else Vector.empty

    headerBanner(d) ++ s1 ++ s2.getOrElse(Vector.empty) ++ divider ++ footerInfo(d) ++ legend()

  /** Visual anchor (x, y in viewBox units) of a body within a stage group
    * centred at `cx`. Suns sit at the centre (y=0), rings at the top rim
    * (y=-100), carriers slightly below centre (y=+40 — the carrier disc). */
  private def bodyAnchor(b: Body, cx1: Double, cx2: Double): (Double, Double) = b match
    case Body.S1 => (cx1, 0.0)
    case Body.C1 => (cx1, 40.0)
    case Body.R1 => (cx1, -100.0)
    case Body.S2 => (cx2, 0.0)
    case Body.C2 => (cx2, 40.0)
    case Body.R2 => (cx2, -100.0)

  /** Inter-stage coupling annotation overlaid between the two stage groups.
    *
    * `PlanetStack` and `CarrierToCarrier` draw the legacy "shared" centre line;
    * every other weld draws a curved arc between the two welded bodies' visual
    * anchor points. The label sits at `labelY` so two stacked couplings don't
    * overlap. */
  private def couplingConnector(
      c: Coupling, cx1: Double, cx2: Double, labelY: Double
  ): Vector[SvgElement] =
    inline def label(text: String): SvgElement =
      s.text(
        s.x := "0", s.y := f"$labelY%.2f", s.textAnchor := "middle",
        s.fill := CouplingColor, s.fontSize := "9",
        s.fontFamily := "ui-monospace, monospace",
        text
      )
    c match
      case Coupling.PlanetStack =>
        Vector(
          s.line(
            s.x1 := f"${cx1 + 95}%.2f", s.y1 := "0",
            s.x2 := f"${cx2 - 95}%.2f", s.y2 := "0",
            s.stroke := CarrierColor, s.strokeOpacity := "0.55",
            s.strokeWidth := "3", svg.strokeLineCap := "butt"
          ),
          label("stacked planets")
        )
      case Coupling.CarrierToCarrier =>
        Vector(
          s.line(
            s.x1 := f"${cx1 + 95}%.2f", s.y1 := "0",
            s.x2 := f"${cx2 - 95}%.2f", s.y2 := "0",
            s.stroke := CarrierColor, s.strokeOpacity := "0.55",
            s.strokeWidth := "3", svg.strokeLineCap := "butt"
          ),
          label("shared carrier")
        )
      case other =>
        // Curved arc between the two welded bodies' anchor points.
        val (ba, bb) = Coupling.bodyPair(other).getOrElse((Body.R1, Body.S2))
        val (ax, ay) = bodyAnchor(ba, cx1, cx2)
        val (bx, by) = bodyAnchor(bb, cx1, cx2)
        Vector(
          s.path(
            s.d := f"M $ax%.1f $ay%.1f Q 0 ${math.min(ay, by) - 30}%.1f $bx%.1f $by%.1f",
            s.fill := "none", s.stroke := CouplingColor,
            s.strokeWidth := "2.5", svg.strokeLineCap := "butt",
            s.strokeOpacity := "0.85"
          ),
          label(Coupling.label(other))
        )

  private def headerBanner(d: CompoundDesign): Vector[SvgElement] =
    val title = d.topology.legacyMode match
      case Some(Mode.SingleStage) => "Single-stage planetary"
      case Some(Mode.Standard)    => "Compound planetary (sun-driven)"
      case Some(Mode.CDCP)        => "Carrier-driven compound planetary"
      case Some(Mode.NSS)         => "Compound planetary (no second sun)"
      case None if d.topology == Topology.sunlessCdcpCompound => "Carrier-driven compound planetary (sun-less)"
      case None                   => if d.stage2.isEmpty then "Single-stage planetary (custom roles)" else "Compound planetary (custom topology)"
    Vector(
      s.text(
        s.x := "0", s.y := "-138", s.textAnchor := "middle",
        s.fill := TextColor, s.fontSize := "13", s.fontWeight := "600",
        s.fontFamily := "ui-sans-serif, system-ui",
        title
      ),
      s.text(
        s.x := "0", s.y := "-122", s.textAnchor := "middle",
        s.fill := SubText, s.fontSize := "11",
        s.fontFamily := "ui-monospace, monospace",
        {
          val r = d.ratio.value
          val sign = if r < 0 then "−" else ""
          val arrow = if r < 0 then " ↺" else ""
          f"ratio $sign${math.abs(r)}%.3f : 1$arrow     η ${UiLogic.fmtEfficiency(d.efficiency)}     N=${d.stage1.set.numPlanets}"
        }
      )
    )

  /** Tiny colour-keyed legend in the bottom-left of the schematic. */
  private def legend(): Vector[SvgElement] =
    val baseX = -210.0
    val baseY = -146.0
    def dot(x: Double, color: String): SvgElement =
      s.circle(s.cx := f"$x%.2f", s.cy := f"$baseY%.2f",
               s.r := "3", s.fill := color)
    def text(x: Double, content: String): SvgElement =
      s.text(s.x := f"$x%.2f", s.y := f"${baseY + 3}%.2f",
             s.fill := SubText, s.fontSize := "9",
             s.fontFamily := "ui-monospace, monospace", content)
    Vector(
      dot(baseX,         InputColor),  text(baseX + 6, "input"),
      dot(baseX + 40,    OutputColor), text(baseX + 46, "output"),
      dot(baseX + 84,    FixedColor),  text(baseX + 90, "fixed")
    )

  private def footerInfo(d: CompoundDesign): Vector[SvgElement] =
    val module1 = f"m₁=${d.stage1.module.value}%.2f mm"
    val module2 = d.stage2.fold("")(s2 => f"     m₂=${s2.module.value}%.2f mm")
    val c2c     = f"     C2C=${d.stage1.centerToCenter}%.1f mm"
    Vector(
      s.text(
        s.x := "0", s.y := "148", s.textAnchor := "middle",
        s.fill := SubText, s.fontSize := "10",
        s.fontFamily := "ui-monospace, monospace",
        s"$module1$module2$c2c"
      )
    )

  private def rotateXform(angleRad: Double, ox: Double, oy: Double): String =
    val deg = angleRad * 180.0 / math.Pi
    s"rotate(${"%.4f".format(deg)} ${"%.3f".format(ox)} ${"%.3f".format(oy)})"

  // ── Involute tooth geometry ────────────────────────────────────────────────
  //
  // Two involute gears at the correct centre distance mesh tangentially along
  // the common line of action regardless of relative rotation. The flanks
  // touch as smooth curves, sliding past each other — there is no discrete
  // tip-on-tooth clash to engineer around, and no fragile phase that needs to
  // align across N simultaneous contacts. This is what real gears use.

  private val PressureAngleRad: Double = GearGeometry.PressureAngleRad
  private val FlankSamples: Int  = 16
  private val TipSamples: Int    = 4
  private val TroughSamples: Int = 3

  private inline def inv(t: Double): Double = GearGeometry.inv(t)

  /** Counter for unique clip-path IDs. Each rendered polygon gets its own
    * clipPath so the rendered stroke can be clipped to the polygon interior. */
  private var clipIdCounter: Int = 0
  private def nextClipId(): String =
    clipIdCounter += 1
    s"cpgc-clip-$clipIdCounter"

  /** SVG `clip-path` attribute (not exposed by Laminar's default svg bindings). */
  private val clipPathAttr =
    svg.svgAttr[String]("clip-path", com.raquo.laminar.codecs.StringAsIsCodec, namespace = None)


  /** External involute gear (sun, planet). Polygon traces tip → right flank
    * (involute) → trough at root → next tooth, around the full circumference.
    */
  private def involuteToothedGear(
      cx: Double, cy: Double,
      teeth: Int, modulePx: Double,
      color: String,
      rotateRad: Double = 0.0,
      fillOpacity: Double = 0.55,
      strokeWidth: Double = 0.6
  ): SvgElement =
    val r        = coreGearRadii(teeth, modulePx, internal = false)
    val twoPi    = 2.0 * math.Pi
    val toothPitch = twoPi / teeth
    val deltaBase  = math.Pi / (2.0 * teeth) + inv(PressureAngleRad)

    def alphaAt(rr: Double): Double =
      if rr <= r.base then 0.0 else math.acos(r.base / rr)

    val sb = StringBuilder()
    var first = true
    inline def add(x: Double, y: Double): Unit =
      if first then { sb ++= s"M ${"%.3f".format(x)} ${"%.3f".format(y)} "; first = false }
      else            sb ++= s"L ${"%.3f".format(x)} ${"%.3f".format(y)} "
    inline def addPolar(rr: Double, theta: Double): Unit =
      add(cx + rr * math.cos(theta), cy + rr * math.sin(theta))

    val rFlankStart = math.max(r.base, r.root)
    val flankSpan   = r.outer - rFlankStart

    var k = 0
    while k < teeth do
      val thetaC = k * toothPitch + rotateRad

      // Right flank: rFlankStart → rOuter
      var s = 0
      while s < FlankSamples do
        val frac = s.toDouble / (FlankSamples - 1)
        val rr   = rFlankStart + frac * flankSpan
        addPolar(rr, thetaC + deltaBase - inv(alphaAt(rr)))
        s += 1

      // Tip arc
      val alphaA       = alphaAt(r.outer)
      val thetaRightTop = thetaC + deltaBase - inv(alphaA)
      val thetaLeftTop  = thetaC - deltaBase + inv(alphaA)
      s = 1
      while s <= TipSamples do
        val frac = s.toDouble / (TipSamples + 1)
        addPolar(r.outer, thetaRightTop + frac * (thetaLeftTop - thetaRightTop))
        s += 1

      // Left flank: rOuter → rFlankStart
      s = 0
      while s < FlankSamples do
        val frac = 1.0 - s.toDouble / (FlankSamples - 1)
        val rr   = rFlankStart + frac * flankSpan
        addPolar(rr, thetaC - deltaBase + inv(alphaAt(rr)))
        s += 1

      // Drop to root if r_root < r_base
      val thetaLeftBase = thetaC - deltaBase
      if r.root < r.base then addPolar(r.root, thetaLeftBase)

      // Trough samples
      val thetaNextRightBase = thetaC + toothPitch + deltaBase
      s = 1
      while s < TroughSamples do
        val frac = s.toDouble / TroughSamples
        addPolar(r.root, thetaLeftBase + frac * (thetaNextRightBase - thetaLeftBase))
        s += 1

      // Trough end / climb back up to next tooth's right-flank base
      if r.root < r.base then addPolar(r.root, thetaNextRightBase)

      k += 1

    sb ++= "Z"
    pathWithInsideStroke(sb.toString, color, fillOpacity, strokeWidth)

  /** Internal involute ring gear. Teeth point inward. Polygon traces the inner
    * tooth contour: tip → right flank (involute, outward) → root → next tooth.
    *
    * For an internal gear, the flank position at radius r (relative to the
    * tooth's centreline θ_c) is:
    *   θ_right(r) = θ_c + π/(2T) + (inv(α(r)) − inv(α_p))
    * At r = r_pitch this is θ_c + π/(2T) (half-pitch). The correction is
    * positive above pitch (wider at root) and negative below pitch (narrower
    * at tip). Below r_base, no involute — straight radial down to the tip
    * apex on the centreline.
    */
  private def involuteRing(
      cx: Double, cy: Double,
      teeth: Int, modulePx: Double,
      color: String,
      rotateRad: Double = 0.0,
      fillOpacity: Double = 0.0,
      strokeWidth: Double = 0.7
  ): SvgElement =
    val r          = coreGearRadii(teeth, modulePx, internal = true)
    val twoPi      = 2.0 * math.Pi
    val toothPitch = twoPi / teeth
    val deltaPitch = math.Pi / (2.0 * teeth)        // half tooth thickness at pitch
    val invP       = inv(PressureAngleRad)
    val rTip       = r.outer
    val rRoot      = r.root
    val rFlankLo   = math.max(r.base, rTip)         // lowest valid involute radius

    def alphaAt(rr: Double): Double =
      if rr <= r.base then 0.0 else math.acos(r.base / rr)

    /** Right-flank world angle (internal involute) at radius rr for a tooth
      * centred at thetaC. */
    inline def thetaRight(thetaC: Double, rr: Double): Double =
      thetaC + deltaPitch + (inv(alphaAt(rr)) - invP)

    inline def thetaLeft(thetaC: Double, rr: Double): Double =
      thetaC - deltaPitch - (inv(alphaAt(rr)) - invP)

    val sb = StringBuilder()
    var first = true
    inline def add(x: Double, y: Double): Unit =
      if first then { sb ++= s"M ${"%.3f".format(x)} ${"%.3f".format(y)} "; first = false }
      else            sb ++= s"L ${"%.3f".format(x)} ${"%.3f".format(y)} "
    inline def addPolar(rr: Double, theta: Double): Unit =
      add(cx + rr * math.cos(theta), cy + rr * math.sin(theta))

    var k = 0
    while k < teeth do
      val thetaC = k * toothPitch + rotateRad

      // Tip apex on centreline (innermost)
      addPolar(rTip, thetaC)

      // Right flank: rFlankLo → rRoot (outward). FlankSamples+1 points.
      var s = 0
      while s <= FlankSamples do
        val frac = s.toDouble / FlankSamples
        val rr   = rFlankLo + frac * (rRoot - rFlankLo)
        addPolar(rr, thetaRight(thetaC, rr))
        s += 1

      // Trough along rRoot to next tooth (TroughSamples − 1 intermediate points)
      val thetaRightRoot    = thetaRight(thetaC, rRoot)
      val thetaNextLeftRoot = thetaLeft(thetaC + toothPitch, rRoot)
      s = 1
      while s < TroughSamples do
        val frac = s.toDouble / TroughSamples
        addPolar(rRoot, thetaRightRoot + frac * (thetaNextLeftRoot - thetaRightRoot))
        s += 1

      // Left flank of next tooth: rRoot → rFlankLo. FlankSamples+1 points.
      val thetaNextC = thetaC + toothPitch
      s = 0
      while s <= FlankSamples do
        val frac = 1.0 - s.toDouble / FlankSamples
        val rr   = rFlankLo + frac * (rRoot - rFlankLo)
        addPolar(rr, thetaLeft(thetaNextC, rr))
        s += 1

      k += 1

    sb ++= "Z"
    pathWithInsideStroke(sb.toString, color, fillOpacity, strokeWidth)

  /** Render a closed polygon path with an "inside" stroke. SVG has no native
    * `stroke-alignment: inside`, so we use the well-known clip-path workaround:
    * the rendered path is clipped to itself, so the half of the stroke that
    * would paint outside the polygon's geometric outline gets clipped away.
    * The visible boundary is exactly the polygon outline, regardless of stroke
    * width — and the painted stroke width must be DOUBLED to compensate for
    * losing the outside half.
    */
  private def pathWithInsideStroke(
      pathD: String, color: String, fillOpacity: Double, strokeWidth: Double
  ): SvgElement =
    val clipId = nextClipId()
    s.g(
      s.defs(
        svg.clipPathTag(
          s.idAttr := clipId,
          s.path(s.d := pathD)
        )
      ),
      s.path(
        s.d := pathD,
        s.fill := color, s.fillOpacity := fillOpacity.toString,
        s.stroke := color, s.strokeWidth := (strokeWidth * 2.0).toString,
        svg.strokeLineJoin := "round",
        clipPathAttr := s"url(#$clipId)"
      )
    )

  private def stageGroup(
      stage: Stage,
      cx: Double,
      label: String,
      sunColor: String,
      planetColor: String,
      ringColor: String,
      angles: Signal[BodyAngles],
      stageIndex: Int,
      showSun: Boolean,
      sunRole: Role,
      carrierRole: Role,
      ringRole: Role,
  ): Vector[SvgElement] =
    val targetOuterR = 95.0
    val rawOuterR    = stage.ringOuterDiameter / 2.0
    val scale        = targetOuterR / rawOuterR
    val px           = (x: Double) => x * scale

    val mPx          = px(stage.module.value)
    val sunPitchR    = mPx * stage.set.sun.toDouble / 2.0
    val planetPitchR = mPx * stage.set.planet.toDouble / 2.0
    val ringPitchR   = mPx * stage.set.ring.toDouble / 2.0
    // Ring's outer rim flush with the polygon's back-of-tooth (dedendum).
    val ringOR    = mPx * stage.set.ring.toDouble / 2.0 + 1.25 * mPx
    val carrierR  = px(stage.centerToCenter)
    val n         = stage.set.numPlanets

    val sunSig: Signal[Double] = angles.map(b =>
      if stageIndex == 1 then b.sun1 else b.sun2)
    val carrierSig: Signal[Double] = angles.map(b =>
      if stageIndex == 1 then b.carrier1 else b.carrier2)
    val planetSpinSig: Signal[Double] = angles.map(b =>
      if stageIndex == 1 then b.planet1Spin else b.planet2Spin)
    val ringSig: Signal[Double] = angles.map(b =>
      if stageIndex == 1 then b.ring1 else b.ring2)

    val ringStroke   = roleStrokeColor(ringRole, ringColor)
    val ringStrokeW  = roleStrokeWidth(ringRole, 2.0)
    val sunStroke    = roleStrokeColor(sunRole, sunColor)

    // Dashed pitch circle
    def pitchCircle(centerX: Double, centerY: Double, radius: Double): SvgElement =
      s.circle(
        s.cx := f"$centerX%.2f", s.cy := f"$centerY%.2f",
        s.r := f"$radius%.2f",
        s.fill := "none", s.stroke := SubText, s.strokeWidth := "0.8",
        svg.strokeDashArray := "2.5 2.5", svg.strokeLineCap := "butt",
        s.strokeOpacity := "0.7"
      )

    // Ring: rotates about stage center.
    val ringEl = s.g(
      svg.transform <-- ringSig.map(rotateXform(_, cx, 0)),
      // Outer rim
      s.circle(
        s.cx := f"$cx%.2f", s.cy := "0",
        s.r := f"$ringOR%.2f",
        s.fill := "none", s.stroke := ringStroke, s.strokeWidth := f"$ringStrokeW%.2f"
      ),
      // Inner pitch circle (faint)
      pitchCircle(cx, 0, ringPitchR),
      // Involute inner tooth profile — static phase offset so planet-ring mesh
      // satisfies the internal-mesh phase relation `f_r − f_p_outward ≡ 0.5`.
      involuteRing(
        cx, 0, stage.set.ring.toInt, mPx, ringStroke,
        rotateRad = ringPhaseOffset(
          stage.set.sun.toInt, stage.set.planet.toInt, stage.set.ring.toInt,
          -math.Pi / 2.0
        )
      )
    )

    // Role badge (IN / OUT) for the gear at (px, py), in role's colour.
    def roleBadge(role: Role, x: Double, y: Double): SvgElement = role match
      case Role.Input  =>
        s.text(s.x := f"$x%.2f", s.y := f"$y%.2f", s.textAnchor := "middle",
               s.fill := InputColor, s.fontSize := "8",
               s.fontWeight := "700",
               s.fontFamily := "ui-monospace, monospace",
               s.pointerEvents := "none",
               "IN")
      case Role.Output =>
        s.text(s.x := f"$x%.2f", s.y := f"$y%.2f", s.textAnchor := "middle",
               s.fill := OutputColor, s.fontSize := "8",
               s.fontWeight := "700",
               s.fontFamily := "ui-monospace, monospace",
               s.pointerEvents := "none",
               "OUT")
      case Role.Fixed  =>
        s.text(s.x := f"$x%.2f", s.y := f"$y%.2f", s.textAnchor := "middle",
               s.fill := FixedColor, s.fontSize := "8",
               s.fontWeight := "700",
               s.fontFamily := "ui-monospace, monospace",
               s.pointerEvents := "none",
               "FIXED")
      case Role.Plain  => s.g()

    // Sun: rotates about stage center.
    val sunEl =
      if !showSun then s.g()
      else s.g(
        svg.transform <-- sunSig.map(rotateXform(_, cx, 0)),
        s.circle(
          s.cx := f"$cx%.2f", s.cy := "0",
          s.r := f"${(sunPitchR + mPx) * 1.35}%.2f",
          s.fill := "url(#sun-glow)"
        ),
        pitchCircle(cx, 0, sunPitchR),
        involuteToothedGear(cx, 0, stage.set.sun.toInt, mPx, sunStroke),
        {
          val sunHubR = sunRole match
            case Role.Plain => 2.4
            case _          => 3.4
          val sunHubW = sunRole match
            case Role.Plain => 0.8
            case _          => 2.0
          s.circle(s.cx := f"$cx%.2f", s.cy := "0", s.r := f"$sunHubR%.2f",
                   s.fill := HubFill, s.stroke := sunStroke,
                   s.strokeWidth := f"$sunHubW%.2f")
        }
      )

    // Ring-level static IN/OUT badge near the ring rim (placed outside the
    // rotating ring group so the badge text stays upright).
    val ringRoleBadge = ringRole match
      case Role.Plain => s.g()
      case _          => roleBadge(ringRole, cx, -ringOR - 4)

    // Sun-level static IN/OUT badge at the centre (offset slightly above the
    // sun pin so the rotating sun doesn't sweep over it).
    val sunRoleBadge = sunRole match
      case Role.Plain         => s.g()
      case _ if !showSun      => s.g()
      case _                  => roleBadge(sunRole, cx, -sunPitchR - mPx * 1.6)

    // Carrier: outer <g> rotates about stage center; planets sit inside at static
    // angles, with their own inner <g> handling per-planet spin in the carrier frame.
    val carrierStroke = roleStrokeColor(carrierRole, CarrierColor)
    val carrierStrokeW = roleStrokeWidth(carrierRole, 1.2)
    val carrierOpacity = carrierRole match
      case Role.Plain => "0.7"
      case _          => "1.0"
    val carrierG = s.g(
      svg.transform <-- carrierSig.map(rotateXform(_, cx, 0)),
      // Carrier circle (rotationally symmetric, but inside this group anyway)
      s.circle(
        s.cx := f"$cx%.2f", s.cy := "0",
        s.r := f"$carrierR%.2f",
        s.fill := "none", s.stroke := carrierStroke,
        s.strokeWidth := f"$carrierStrokeW%.2f",
        svg.strokeDashArray := "3 3", svg.strokeLineCap := "butt",
        s.strokeOpacity := carrierOpacity
      ),
      (0 until n).flatMap { i =>
        val theta = 2.0 * math.Pi * i / n - math.Pi / 2.0
        val pxC   = cx + carrierR * math.cos(theta)
        val pyC   = carrierR * math.sin(theta)
        // Carrier spoke (static within carrier <g>)
        val spoke = s.line(
          s.x1 := f"$cx%.2f", s.y1 := "0",
          s.x2 := f"$pxC%.2f", s.y2 := f"$pyC%.2f",
          s.stroke := carrierStroke, s.strokeOpacity := "0.35", s.strokeWidth := "1.2"
        )
        // Planet body: spin relative to the carrier frame
        val planet = s.g(
          svg.transform <-- planetSpinSig.combineWith(carrierSig).map { (ps, c) =>
            rotateXform(ps - c, pxC, pyC)
          },
          pitchCircle(pxC, pyC, planetPitchR),
          involuteToothedGear(pxC, pyC, stage.set.planet.toInt, mPx, planetColor,
            rotateRad = theta + planetPhaseOffset(stage.set.sun.toInt, stage.set.planet.toInt, theta)),
          s.circle(s.cx := f"$pxC%.2f", s.cy := f"$pyC%.2f", s.r := "1.6",
                   s.fill := HubFill, s.stroke := planetColor, s.strokeWidth := "0.6")
        )
        Vector(spoke, planet)
      }
    )

    // Carrier-level IN badge at the bottom of the carrier circle.
    val carrierRoleBadge = carrierRole match
      case Role.Plain => s.g()
      case _          => roleBadge(carrierRole, cx, carrierR + 9)

    val labelText =
      val pr = s"P${stage.set.planet.toInt} R${stage.set.ring.toInt}"
      if showSun then s"$label · S${stage.set.sun.toInt} $pr" else s"$label · $pr"
    val labelEl = s.text(
      s.x := f"$cx%.2f", s.y := f"${ringOR + 16}%.2f", s.textAnchor := "middle",
      s.fill := SubText, s.fontSize := "11", s.fontFamily := "ui-monospace, monospace",
      labelText
    )

    Vector(ringEl, carrierG, sunEl, ringRoleBadge, sunRoleBadge, carrierRoleBadge, labelEl)

