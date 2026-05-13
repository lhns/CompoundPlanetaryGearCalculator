package cpgc.web

import com.raquo.laminar.api.L.*
import com.raquo.laminar.api.L.svg as s
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
  private val TextColor    = "#e6e6e6"
  private val SubText      = "#a6accd"

  def apply(
      designSignal: Signal[Option[CompoundDesign]],
      phaseSignal:  Signal[Double]
  ): SvgElement =
    s.svg(
      s.viewBox := "-220 -160 440 320",
      s.width   := "100%",
      s.height  := "100%",
      svg.preserveAspectRatio := "xMidYMid meet",
      defs(),
      children <-- designSignal.map {
        case Some(d) => renderDesign(d, phaseSignal)
        case None    => Vector(placeholder())
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

  private def placeholder(): SvgElement =
    s.text(
      s.x := "0", s.y := "0",
      s.textAnchor := "middle",
      s.fill := SubText, s.fontSize := "14",
      s.fontFamily := "ui-sans-serif, system-ui",
      "Run a search to view a design"
    )

  private def renderDesign(
      d: CompoundDesign,
      phaseSignal: Signal[Double]
  ): Vector[SvgElement] =
    val twoStage = d.stage2.isDefined
    val (cx1, cx2) = if twoStage then (-110.0, 110.0) else (0.0, 0.0)

    val angles: Signal[BodyAngles] = phaseSignal.map(Kinematics.at(d, _))

    val s1 = stageGroup(d.stage1, cx1, "Stage 1", Stage1Sun, Stage1Planet, Stage1Ring,
                        angles, stageIndex = 1, showSun = true)
    val s2 = d.stage2.map(st2 =>
      stageGroup(st2, cx2, "Stage 2", Stage2Sun, Stage2Planet, Stage2Ring,
                 angles, stageIndex = 2, showSun = d.mode != Mode.NSS))

    val divider =
      if twoStage then
        Vector(
          s.line(
            s.x1 := f"${cx1 + 95}%.2f", s.y1 := "0",
            s.x2 := f"${cx2 - 95}%.2f", s.y2 := "0",
            s.stroke := CarrierColor, s.strokeOpacity := "0.55",
            s.strokeWidth := "3", svg.strokeLineCap := "round"
          ),
          s.text(
            s.x := "0", s.y := "-6", s.textAnchor := "middle",
            s.fill := CarrierColor, s.fontSize := "9",
            s.fontFamily := "ui-monospace, monospace",
            "shared carrier"
          )
        )
      else Vector.empty

    headerBanner(d) ++ s1 ++ s2.getOrElse(Vector.empty) ++ divider ++ footerInfo(d)

  private def headerBanner(d: CompoundDesign): Vector[SvgElement] =
    val title = d.mode match
      case Mode.SingleStage => "Single-stage planetary"
      case Mode.Standard    => "Compound planetary (sun-driven)"
      case Mode.CDCP        => "Carrier-driven compound planetary"
      case Mode.NSS         => "Compound planetary (no second sun)"
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
        f"ratio ${d.ratio.value}%.3f : 1     η ${d.efficiency * 100}%.1f%%     N=${d.stage1.set.numPlanets}"
      )
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
      showSun: Boolean
  ): Vector[SvgElement] =
    val targetOuterR = 95.0
    val rawOuterR    = stage.ringOuterDiameter / 2.0
    val scale        = targetOuterR / rawOuterR
    val px           = (x: Double) => x * scale

    val mPx       = px(stage.module.value)
    val sunPitchR = mPx * stage.set.sun.toDouble / 2.0
    // Ring's outer rim flush with the polygon's back-of-tooth (dedendum).
    val ringOR    = mPx * stage.set.ring.toDouble / 2.0 + 1.25 * mPx
    val carrierR  = px(stage.centerToCenter)
    val n         = stage.set.numPlanets

    val sunSig: Signal[Double] = angles.map(b =>
      if stageIndex == 1 then b.sun1 else b.sun2)
    val carrierSig: Signal[Double] = angles.map(_.carrier)
    val planetSpinSig: Signal[Double] = angles.map(b =>
      if stageIndex == 1 then b.planet1Spin else b.planet2Spin)
    val ringSig: Signal[Double] = angles.map(b =>
      if stageIndex == 1 then b.ring1 else b.ring2)

    // Ring: rotates about stage center.
    val ringEl = s.g(
      svg.transform <-- ringSig.map(rotateXform(_, cx, 0)),
      // Outer rim
      s.circle(
        s.cx := f"$cx%.2f", s.cy := "0",
        s.r := f"$ringOR%.2f",
        s.fill := "none", s.stroke := ringColor, s.strokeWidth := "2"
      ),
      // Involute inner tooth profile — static phase offset so planet-ring mesh
      // satisfies the internal-mesh phase relation `f_r − f_p_outward ≡ 0.5`.
      involuteRing(
        cx, 0, stage.set.ring.toInt, mPx, ringColor,
        rotateRad = ringPhaseOffset(
          stage.set.sun.toInt, stage.set.planet.toInt, stage.set.ring.toInt,
          -math.Pi / 2.0
        )
      )
    )

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
        involuteToothedGear(cx, 0, stage.set.sun.toInt, mPx, sunColor),
        s.circle(s.cx := f"$cx%.2f", s.cy := "0", s.r := "2.4",
                 s.fill := "#0d1117", s.stroke := sunColor, s.strokeWidth := "0.8")
      )

    // Carrier: outer <g> rotates about stage center; planets sit inside at static
    // angles, with their own inner <g> handling per-planet spin in the carrier frame.
    val carrierG = s.g(
      svg.transform <-- carrierSig.map(rotateXform(_, cx, 0)),
      // Carrier circle (rotationally symmetric, but inside this group anyway)
      s.circle(
        s.cx := f"$cx%.2f", s.cy := "0",
        s.r := f"$carrierR%.2f",
        s.fill := "none", s.stroke := CarrierColor, s.strokeWidth := "0.8",
        svg.strokeDashArray := "3 3", s.strokeOpacity := "0.7"
      ),
      (0 until n).flatMap { i =>
        val theta = 2.0 * math.Pi * i / n - math.Pi / 2.0
        val pxC   = cx + carrierR * math.cos(theta)
        val pyC   = carrierR * math.sin(theta)
        // Carrier spoke (static within carrier <g>)
        val spoke = s.line(
          s.x1 := f"$cx%.2f", s.y1 := "0",
          s.x2 := f"$pxC%.2f", s.y2 := f"$pyC%.2f",
          s.stroke := CarrierColor, s.strokeOpacity := "0.35", s.strokeWidth := "1.2"
        )
        // Planet body: spin relative to the carrier frame
        val planet = s.g(
          svg.transform <-- planetSpinSig.combineWith(carrierSig).map { (ps, c) =>
            rotateXform(ps - c, pxC, pyC)
          },
          involuteToothedGear(pxC, pyC, stage.set.planet.toInt, mPx, planetColor,
            rotateRad = theta + planetPhaseOffset(stage.set.sun.toInt, stage.set.planet.toInt, theta)),
          s.circle(s.cx := f"$pxC%.2f", s.cy := f"$pyC%.2f", s.r := "1.6",
                   s.fill := "#0d1117", s.stroke := planetColor, s.strokeWidth := "0.6")
        )
        Vector(spoke, planet)
      }
    )

    val labelEl = s.text(
      s.x := f"$cx%.2f", s.y := f"${ringOR + 16}%.2f", s.textAnchor := "middle",
      s.fill := SubText, s.fontSize := "11", s.fontFamily := "ui-monospace, monospace",
      s"$label · S${stage.set.sun.toInt} P${stage.set.planet.toInt} R${stage.set.ring.toInt}"
    )

    Vector(ringEl, carrierG, sunEl, labelEl)

