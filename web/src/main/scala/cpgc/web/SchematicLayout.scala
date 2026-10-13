package cpgc.web

import cpgc.constraints.Body
import cpgc.domain.Stage
import cpgc.render.GearGeometry
import cpgc.render.GearGeometry.{gearRadii as coreGearRadii}

// Pure schematic geometry (positions, radii, angles, badge anchors, tooth outlines); no DOM, no Laminar.
object SchematicLayout:

  /** Outer radius of every stage's ring in viewBox units; each stage is scaled to it. */
  val TargetOuterR = 95.0

  /** Stage centres (x in viewBox units): side by side for a compound, centred for a single stage. */
  def stageCenters(twoStage: Boolean): (Double, Double) =
    if twoStage then (-110.0, 110.0) else (0.0, 0.0)

  /** Drawn geometry of one stage, centred at (`cx`, 0). */
  final case class StageLayout(
      cx: Double,
      scale: Double,
      mPx: Double,
      sunPitchR: Double,
      planetPitchR: Double,
      ringPitchR: Double,
      ringOR: Double,
      carrierR: Double,
      planets: Vector[(Double, Double)],
      ringBadge: (Double, Double),
      sunBadge: (Double, Double),
      carrierBadge: (Double, Double),
      labelY: Double
  )

  /** Position of planet `i` of `n` on a carrier of radius `carrierR` about (`cx`, 0):
    * planet 0 at the top, then k·360/N clockwise on screen. */
  def planetPosition(i: Int, n: Int, cx: Double, carrierR: Double): (Double, Double) =
    val theta = 2.0 * math.Pi * i / n - math.Pi / 2.0
    (cx + carrierR * math.cos(theta), carrierR * math.sin(theta))

  def stageLayout(stage: Stage, cx: Double): StageLayout =
    val rawOuterR    = stage.ringOuterDiameter / 2.0
    val scale        = TargetOuterR / rawOuterR
    val px           = (x: Double) => x * scale
    val mPx          = px(stage.module.value)
    val sunPitchR    = mPx * stage.set.sun.toDouble / 2.0
    val planetPitchR = mPx * stage.set.planet.toDouble / 2.0
    val ringPitchR   = mPx * stage.set.ring.toDouble / 2.0
    // Ring's outer rim flush with the polygon's back-of-tooth (dedendum).
    val ringOR    = coreGearRadii(stage.set.ring.toInt, mPx, internal = true).root
    val carrierR  = px(stage.centerToCenter)
    val n         = stage.set.numPlanets
    StageLayout(
      cx = cx, scale = scale, mPx = mPx,
      sunPitchR = sunPitchR, planetPitchR = planetPitchR, ringPitchR = ringPitchR,
      ringOR = ringOR, carrierR = carrierR,
      planets = Vector.tabulate(n)(planetPosition(_, n, cx, carrierR)),
      ringBadge = (cx, -ringOR - 4),
      sunBadge = (cx, -sunPitchR - mPx * 1.6),
      carrierBadge = (cx, carrierR + 9),
      labelY = ringOR + 16
    )

  /** Phasing turns → drawn angle: planet i sits at 2πi/N − π/2, so the phasing frame
    * (planet k at k/N turns) is rotated by a quarter turn; mesh conditions are invariant under that shift. */
  def toRad(turns: Double): Double = 2.0 * math.Pi * (turns - 0.25)

  def rotateXform(angleRad: Double, ox: Double, oy: Double): String =
    val deg = angleRad * 180.0 / math.Pi
    s"rotate(${"%.4f".format(deg)} ${"%.3f".format(ox)} ${"%.3f".format(oy)})"

  /** Visual anchor (x, y in viewBox units) of a body within a stage group
    * centred at `cx`. Suns sit at the centre (y=0), rings at the top rim
    * (y=-100), carriers slightly below centre (y=+40 — the carrier disc). */
  def bodyAnchor(b: Body, cx1: Double, cx2: Double): (Double, Double) = b match
    case Body.S1 => (cx1, 0.0)
    case Body.C1 => (cx1, 40.0)
    case Body.R1 => (cx1, -100.0)
    case Body.S2 => (cx2, 0.0)
    case Body.C2 => (cx2, 40.0)
    case Body.R2 => (cx2, -100.0)

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

  /** SVG path data of an external involute gear (sun, planet). Traces tip → right flank
    * (involute) → trough at root → next tooth, around the full circumference.
    */
  def externalGearPath(
      cx: Double, cy: Double,
      teeth: Int, modulePx: Double,
      rotateRad: Double
  ): String =
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
    sb.toString

  /** SVG path data of an internal involute ring gear. Teeth point inward. Traces the inner
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
  def internalGearPath(
      cx: Double, cy: Double,
      teeth: Int, modulePx: Double,
      rotateRad: Double
  ): String =
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
    sb.toString
