package cpgc.web

import cpgc.constraints.{Body, Coupling, Topology}
import cpgc.domain.Stage
import cpgc.render.GearGeometry
import cpgc.render.GearGeometry.{gearRadii as coreGearRadii}

// Pure schematic geometry (stage scale and spacing, radii, badge anchors, couplings) plus the one tooth-outline
// builder shared with the 3D view; no DOM, no Laminar.
object SchematicLayout:

  /** Outer radius of every stage's ring in viewBox units; each stage is scaled to it. */
  val TargetOuterR = 95.0

  /** Stage centres (x in viewBox units): side by side for a compound, centred for a single stage. */
  def stageCenters(twoStage: Boolean): (Double, Double) =
    if twoStage then (-110.0, 110.0) else (0.0, 0.0)

  /** Drawn geometry of one stage, centred at (`cx`, 0). `scale` (drawing units per mm) is the one
    * mm to drawing conversion; every other length here is already in drawing units. */
  final case class StageLayout(
      cx: Double,
      scale: Double,
      mPx: Double,
      ringOR: Double,
      sunOR: Double,
      carrierR: Double,
      ringBadge: (Double, Double),
      sunBadge: (Double, Double),
      carrierBadge: (Double, Double),
      labelY: Double
  )

  def stageLayout(stage: Stage, cx: Double): StageLayout =
    val rawOuterR    = stage.ringOuterDiameter / 2.0
    val scale        = TargetOuterR / rawOuterR
    val px           = (x: Double) => x * scale
    val mPx          = px(stage.module.value)
    val sunPitchR    = mPx * stage.set.sun.toDouble / 2.0
    // Ring's outer rim flush with the polygon's back-of-tooth (dedendum).
    val ringOR    = coreGearRadii(stage.set.ring.toInt, mPx, internal = true).root
    val carrierR  = px(stage.centerToCenter)
    StageLayout(
      cx = cx, scale = scale, mPx = mPx,
      ringOR = ringOR, sunOR = coreGearRadii(stage.set.sun.toInt, mPx, internal = false).outer,
      carrierR = carrierR,
      ringBadge = (cx, -ringOR - 4),
      sunBadge = (cx, -sunPitchR - mPx * 1.6),
      carrierBadge = (cx, carrierR + 9),
      labelY = ringOR + 16
    )

  def rotateXform(angleRad: Double, ox: Double, oy: Double): String =
    val deg = angleRad * 180.0 / math.Pi
    s"rotate(${"%.4f".format(deg)} ${"%.3f".format(ox)} ${"%.3f".format(oy)})"

  // ── Inter-stage couplings ──────────────────────────────────────────────────

  /** Which coupling (1 = first, 2 = second) joins each body, and which one stacks the planets.
    * Absent suns never appear: `Topology` forbids welding them. Single stages have none. */
  final case class CouplingMarks(bodies: Map[Body, Int], planets: Option[Int])

  def couplingMarks(t: Topology): CouplingMarks =
    if t.stages < 2 then CouplingMarks(Map.empty, None)
    else
      val slots = Seq(1 -> t.firstCoupling, 2 -> t.secondCoupling)
      val bodies = (for
        (slot, c) <- slots
        (a, b)    <- Coupling.bodyPair(c).toSeq
        body      <- Seq(a, b) if t.presentBodies.contains(body)
      yield body -> slot).toMap
      CouplingMarks(bodies, slots.collectFirst { case (slot, Coupling.PlanetStack) => slot })

  /** Legend text for a coupling. */
  def couplingLegendLabel(c: Coupling): String = c match
    case Coupling.PlanetStack      => "stacked planets (one part)"
    case Coupling.CarrierToCarrier => "Carrier 1 ↔ Carrier 2 (shared)"
    case other                     => Coupling.label(other)

  /** Drawn connector between two coupled bodies: quadratic Bézier `from` → `to` via `ctrl`. */
  final case class Connector(from: (Double, Double), ctrl: (Double, Double), to: (Double, Double))

  /** Angle (degrees above/below the horizontal) at which a central weld leaves a body's rim,
    * facing the other stage. Slot 1 runs above the mid-line, slot 2 below, so two welds never cross. */
  val PortAngleDeg = 35.0

  /** Outer radius of a central body in the drawing; a coupled carrier is welded at its hub. */
  def rimRadius(b: Body, lay: StageLayout): Double = b match
    case Body.S1 | Body.S2 => lay.sunOR
    case Body.C1 | Body.C2 => lay.carrierR
    case Body.R1 | Body.R2 => lay.ringOR

  /** Connector for a central weld in coupling `slot` (1 or 2), ending on the welded bodies' rims,
    * facing each other across the gap between the stages. `CarrierToCarrier` joins the two hubs;
    * `PlanetStack` has no central connector. */
  def connector(c: Coupling, slot: Int, lay1: StageLayout, lay2: StageLayout): Option[Connector] =
    Coupling.bodyPair(c).map { (b1, b2) =>
      val phi = math.toRadians(if slot == 1 then -PortAngleDeg else PortAngleDeg)
      def port(lay: StageLayout, b: Body, toward: Double): (Double, Double) =
        if c == Coupling.CarrierToCarrier then (lay.cx, 0.0)
        else
          val r = rimRadius(b, lay)
          (lay.cx + toward * r * math.cos(phi), r * math.sin(phi))
      val from = port(lay1, b1, 1.0)
      val to   = port(lay2, b2, -1.0)
      val bow  = if c == Coupling.CarrierToCarrier then 0.0 else if slot == 1 then -10.0 else 10.0
      Connector(from, ((from._1 + to._1) / 2.0, (from._2 + to._2) / 2.0 + bow), to)
    }

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

  // The one implementation of the tooth outlines: closed, simple polygons (first point not repeated),
  // counter-clockwise per tooth, about the origin in y-down convention. The scene stores them (in mm); the
  // SVG paths are just their text and the 3D view extrudes them.

  /** SVG path data of a closed polygon. */
  def polygonPath(pts: Vector[(Double, Double)]): String =
    val sb = StringBuilder()
    pts.iterator.zipWithIndex.foreach { case ((x, y), i) =>
      sb ++= (if i == 0 then "M " else "L ") ++= "%.3f %.3f ".format(x, y)
    }
    sb ++= "Z"
    sb.toString

  /** A scene point (mm, about the stage axis) in the drawing: scaled by `k`, about the axis at x = `cx`. */
  def place(p: (Double, Double), k: Double, cx: Double): (Double, Double) = (cx + k * p._1, k * p._2)

  /** Centre of a scene gear in the drawing. */
  def gearCentre(g: GearboxScene.Gear, k: Double, cx: Double): (Double, Double) = place(g.centre, k, cx)

  /** SVG path of a scene gear's outline in the drawing. */
  def gearPath(g: GearboxScene.Gear, k: Double, cx: Double): String =
    val (ox, oy) = gearCentre(g, k, cx)
    polygonPath(g.outline.map((x, y) => (ox + k * x, oy + k * y)))

  /** Outline of an external involute gear (sun, planet) about the origin, in the unit of `module`. */
  def externalGearPoints(teeth: Int, module: Double, rotateRad: Double): Vector[(Double, Double)] =
    val r          = coreGearRadii(teeth, module, internal = false)
    val toothPitch = 2.0 * math.Pi / teeth
    val deltaBase  = math.Pi / (2.0 * teeth) + inv(PressureAngleRad)
    def alphaAt(rr: Double): Double = if rr <= r.base then 0.0 else math.acos(r.base / rr)

    val out = Vector.newBuilder[(Double, Double)]
    def addPolar(rr: Double, theta: Double): Unit = out += ((rr * math.cos(theta), rr * math.sin(theta)))

    val rFlankStart = math.max(r.base, r.root)
    val flankSpan   = r.outer - rFlankStart
    var k = 0
    while k < teeth do
      val thetaC = k * toothPitch + rotateRad
      // Per tooth: left flank up, tip, right flank down, then the trough to the next tooth.
      if r.root < r.base then addPolar(r.root, thetaC - deltaBase)
      var s = 0
      while s < FlankSamples do
        val rr = rFlankStart + (s.toDouble / (FlankSamples - 1)) * flankSpan
        addPolar(rr, thetaC - deltaBase + inv(alphaAt(rr)))
        s += 1
      val alphaA        = alphaAt(r.outer)
      val thetaRightTop = thetaC + deltaBase - inv(alphaA)
      val thetaLeftTop  = thetaC - deltaBase + inv(alphaA)
      s = 1
      while s <= TipSamples do
        addPolar(r.outer, thetaLeftTop + (s.toDouble / (TipSamples + 1)) * (thetaRightTop - thetaLeftTop))
        s += 1
      s = 0
      while s < FlankSamples do
        val rr = rFlankStart + (1.0 - s.toDouble / (FlankSamples - 1)) * flankSpan
        addPolar(rr, thetaC + deltaBase - inv(alphaAt(rr)))
        s += 1
      val thetaRightBase = thetaC + deltaBase
      if r.root < r.base then addPolar(r.root, thetaRightBase)
      val thetaNextLeftBase = thetaC + toothPitch - deltaBase
      s = 1
      while s < TroughSamples do
        addPolar(r.root, thetaRightBase + (s.toDouble / TroughSamples) * (thetaNextLeftBase - thetaRightBase))
        s += 1
      k += 1
    out.result()

  /** Outline of an internal involute ring gear's tooth side (teeth point inward) about the origin. */
  def internalGearPoints(teeth: Int, module: Double, rotateRad: Double): Vector[(Double, Double)] =
    val r          = coreGearRadii(teeth, module, internal = true)
    val toothPitch = 2.0 * math.Pi / teeth
    val deltaPitch = math.Pi / (2.0 * teeth)
    val invP       = inv(PressureAngleRad)
    val rTip       = r.outer
    val rRoot      = r.root
    val rFlankLo   = math.max(r.base, rTip)
    def alphaAt(rr: Double): Double = if rr <= r.base then 0.0 else math.acos(r.base / rr)
    def thetaRight(thetaC: Double, rr: Double): Double = thetaC + deltaPitch + (inv(alphaAt(rr)) - invP)
    def thetaLeft(thetaC: Double, rr: Double): Double  = thetaC - deltaPitch - (inv(alphaAt(rr)) - invP)

    val out = Vector.newBuilder[(Double, Double)]
    def addPolar(rr: Double, theta: Double): Unit = out += ((rr * math.cos(theta), rr * math.sin(theta)))

    var k = 0
    while k < teeth do
      val thetaC = k * toothPitch + rotateRad
      addPolar(rTip, thetaC)
      var s = 0
      while s <= FlankSamples do
        val rr = rFlankLo + (s.toDouble / FlankSamples) * (rRoot - rFlankLo)
        addPolar(rr, thetaRight(thetaC, rr))
        s += 1
      val thetaRightRoot    = thetaRight(thetaC, rRoot)
      val thetaNextLeftRoot = thetaLeft(thetaC + toothPitch, rRoot)
      s = 1
      while s < TroughSamples do
        addPolar(rRoot, thetaRightRoot + (s.toDouble / TroughSamples) * (thetaNextLeftRoot - thetaRightRoot))
        s += 1
      val thetaNextC = thetaC + toothPitch
      s = 0
      while s <= FlankSamples do
        val rr = rFlankLo + (1.0 - s.toDouble / FlankSamples) * (rRoot - rFlankLo)
        addPolar(rr, thetaLeft(thetaNextC, rr))
        s += 1
      k += 1
    out.result()
