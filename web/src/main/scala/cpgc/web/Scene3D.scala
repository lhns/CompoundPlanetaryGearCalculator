package cpgc.web

import cpgc.constraints.{Body, Coupling}
import cpgc.kinematics.BodyAngles
import cpgc.render.GearGeometry
import cpgc.web.GearboxScene.{Gear, Scene}
import cpgc.web.Palette.Tone

/** The 3D-only layer on top of `GearboxScene`: depth. It adds stage offsets, the face width, extrusion
  * spans, carrier plates and pins, the stacked planet's hub and the weld solids. Positions, sizes,
  * counts, angles and colours all come from the scene; nothing here recomputes them.
  *
  * Coordinates are millimetres, x right, y up (the scene's y is flipped), z along the axis with stage 1
  * at the bottom. A schematic rotation by `a` is a rotation by `-a` about z. Each cluster is a group of
  * welded bodies turning as one; planets sit in their carrier's cluster. */
object Scene3D:

  /** Gear face width as a fraction of the larger ring outer diameter. */
  val FaceWidthFactor = 0.12
  /** Axial gap between the stages, as a fraction of the face width. */
  val StageGapFactor = 0.35

  private val PlateFactor    = 0.10 // carrier plate thickness / face width
  private val PlateClearance = 0.04 // plate-to-gear clearance / face width
  private val ArmFactor      = 0.08 // ring-weld spoke thickness / face width

  /** A solid in its group's local frame; `tone` is the scene's colour key. */
  sealed trait Prim:
    def tone: Tone
    def z0: Double
    def z1: Double

  /** A scene gear extruded over z0..z1: solid, or (ring) a disc of `gear.outerRadius` with the outline as a hole. */
  final case class Extruded(gear: Gear, z0: Double, z1: Double) extends Prim:
    def tone: Tone = gear.tone
    def outline: Vector[(Double, Double)] = gear.outline.map((x, y) => (x, -y))

  /** Cylinder about the axis through (x, y); with different radii a cone, e.g. a ring-to-ring sleeve. */
  final case class Cyl(
      x: Double, y: Double, rTop: Double, rBottom: Double, z0: Double, z1: Double, openEnded: Boolean, tone: Tone
  ) extends Prim

  /** Straight bar of `width` from (x0, y0) to (x1, y1), over z0..z1. */
  final case class Bar(
      x0: Double, y0: Double, x1: Double, y1: Double, width: Double, z0: Double, z1: Double, tone: Tone
  ) extends Prim

  /** A planet (a stacked one spans both stages); turns about its own axis relative to its carrier.
    * `gear` is the stage-1 gear of a stacked planet. */
  final case class PlanetPart(gear: Gear, x: Double, y: Double, carrier: Body, stacked: Boolean, prims: Vector[Prim])

  /** Rigidly welded bodies. `angleBody` supplies the rotation. */
  final case class Cluster(
      bodies: Vector[Body], angleBody: Body, prims: Vector[Prim], connectors: Vector[Prim], planets: Vector[PlanetPart]
  )

  final case class Layout(
      faceWidth: Double,
      stageGap: Double,
      stage1Z: Double,
      stage2Z: Option[Double],
      zMin: Double,
      zMax: Double,
      outerRadius: Double,
      clusters: Vector[Cluster]
  ):
    def allPrims: Vector[Prim] = clusters.flatMap(c => c.prims ++ c.connectors ++ c.planets.flatMap(_.prims))

  def build(scene: Scene): Layout =
    val t        = scene.design.topology
    val twoStage = scene.stages.size == 2
    val outerR   = scene.stages.map(_.stage.ringOuterDiameter / 2.0).max
    val w        = FaceWidthFactor * 2.0 * outerR
    val gap      = StageGapFactor * w
    val plateT   = PlateFactor * w
    val clr      = PlateClearance * w
    val armT     = ArmFactor * w
    val z1       = 0.0
    val z2       = w + gap
    val zOf      = Vector(z1, z2)
    val stacked  = scene.stacked
    val shaftR   = 0.3 * scene.stages.map(s => s.stage.module.value * s.stage.set.sun.toDouble / 2.0).min

    def stageOf(b: Body): Int = GearboxScene.stageOfBody(b)
    def sc(i: Int)            = scene.stages(i - 1)
    def planetRoot(g: Gear)   = GearGeometry.gearRadii(g.teeth, g.module, internal = false).root
    def pinR(i: Int)          = 0.28 * planetRoot(sc(i).planets.head)
    def ringR(i: Int)         = sc(i).ring.outerRadius.get
    // Carrier plates sit outside the stack: below stage 1 and above stage 2.
    def plateZ(i: Int): (Double, Double) =
      if i == 1 then (-clr - plateT, -clr) else (z2 + w + clr, z2 + w + clr + plateT)
    def slotTone(slot: Int): Tone = if slot == 1 then Tone.Cpl1 else Tone.Cpl2

    // Where a weld meets each body, along z.
    def attachZ(b: Body): Double = b match
      case Body.S1 | Body.S2 => zOf(stageOf(b) - 1) + w / 2.0
      case Body.R1           => z1 + w
      case Body.R2           => z2
      case Body.C1           => (plateZ(1)._1 + plateZ(1)._2) / 2.0
      case Body.C2           => (plateZ(2)._1 + plateZ(2)._2) / 2.0

    def bodyPrims(b: Body): Vector[Prim] =
      val i = stageOf(b)
      val s = sc(i)
      val z = zOf(i - 1)
      b match
        case Body.S1 | Body.S2 => s.sun.toVector.map(Extruded(_, z, z + w))
        case Body.R1 | Body.R2 => Vector(Extruded(s.ring, z, z + w))
        case Body.C1 | Body.C2 =>
          val c        = s.carrier
          val (p0, p1) = plateZ(i)
          val arms     = c.arms.map((x, y) => (x, -y))
          val armW     = 3.0 * pinR(i)
          // a stacked planet's pin runs the full height, so stage 2's carrier only adds its plate
          val pinSpan =
            if stacked then (if i == 1 then Some((p0, plateZ(2)._2)) else None)
            else if i == 1 then Some((p0, z1 + w))
            else Some((z2, p1))
          Vector(Cyl(0, 0, 0.8 * armW, 0.8 * armW, p0, p1, false, c.tone)) ++
            arms.map((x, y) => Bar(0, 0, x, y, armW, p0, p1, c.tone)) ++
            pinSpan.toVector.flatMap((a, e) => arms.map((x, y) => Cyl(x, y, pinR(i), pinR(i), a, e, false, c.tone)))

    def planetParts(carrier: Body, i: Int): Vector[PlanetPart] =
      sc(i).planets.map { g =>
        PlanetPart(g, g.centre._1, -g.centre._2, carrier, stacked = false, Vector(Extruded(g, zOf(i - 1), zOf(i - 1) + w)))
      }

    def stackedParts(carrier: Body): Vector[PlanetPart] =
      sc(1).planets.zip(sc(2).planets).map { case (g1, g2) =>
        val hubR = 0.5 * math.min(planetRoot(g1), planetRoot(g2))
        PlanetPart(g1, g1.centre._1, -g1.centre._2, carrier, stacked = true,
          Vector(Extruded(g1, z1, z1 + w), Cyl(0, 0, hubR, hubR, z1 + w, z2, false, g1.tone), Extruded(g2, z2, z2 + w)))
      }

    /** Solids that make a coupling visible: a shaft, ring spokes or sleeve, carrier tie posts. */
    def connector(a: Body, b: Body, weld: Tone): Vector[Prim] =
      (a, b) match
        case (Body.R1, Body.R2) =>
          Vector(Cyl(0, 0, ringR(2), ringR(1), z1 + w, z2, true, weld))
        case (Body.C1, Body.C2) if stacked => Vector.empty
        case (Body.C1, Body.C2) =>
          val r = math.max(sc(1).carrier.radius, sc(2).carrier.radius)
          val n = sc(1).planets.size
          Vector.tabulate(n) { k =>
            val th = 2.0 * math.Pi * (k + 0.5) / n
            Cyl(r * math.sin(th), r * math.cos(th), pinR(1), pinR(1), plateZ(1)._1, plateZ(2)._2, false, weld)
          }
        case _ =>
          def spokes(r: Body): Vector[Prim] =
            if r != Body.R1 && r != Body.R2 then Vector.empty
            else
              val (rad, za, zb) =
                if r == Body.R1 then (ringR(1), z1 + w, z1 + w + armT) else (ringR(2), z2 - armT, z2)
              Vector.tabulate(3) { k =>
                val th = 2.0 * math.Pi * k / 3
                Bar(0, 0, rad * math.cos(th), rad * math.sin(th), 2.0 * shaftR, za, zb, weld)
              }
          val (za, zb) = (attachZ(a), attachZ(b))
          Vector(Cyl(0, 0, shaftR, shaftR, math.min(za, zb), math.max(za, zb), false, weld)) ++ spokes(a) ++ spokes(b)

    val connectors: Vector[(Body, Vector[Prim])] =
      scene.couplings.flatMap((slot, c) => Coupling.bodyPair(c).map(slot -> _))
        .filter { case (_, (a, b)) => t.presentBodies.contains(a) && t.presentBodies.contains(b) }
        .map { case (slot, (a, b)) => a -> connector(a, b, slotTone(slot)) }

    val clusters = scene.clusters.map { group =>
      val planets = group.flatMap {
        case b @ Body.C1             => if stacked then stackedParts(b) else planetParts(b, 1)
        case b @ Body.C2 if !stacked => planetParts(b, 2)
        case _                       => Vector.empty
      }
      Cluster(group, group.head, group.flatMap(bodyPrims),
        connectors.collect { case (a, ps) if group.contains(a) => ps }.flatten, planets)
    }

    Layout(w, gap, z1, Option.when(twoStage)(z2), plateZ(1)._1, if twoStage then plateZ(2)._2 else z1 + w,
      outerR, clusters)

  /** Rotation about z of a cluster, in the schematic's clockwise-positive convention. */
  def clusterAngle(c: Cluster, a: BodyAngles): Double = a.of(c.angleBody)

  /** Rotation of a planet relative to its carrier. */
  def planetAngle(p: PlanetPart, a: BodyAngles): Double = GearboxScene.angle(p.gear, a)
