package cpgc.web

import cpgc.constraints.{Body, Coupling}
import cpgc.render.GearGeometry
import cpgc.web.GearboxScene.{Gear, Scene}
import cpgc.web.Palette.Tone

/** The 3D-only layer on top of `GearboxScene`: depth. It adds stage offsets, the face width, extrusion
  * spans, carrier plates and pins, the stacked planet's hub and the weld solids. Positions, sizes,
  * counts, angles and colours all come from the scene; nothing here recomputes them.
  *
  * Coordinates are millimetres, x right, y up (`toWorld` flips the scene's y, the one place where it
  * happens), z along the axis with stage 1 at the bottom. A schematic rotation by `a` is a rotation by
  * `-a` about z (applied in `ThreeScene.pose`). Each cluster is a group of welded bodies turning as one;
  * planets sit in their carrier's cluster. */
object Scene3D:

  /** Gear face width as a fraction of the larger ring outer diameter. */
  val FaceWidthFactor = 0.12
  /** Axial gap between the stages, as a fraction of the face width. */
  val StageGapFactor = 0.35

  private val PlateFactor    = 0.10 // carrier plate thickness / face width
  private val PlateClearance = 0.04 // plate-to-gear clearance / face width
  private val ArmFactor      = 0.08 // ring-weld spoke thickness / face width

  /** Scene (y down) to 3D (y up). */
  private def toWorld(p: (Double, Double)): (Double, Double) = (p._1, -p._2)

  /** A solid in its group's local frame; `tone` is the scene's colour key. */
  sealed trait Prim:
    def tone: Tone
    def z0: Double
    def z1: Double

  /** A scene gear extruded over z0..z1: solid, or (ring) a disc of `gear.outerRadius` with the outline as a hole. */
  final case class Extruded(gear: Gear, z0: Double, z1: Double) extends Prim:
    def tone: Tone = gear.tone
    def outline: Vector[(Double, Double)] = gear.outline.map(toWorld)

  /** Cylinder about the axis through (x, y); with different radii a cone, e.g. a ring-to-ring sleeve. */
  final case class Cyl(
      x: Double, y: Double, rTop: Double, rBottom: Double, z0: Double, z1: Double, openEnded: Boolean, tone: Tone
  ) extends Prim

  /** Straight bar of `width` from (x0, y0) to (x1, y1), over z0..z1. */
  final case class Bar(
      x0: Double, y0: Double, x1: Double, y1: Double, width: Double, z0: Double, z1: Double, tone: Tone
  ) extends Prim

  /** A planet (a stacked one spans both stages); turns about its own axis relative to its carrier.
    * `gear` is the stage-1 gear of a stacked planet, `(x, y)` its centre in the cluster's frame. */
  final case class PlanetPart(gear: Gear, x: Double, y: Double, stacked: Boolean, prims: Vector[Prim])

  /** Rigidly welded bodies; they turn as one (`GearboxScene.clusterAngle`). */
  final case class Cluster(
      bodies: Vector[Body], prims: Vector[Prim], connectors: Vector[Prim], planets: Vector[PlanetPart]
  )

  final case class Layout(
      faceWidth: Double,
      stage2Z: Option[Double], // stage 1 starts at z = 0
      zMin: Double,
      zMax: Double,
      outerRadius: Double,
      clusters: Vector[Cluster]
  )

  /** Radius of the sphere around the model's axis midpoint that contains it: the outer ring cylinder
    * (no part reaches further out) over the full axial span. Orientation-independent, so it holds
    * for every orbit angle. */
  def boundingRadius(l: Layout): Double =
    math.hypot(l.outerRadius, (l.zMax - l.zMin) / 2.0)

  /** Camera distance at which a sphere of `radius` fits the frustum with `margin` to spare, for a
    * vertical field of view `fovRad` and a canvas of `aspect` = width / height. Uses the narrower of
    * the vertical and horizontal fields, i.e. the larger distance. */
  def fitDistance(radius: Double, fovRad: Double, aspect: Double, margin: Double = FitMargin): Double =
    val vHalf = fovRad / 2.0
    val hHalf = math.atan(math.max(aspect, 1e-6) * math.tan(vHalf))
    radius * margin / math.sin(math.min(vHalf, hHalf))

  /** Space left around the model at the initial framing. */
  val FitMargin = 1.15

  /** Near and far clip planes for a camera `dist` from the centre of a sphere of `radius`: tight around the
    * model so the depth buffer resolves the sub-millimetre plate gaps; far/near never exceeds 1000. */
  def clipPlanes(dist: Double, radius: Double): (Double, Double) =
    val far = dist + 1.5 * radius
    (math.max(dist - 1.5 * radius, far / 1000.0), far)

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
          val arms     = c.arms.map(toWorld)
          val armW     = 3.0 * pinR(i)
          // a stacked planet's pin runs the full height, so stage 2's carrier only adds its plate. Pins stick
          // out past the planet's far face: a cap flush with it would z-fight.
          val pinSpan =
            if stacked then (if i == 1 then Some((p0, plateZ(2)._2)) else None)
            else if i == 1 then Some((p0, z1 + w + clr))
            else Some((z2 - clr, p1))
          Vector(Cyl(0, 0, 0.8 * armW, 0.8 * armW, p0, p1, false, c.tone)) ++
            arms.map((x, y) => Bar(0, 0, x, y, armW, p0, p1, c.tone)) ++
            pinSpan.toVector.flatMap((a, e) => arms.map((x, y) => Cyl(x, y, pinR(i), pinR(i), a, e, false, c.tone)))

    def planetParts(i: Int): Vector[PlanetPart] =
      sc(i).planets.map { g =>
        val (x, y) = toWorld(g.centre)
        PlanetPart(g, x, y, stacked = false, Vector(Extruded(g, zOf(i - 1), zOf(i - 1) + w)))
      }

    def stackedParts: Vector[PlanetPart] =
      sc(1).planets.zip(sc(2).planets).map { case (g1, g2) =>
        val hubR   = 0.5 * math.min(planetRoot(g1), planetRoot(g2))
        val (x, y) = toWorld(g1.centre)
        PlanetPart(g1, x, y, stacked = true,
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
        .map { case (slot, (a, b)) => a -> connector(a, b, Palette.slotTone(slot)) }

    val clusters = scene.clusters.map { group =>
      val planets = group.flatMap {
        case Body.C1             => if stacked then stackedParts else planetParts(1)
        case Body.C2 if !stacked => planetParts(2)
        case _                   => Vector.empty
      }
      Cluster(group, group.flatMap(bodyPrims),
        connectors.collect { case (a, ps) if group.contains(a) => ps }.flatten, planets)
    }

    Layout(w, Option.when(twoStage)(z2), plateZ(1)._1, if twoStage then plateZ(2)._2 else z1 + w, outerR, clusters)
