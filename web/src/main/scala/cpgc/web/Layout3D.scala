package cpgc.web

import cpgc.constraints.{Body, BodyRole, Coupling}
import cpgc.domain.{CompoundDesign, Stage}
import cpgc.kinematics.BodyAngles
import cpgc.render.{GearGeometry, Phasing}
import cpgc.web.SchematicLayout.{externalGearPoints, internalGearPoints, planetPosition, toRad}

/** Pure layout of the 3D view: which solids make up the gearbox, where they sit and which rigid
  * body each belongs to. No DOM and no three.js; `ThreeScene` turns it into meshes.
  *
  * Coordinates are millimetres, x right, y up, z along the axis with stage 1 at the bottom. The
  * schematic is y-down, so points are flipped; a schematic rotation by `a` is a rotation by `-a`
  * about z. Each *cluster* is a set of bodies welded into one rigid piece; its group turns by the
  * angle of its first body. Planets sit inside their carrier's cluster and turn relative to it. */
object Layout3D:

  /** Gear face width as a fraction of the larger ring outer diameter. */
  val FaceWidthFactor = 0.12
  /** Axial gap between the stages, as a fraction of the face width. */
  val StageGapFactor = 0.35

  private val PlateFactor    = 0.10 // carrier plate thickness / face width
  private val PlateClearance = 0.04 // plate-to-gear clearance / face width
  private val ArmFactor      = 0.08 // ring-weld spoke thickness / face width

  /** What a solid is, so the renderer can colour it (the 2D schematic's accents for pinned roles). */
  enum Tone:
    case Input, Output, Fixed, Weld, Carrier, Sun1, Ring1, Planet1, Sun2, Ring2, Planet2, Stacked

  /** A solid in its group's local frame. */
  sealed trait Prim:
    def tone: Tone
    def z0: Double
    def z1: Double

  /** Gear body: `outline` extruded over z0..z1. With `outerRadius`, a disc of that radius with
    * `outline` as a hole (ring gear); without, a solid (sun, planet). */
  final case class Extruded(
      outline: Vector[(Double, Double)], outerRadius: Option[Double], z0: Double, z1: Double, tone: Tone
  ) extends Prim

  /** Cylinder about the axis through (x, y); with different radii a cone, e.g. a ring-to-ring sleeve. */
  final case class Cyl(
      x: Double, y: Double, rTop: Double, rBottom: Double, z0: Double, z1: Double, openEnded: Boolean, tone: Tone
  ) extends Prim

  /** Straight bar of `width` from (x0, y0) to (x1, y1), over z0..z1. */
  final case class Bar(
      x0: Double, y0: Double, x1: Double, y1: Double, width: Double, z0: Double, z1: Double, tone: Tone
  ) extends Prim

  /** A planet (a stacked one spans both stages); turns about its own axis relative to `carrier`. */
  final case class PlanetPart(
      index: Int, x: Double, y: Double, stage: Int, carrier: Body, stacked: Boolean, prims: Vector[Prim]
  )

  /** Rigidly welded bodies. `angleBody` supplies the rotation. */
  final case class Cluster(
      bodies: Vector[Body], angleBody: Body, role: Option[BodyRole], prims: Vector[Prim], planets: Vector[PlanetPart]
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
    def allPrims: Vector[Prim] = clusters.flatMap(c => c.prims ++ c.planets.flatMap(_.prims))
    def hasWeld: Boolean       = allPrims.exists(_.tone == Tone.Weld)

  private def flip(pts: Vector[(Double, Double)]): Vector[(Double, Double)] = pts.map((x, y) => (x, -y))

  /** Weld groups: bodies joined by a central-shaft coupling share a cluster. */
  private[web] def clusterBodies(d: CompoundDesign): Vector[Vector[Body]] =
    val t       = d.topology
    val present = Body.values.toVector.filter(t.presentBodies.contains)
    val welds   = Vector(t.firstCoupling, t.secondCoupling).filter(_ => t.stages == 2)
      .flatMap(Coupling.bodyPair).filter((a, b) => t.presentBodies.contains(a) && t.presentBodies.contains(b))
    val merged = welds.foldLeft(present.map(Set(_))) { case (groups, (a, b)) =>
      val (hit, rest) = groups.partition(g => g.contains(a) || g.contains(b))
      rest :+ hit.flatten.toSet
    }
    merged.map(g => present.filter(g.contains)).sortBy(g => Body.values.indexOf(g.head))

  def build(d: CompoundDesign): Layout =
    val t       = d.topology
    val twoStage = d.stage2.isDefined
    val stages  = Vector(Some(d.stage1), d.stage2).flatten
    val outerR  = stages.map(_.ringOuterDiameter / 2.0).max
    val w       = FaceWidthFactor * 2.0 * outerR
    val gap     = StageGapFactor * w
    val plateT  = PlateFactor * w
    val clr     = PlateClearance * w
    val armT    = ArmFactor * w
    val z1      = 0.0
    val z2      = w + gap
    val zOf     = Vector(z1, z2)
    val phasing = Phasing.solve(d)
    val phases  = Vector(Some(phasing.stage1), phasing.stage2).flatten
    val shaftR  = 0.3 * stages.map(s => s.module.value * s.set.sun.toDouble / 2.0).min
    val stacked = t.stacked && twoStage

    def stageOf(b: Body): Int = if b == Body.S1 || b == Body.C1 || b == Body.R1 then 1 else 2
    def stageFor(i: Int): Stage = if i == 1 then d.stage1 else d.stage2.get
    def planetRoot(s: Stage) = GearGeometry.gearRadii(s.set.planet.toInt, s.module.value, internal = false).root
    def pinR(s: Stage)       = 0.28 * planetRoot(s)
    def planetPos(s: Stage) = Vector.tabulate(s.set.numPlanets) { k =>
      val (x, y) = planetPosition(k, s.set.numPlanets, 0.0, s.centerToCenter)
      (x, -y)
    }
    // Carrier plates sit outside the stack: below stage 1 and above stage 2.
    def plateZ(i: Int): (Double, Double) =
      if i == 1 then (-clr - plateT, -clr) else (z2 + w + clr, z2 + w + clr + plateT)

    def roleTone(role: Option[BodyRole], free: Tone) = role match
      case Some(BodyRole.Input)  => Tone.Input
      case Some(BodyRole.Output) => Tone.Output
      case Some(BodyRole.Fixed)  => Tone.Fixed
      case _                     => free

    // Where a weld meets each body, along z.
    def attachZ(b: Body): Double = b match
      case Body.S1 | Body.S2 => zOf(stageOf(b) - 1) + w / 2.0
      case Body.R1           => z1 + w
      case Body.R2           => z2
      case Body.C1           => (plateZ(1)._1 + plateZ(1)._2) / 2.0
      case Body.C2           => (plateZ(2)._1 + plateZ(2)._2) / 2.0

    def bodyPrims(b: Body, role: Option[BodyRole]): Vector[Prim] =
      val i  = stageOf(b)
      val st = stageFor(i)
      val ph = phases(i - 1)
      val m  = st.module.value
      val z  = zOf(i - 1)
      b match
        case Body.S1 | Body.S2 =>
          val outline = flip(externalGearPoints(st.set.sun.toInt, m, ph.sun.fold(0.0)(toRad)))
          Vector(Extruded(outline, None, z, z + w, roleTone(role, if i == 1 then Tone.Sun1 else Tone.Sun2)))
        case Body.R1 | Body.R2 =>
          val outline = flip(internalGearPoints(st.set.ring.toInt, m, toRad(ph.ring)))
          Vector(Extruded(outline, Some(st.ringOuterDiameter / 2.0), z, z + w,
            roleTone(role, if i == 1 then Tone.Ring1 else Tone.Ring2)))
        case Body.C1 | Body.C2 =>
          val tone     = roleTone(role, Tone.Carrier)
          val (p0, p1) = plateZ(i)
          val pos      = planetPos(st)
          val armW     = 3.0 * pinR(st)
          // a stacked planet's pin runs the full height, so stage 2's carrier only adds its plate
          val pinSpan =
            if stacked then (if i == 1 then Some((p0, plateZ(2)._2)) else None)
            else if i == 1 then Some((p0, z1 + w))
            else Some((z2, p1))
          Vector(Cyl(0, 0, 0.8 * armW, 0.8 * armW, p0, p1, false, tone)) ++
            pos.map((x, y) => Bar(0, 0, x, y, armW, p0, p1, tone)) ++
            pinSpan.toVector.flatMap((a, c) => pos.map((x, y) => Cyl(x, y, pinR(st), pinR(st), a, c, false, tone)))

    def planetParts(carrier: Body, i: Int): Vector[PlanetPart] =
      val st = stageFor(i)
      planetPos(st).zipWithIndex.map { case ((x, y), k) =>
        val outline = flip(externalGearPoints(st.set.planet.toInt, st.module.value, toRad(phases(i - 1).planets(k))))
        PlanetPart(k, x, y, i, carrier, stacked = false,
          Vector(Extruded(outline, None, zOf(i - 1), zOf(i - 1) + w, if i == 1 then Tone.Planet1 else Tone.Planet2)))
      }

    def stackedParts(carrier: Body): Vector[PlanetPart] =
      val hubR = 0.5 * math.min(planetRoot(d.stage1), planetRoot(d.stage2.get))
      planetPos(d.stage1).zipWithIndex.map { case ((x, y), k) =>
        def half(i: Int) =
          val st = stageFor(i)
          Extruded(flip(externalGearPoints(st.set.planet.toInt, st.module.value, toRad(phases(i - 1).planets(k)))),
            None, zOf(i - 1), zOf(i - 1) + w, Tone.Stacked)
        PlanetPart(k, x, y, 1, carrier, stacked = true,
          Vector(half(1), Cyl(0, 0, hubR, hubR, z1 + w, z2, false, Tone.Stacked), half(2)))
      }

    /** Solids that make a coupling visible: a shaft, ring spokes or sleeve, carrier tie posts. */
    def connector(a: Body, b: Body): Vector[Prim] =
      def ringR(i: Int) = stageFor(i).ringOuterDiameter / 2.0
      (a, b) match
        case (Body.R1, Body.R2) =>
          Vector(Cyl(0, 0, ringR(2), ringR(1), z1 + w, z2, true, Tone.Weld))
        case (Body.C1, Body.C2) if stacked => Vector.empty
        case (Body.C1, Body.C2) =>
          val r = math.max(d.stage1.centerToCenter, d.stage2.get.centerToCenter)
          val n = d.stage1.set.numPlanets
          Vector.tabulate(n) { k =>
            val th = 2.0 * math.Pi * (k + 0.5) / n
            Cyl(r * math.sin(th), r * math.cos(th), pinR(d.stage1), pinR(d.stage1), plateZ(1)._1, plateZ(2)._2, false, Tone.Weld)
          }
        case _ =>
          def spokes(r: Body): Vector[Prim] =
            if r != Body.R1 && r != Body.R2 then Vector.empty
            else
              val (rad, za, zb) =
                if r == Body.R1 then (ringR(1), z1 + w, z1 + w + armT) else (ringR(2), z2 - armT, z2)
              Vector.tabulate(3) { k =>
                val th = 2.0 * math.Pi * k / 3
                Bar(0, 0, rad * math.cos(th), rad * math.sin(th), 2.0 * shaftR, za, zb, Tone.Weld)
              }
          val (za, zb) = (attachZ(a), attachZ(b))
          Vector(Cyl(0, 0, shaftR, shaftR, math.min(za, zb), math.max(za, zb), false, Tone.Weld)) ++ spokes(a) ++ spokes(b)

    val connectors: Vector[(Body, Vector[Prim])] =
      if !twoStage then Vector.empty
      else Vector(t.firstCoupling, t.secondCoupling).flatMap(Coupling.bodyPair)
        .filter((a, b) => t.presentBodies.contains(a) && t.presentBodies.contains(b))
        .map((a, b) => a -> connector(a, b))

    val clusters = clusterBodies(d).map { group =>
      val role = Vector(BodyRole.Input, BodyRole.Output, BodyRole.Fixed)
        .find(r => group.exists(b => t.roles.get(b).contains(r)))
      val planets = group.flatMap {
        case b @ Body.C1                 => if stacked then stackedParts(b) else planetParts(b, 1)
        case b @ Body.C2 if !stacked     => planetParts(b, 2)
        case _                           => Vector.empty
      }
      Cluster(group, group.head, role,
        group.flatMap(bodyPrims(_, role)) ++ connectors.collect { case (a, ps) if group.contains(a) => ps }.flatten,
        planets)
    }

    Layout(w, gap, z1, Option.when(twoStage)(z2), plateZ(1)._1, if twoStage then plateZ(2)._2 else z1 + w,
      outerR, clusters)

  /** Rotation about z of a cluster, in the schematic's clockwise-positive convention. */
  def clusterAngle(c: Cluster, a: BodyAngles): Double = a.of(c.angleBody)

  /** Rotation of a planet relative to its carrier. */
  def planetAngle(p: PlanetPart, a: BodyAngles): Double =
    (if p.stage == 1 then a.planet1Spin else a.planet2Spin) - a.of(p.carrier)
