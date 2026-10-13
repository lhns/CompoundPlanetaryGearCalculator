package cpgc.web

import cpgc.constraints.{Body, BodyRole, Coupling}
import cpgc.domain.{CompoundDesign, Stage}
import cpgc.kinematics.BodyAngles
import cpgc.render.Phasing
import cpgc.web.Palette.Tone
import cpgc.web.SchematicLayout.{externalGearPoints, internalGearPoints}

/** The one pure scene model of a design, consumed by both renderers (SVG schematic and 3D view).
  * It owns everything that is not drawing style: which gears exist, how many, where, their size and
  * tooth outline (with the tooth phase baked in), colour tone and role. Absent suns are not in it.
  *
  * Units and axes: lengths in millimetres, x right, y down (screen convention), each stage about its
  * own axis; angles in radians, clockwise-positive on screen. Planet centres are given at rest, in
  * the carrier's frame. Renderers convert at one point each: the schematic scales mm to drawing units
  * (`StageLayout.scale`), the 3D view flips y and negates angles (`Scene3D`, `ThreeScene`). Angles per
  * phase come from `Kinematics` via `angle` / `carrierAngle` / `clusterAngle`. */
object GearboxScene:

  enum Kind:
    case Sun, Planet, Ring, Carrier

  /** One gear. `outline` is about the gear's own centre; for a ring it is the tooth side (a hole in a
    * disc of radius `outerRadius`). Planet k of stage 1 and of stage 2 are one physical part when the
    * scene is `stacked`. */
  final case class Gear(
      id: String,
      stage: Int,
      kind: Kind,
      body: Option[Body], // None for planets
      centre: (Double, Double),
      teeth: Int,
      module: Double,
      pitchR: Double,
      outline: Vector[(Double, Double)],
      outerRadius: Option[Double],
      tone: Tone,
      role: Option[BodyRole],
      coupling: Option[Int]
  )

  /** A carrier: one arm per planet, from the axis to the planet centre, at radius `radius`. */
  final case class Carrier(
      body: Body, radius: Double, arms: Vector[(Double, Double)],
      tone: Tone, role: Option[BodyRole], coupling: Option[Int]
  )

  final case class StageScene(
      stage: Stage, sun: Option[Gear], ring: Gear, planets: Vector[Gear], carrier: Carrier
  ):
    def gears: Vector[Gear] = sun.toVector ++ Vector(ring) ++ planets

  final case class Scene(
      design: CompoundDesign,
      stages: Vector[StageScene],
      clusters: Vector[Vector[Body]],        // bodies welded into one rigid piece
      couplings: Vector[(Int, Coupling)],    // (slot, coupling) of a compound
      stacked: Boolean
  ):
    def gears: Vector[Gear] = stages.flatMap(_.gears)


  /** Weld groups: bodies joined by a central-shaft coupling share a cluster. */
  def clusterBodies(d: CompoundDesign): Vector[Vector[Body]] =
    val t       = d.topology
    val present = Body.values.toVector.filter(t.presentBodies.contains)
    val welds = Vector(t.firstCoupling, t.secondCoupling).filter(_ => t.stages == 2)
      .flatMap(Coupling.bodyPair).filter((a, b) => t.presentBodies.contains(a) && t.presentBodies.contains(b))
    val merged = welds.foldLeft(present.map(Set(_))) { case (groups, (a, b)) =>
      val (hit, rest) = groups.partition(g => g.contains(a) || g.contains(b))
      rest :+ hit.flatten.toSet
    }
    merged.map(g => present.filter(g.contains)).sortBy(g => Body.values.indexOf(g.head))

  def of(d: CompoundDesign): Scene =
    val t       = d.topology
    val marks   = SchematicLayout.couplingMarks(t)
    val phasing = Phasing.solve(d)
    val stacked = t.stacked && d.stage2.isDefined

    // pinned role first, then the coupling, then the stage tint (the schematic's rule, per body)
    def toneOf(role: Option[BodyRole], slot: Option[Int], tint: Tone): Tone = role match
      case Some(BodyRole.Input)  => Tone.Input
      case Some(BodyRole.Output) => Tone.Output
      case Some(BodyRole.Fixed)  => Tone.Fixed
      case _                     => slot.fold(tint)(Palette.slotTone)

    val stageList = Vector((d.stage1, phasing.stage1)) ++ d.stage2.zip(phasing.stage2)
    val stages = stageList.zipWithIndex.map { case ((st, ph), idx) =>
      val i      = idx + 1
      val m      = st.module.value
      val n      = st.set.numPlanets
      val sunB   = if i == 1 then Body.S1 else Body.S2
      val carB   = if i == 1 then Body.C1 else Body.C2
      val ringB  = if i == 1 then Body.R1 else Body.R2
      def role(b: Body) = t.roles.get(b)
      def slot(b: Body) = marks.bodies.get(b)
      val sunTint    = if i == 1 then Tone.Sun1 else Tone.Sun2
      val ringTint   = if i == 1 then Tone.Ring1 else Tone.Ring2
      val planetTint = if i == 1 then Tone.Planet1 else Tone.Planet2

      val sun = Option.when(t.sunPresent(i)) {
        val teeth = st.set.sun.toInt
        Gear(s"S$i", i, Kind.Sun, Some(sunB), (0.0, 0.0), teeth, m, m * teeth / 2.0,
          externalGearPoints(teeth, m, ph.sun.fold(0.0)(toRad)), None,
          toneOf(role(sunB), slot(sunB), sunTint), role(sunB), slot(sunB))
      }
      val ring =
        val teeth = st.set.ring.toInt
        Gear(s"R$i", i, Kind.Ring, Some(ringB), (0.0, 0.0), teeth, m, m * teeth / 2.0,
          internalGearPoints(teeth, m, toRad(ph.ring)), Some(st.ringOuterDiameter / 2.0),
          toneOf(role(ringB), slot(ringB), ringTint), role(ringB), slot(ringB))
      val centres = Vector.tabulate(n)(k => planetPosition(k, n, st.centerToCenter))
      val planets = centres.zipWithIndex.map { case (c, k) =>
        val teeth = st.set.planet.toInt
        Gear(s"P$i.$k", i, Kind.Planet, None, c, teeth, m, m * teeth / 2.0,
          externalGearPoints(teeth, m, toRad(ph.planets(k))), None,
          toneOf(None, marks.planets, planetTint), None, marks.planets)
      }
      val carrier = Carrier(carB, st.centerToCenter, centres, toneOf(role(carB), slot(carB), Tone.Carrier), role(carB), slot(carB))
      StageScene(st, sun, ring, planets, carrier)
    }

    val slots = if d.stage2.isDefined then Vector(1 -> t.firstCoupling, 2 -> t.secondCoupling) else Vector.empty
    Scene(d, stages, clusterBodies(d), slots, stacked)

  /** Position of planet `i` of `n` on a carrier of radius `carrierR` about the axis: planet 0 at the
    * top, then i·360/N clockwise on screen. */
  private def planetPosition(i: Int, n: Int, carrierR: Double): (Double, Double) =
    val theta = 2.0 * math.Pi * i / n - math.Pi / 2.0
    (carrierR * math.cos(theta), carrierR * math.sin(theta))

  /** Phasing turns → drawn angle: planet i sits at 2πi/N − π/2, so the phasing frame
    * (planet k at k/N turns) is rotated by a quarter turn; mesh conditions are invariant under that shift. */
  private def toRad(turns: Double): Double = 2.0 * math.Pi * (turns - 0.25)

  // ── Poses (per frame; the scene itself is static) ──────────────────────────

  /** Carrier angle of a stage. */
  def carrierAngle(s: StageScene, a: BodyAngles): Double = a.of(s.carrier.body)

  /** Angle of a weld cluster: that of its first body (all of them turn as one). */
  def clusterAngle(bodies: Vector[Body], a: BodyAngles): Double = a.of(bodies.head)

  /** Rotation of a gear about its own centre, in the schematic's clockwise-positive convention:
    * sun / ring turn with their body, a planet relative to its carrier. */
  def angle(g: Gear, a: BodyAngles): Double = g.body match
    case Some(b) => a.of(b)
    case None =>
      val (spin, carrier) = if g.stage == 1 then (a.planet1Spin, a.carrier1) else (a.planet2Spin, a.carrier2)
      spin - carrier

  /** Where a planet's centre is after its carrier turned by `carrierRad`, from the rest position. */
  def planetCentreAt(g: Gear, carrierRad: Double): (Double, Double) =
    val (x, y) = g.centre
    (x * math.cos(carrierRad) - y * math.sin(carrierRad), x * math.sin(carrierRad) + y * math.cos(carrierRad))


  def stageOfBody(b: Body): Int = if Set(Body.S1, Body.C1, Body.R1).contains(b) then 1 else 2
