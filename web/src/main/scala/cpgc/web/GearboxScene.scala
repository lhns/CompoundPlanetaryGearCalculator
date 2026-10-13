package cpgc.web

import cpgc.constraints.{Body, BodyRole, Coupling}
import cpgc.domain.{CompoundDesign, Stage}
import cpgc.kinematics.BodyAngles
import cpgc.render.Phasing
import cpgc.web.Palette.Tone
import cpgc.web.SchematicLayout.{externalGearPoints, internalGearPoints, planetPosition, toRad}

/** The one pure scene model of a design, consumed by both renderers (SVG schematic and 3D view).
  * It owns everything that is not drawing style: which gears exist, how many, where, their size and
  * tooth outline (with the tooth phase baked in), colour tone and role. Absent suns are not in it.
  *
  * Millimetres, schematic convention (x right, y down), each stage about its own axis; planet
  * centres are given at rest, in the carrier's frame. Renderers add only their own presentation
  * (the schematic scales and spaces the stages, the 3D view adds depth and weld solids). Angles
  * per phase come from `Kinematics` via `angle` / `carrierAngle`. */
object GearboxScene:

  enum Kind:
    case Sun, Planet, Ring, Carrier

  /** One gear. `outline` is about the gear's own centre; for a ring it is the tooth side (a hole in a
    * disc of radius `outerRadius`). A planet of a stacked design has `stackIndex` = planet number, shared
    * by its stage-1 and stage-2 gears (one physical part). */
  final case class Gear(
      id: String,
      stage: Int,
      kind: Kind,
      body: Option[Body], // None for planets
      index: Int,         // planet number, 0 otherwise
      centre: (Double, Double),
      teeth: Int,
      module: Double,
      pitchR: Double,
      outline: Vector[(Double, Double)],
      outerRadius: Option[Double],
      tone: Tone,
      role: Option[BodyRole],
      coupling: Option[Int],
      stackIndex: Option[Int]
  )

  /** A carrier: one arm per planet, from the axis to the planet centre, at radius `radius`. */
  final case class Carrier(
      stage: Int, body: Body, radius: Double, arms: Vector[(Double, Double)],
      tone: Tone, role: Option[BodyRole], coupling: Option[Int]
  )

  final case class StageScene(
      index: Int, stage: Stage, sun: Option[Gear], ring: Gear, planets: Vector[Gear], carrier: Carrier
  ):
    def gears: Vector[Gear] = sun.toVector ++ Vector(ring) ++ planets

  final case class Scene(
      design: CompoundDesign,
      stages: Vector[StageScene],
      clusters: Vector[Vector[Body]],        // bodies welded into one rigid piece
      couplings: Vector[(Int, Coupling)],    // (slot, coupling) of a compound
      marks: SchematicLayout.CouplingMarks,
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

    def slotTone(slot: Int): Tone = if slot == 1 then Tone.Cpl1 else Tone.Cpl2
    // pinned role first, then the coupling, then the stage tint (the schematic's rule, per body)
    def toneOf(role: Option[BodyRole], slot: Option[Int], tint: Tone): Tone = role match
      case Some(BodyRole.Input)  => Tone.Input
      case Some(BodyRole.Output) => Tone.Output
      case Some(BodyRole.Fixed)  => Tone.Fixed
      case _                     => slot.fold(tint)(slotTone)

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
        Gear(s"S$i", i, Kind.Sun, Some(sunB), 0, (0.0, 0.0), teeth, m, m * teeth / 2.0,
          externalGearPoints(teeth, m, ph.sun.fold(0.0)(toRad)), None,
          toneOf(role(sunB), slot(sunB), sunTint), role(sunB), slot(sunB), None)
      }
      val ring =
        val teeth = st.set.ring.toInt
        Gear(s"R$i", i, Kind.Ring, Some(ringB), 0, (0.0, 0.0), teeth, m, m * teeth / 2.0,
          internalGearPoints(teeth, m, toRad(ph.ring)), Some(st.ringOuterDiameter / 2.0),
          toneOf(role(ringB), slot(ringB), ringTint), role(ringB), slot(ringB), None)
      val centres = Vector.tabulate(n)(k => planetPosition(k, n, 0.0, st.centerToCenter))
      val planets = centres.zipWithIndex.map { case (c, k) =>
        val teeth = st.set.planet.toInt
        Gear(s"P$i.$k", i, Kind.Planet, None, k, c, teeth, m, m * teeth / 2.0,
          externalGearPoints(teeth, m, toRad(ph.planets(k))), None,
          toneOf(None, marks.planets, planetTint), None, marks.planets, Option.when(stacked)(k))
      }
      val carrier = Carrier(i, carB, st.centerToCenter, centres, toneOf(role(carB), slot(carB), Tone.Carrier), role(carB), slot(carB))
      StageScene(i, st, sun, ring, planets, carrier)
    }

    val slots = if d.stage2.isDefined then Vector(1 -> t.firstCoupling, 2 -> t.secondCoupling) else Vector.empty
    Scene(d, stages, clusterBodies(d), slots, marks, stacked)

  // ── Poses (per frame; the scene itself is static) ──────────────────────────

  /** Carrier angle of a stage. */
  def carrierAngle(s: StageScene, a: BodyAngles): Double = a.of(s.carrier.body)

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
