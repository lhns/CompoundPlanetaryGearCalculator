package cpgc.web

import cpgc.constraints.{Body, Topology}
import cpgc.domain.{CompoundDesign, GearSet, Module, Ratio, Stage, Teeth}
import cpgc.kinematics.Kinematics
import cpgc.search.Scorer.SortBy
import cpgc.web.GearboxScene.Kind
import cpgc.web.Palette.Tone
import cpgc.web.Scene3D.*
import cpgc.web.three.OrbitState
import munit.FunSuite

/** DOM-free checks of the 3D depth layer (on top of the shared scene), orbit maths and the view URL. */
class Scene3DSpec extends FunSuite:

  private def stage(sun: Int, planet: Int, n: Int, m: Double = 2.0) =
    Stage(GearSet(Teeth(sun), Teeth(planet), n), Module(m))

  private def design(t: Topology, s1: Stage = stage(25, 11, 3), s2: Stage = stage(26, 10, 3, 2.5)) =
    CompoundDesign(s1, Some(s2), t, Ratio(5.0), 0.9)

  private def lay(d: CompoundDesign) = Scene3D.build(GearboxScene.of(d))

  private val eps = 1e-6

  private def allPrims(l: Layout): Vector[Prim] = l.clusters.flatMap(c => c.prims ++ c.connectors ++ c.planets.flatMap(_.prims))
  private def extruded(l: Layout) = allPrims(l).collect { case e: Extruded => e }

  test("stage 2 sits above stage 1 with a gap; gears of each stage stay in their band"):
    val l = lay(design(Topology.ravigneaux))
    val z2 = l.stage2Z.get
    assertEqualsDouble(z2, l.faceWidth * (1.0 + Scene3D.StageGapFactor), eps)
    for e <- extruded(l) do
      if e.gear.stage == 1 then assert(e.z0 >= -eps && e.z1 <= l.faceWidth + eps, e.gear.id)
      else assert(e.z0 >= z2 - eps && e.z1 <= z2 + l.faceWidth + eps, e.gear.id)
    assert(l.zMin < 0.0 && l.zMax > z2 + l.faceWidth)

  test("face width is 0.12 x the larger ring outer diameter"):
    val s1 = stage(25, 11, 3, 2.0)
    val s2 = stage(26, 10, 3, 3.0)
    val d  = design(Topology.ravigneaux, s1, s2)
    val od = math.max(s1.ringOuterDiameter, s2.ringOuterDiameter)
    assertEqualsDouble(lay(d).faceWidth, Scene3D.FaceWidthFactor * od, eps)
    assertEqualsDouble(Scene3D.FaceWidthFactor, 0.12, 0.0)
    assertEqualsDouble(lay(d).outerRadius, od / 2.0, eps)

  test("a stacked planet is one part spanning both stages"):
    val l = lay(design(Topology.standardCompound))
    val planets = l.clusters.flatMap(_.planets)
    assertEquals(planets.size, 3) // N planets, not 2N
    assert(planets.forall(_.stacked))
    for p <- planets do
      assertEqualsDouble(p.prims.map(_.z0).min, 0.0, eps)
      assertEqualsDouble(p.prims.map(_.z1).max, l.stage2Z.get + l.faceWidth, eps)
      assertEquals(p.prims.map(_.tone).toSet, Set(Tone.Cpl2)) // PlanetStack is the second coupling

  test("without PlanetStack each stage has its own planets"):
    val l = lay(design(Topology.ravigneaux, stage(25, 11, 3), stage(26, 10, 4, 2.5)))
    val planets = l.clusters.flatMap(_.planets)
    assertEquals(planets.count(_.gear.stage == 1), 3)
    assertEquals(planets.count(_.gear.stage == 2), 4)
    assert(planets.forall(!_.stacked))

  test("absent suns are not extruded"):
    assert(extruded(lay(design(Topology.sunlessCdcpCompound))).forall(_.gear.kind != Kind.Sun))
    val nss = extruded(lay(design(Topology.nssCompound)))
    assert(nss.exists(_.gear.body.contains(Body.S1)) && !nss.exists(_.gear.body.contains(Body.S2)))

  test("carrier arms: one bar per planet on each carrier plate"):
    for (_, d) <- (new GearboxSceneSpec).presets do
      val l    = lay(d)
      val bars = l.clusters.flatMap(_.prims).collect { case b: Bar => b }
      val sc   = GearboxScene.of(d)
      assertEquals(bars.count(_.z1 <= 0.0), sc.stages(0).planets.size)
      sc.stages.lift(1).foreach(s2 => assertEquals(bars.count(_.z0 >= l.stage2Z.get + l.faceWidth), s2.planets.size))

  private def welds(l: Layout, body: Body) = l.clusters.find(_.bodies.contains(body)).get.connectors

  test("Ring1-Sun2 weld: spokes on top of ring 1 and a shaft up to sun 2"):
    val l = lay(design(Topology.ringToSunCascade))
    val w = welds(l, Body.R1)
    val shaft = w.collect { case c: Cyl if c.x == 0.0 && c.y == 0.0 => c }
    assertEquals(shaft.size, 1)
    assertEqualsDouble(shaft.head.z0, l.faceWidth, eps)                       // top of ring 1
    assertEqualsDouble(shaft.head.z1, l.stage2Z.get + l.faceWidth / 2.0, eps) // inside sun 2
    val bars = w.collect { case b: Bar => b }
    assertEquals(bars.size, 3)
    assert(bars.forall(b => b.z0 >= l.faceWidth - eps && b.z1 <= l.stage2Z.get + eps))
    assert(l.clusters.find(_.bodies.contains(Body.S2)).exists(_.bodies.contains(Body.R1)))
    assert(w.forall(_.tone == Tone.Cpl2)) // coupling 2

  test("Carrier1-Sun2 weld: a shaft from carrier 1's plate to sun 2"):
    val l = lay(design(Topology.ravigneaux))
    val shaft = welds(l, Body.C1).collect { case c: Cyl => c }.head
    assert(shaft.z0 < 0.0) // below stage 1: carrier plate
    assertEqualsDouble(shaft.z1, l.stage2Z.get + l.faceWidth / 2.0, eps)
    assertEquals(shaft.tone, Tone.Cpl1)

  test("Ring1-Ring2 weld is a sleeve across the gap"):
    val l = lay(design(Topology.ravigneaux))
    val sleeve = welds(l, Body.R1).collect { case c: Cyl if c.openEnded => c }.head
    assertEqualsDouble(sleeve.z0, l.faceWidth, eps)
    assertEqualsDouble(sleeve.z1, l.stage2Z.get, eps)

  test("a single stage has no weld"):
    val l = lay(CompoundDesign(stage(25, 11, 3), None, Topology.singleStage, Ratio(3.0), 0.9))
    assert(l.stage2Z.isEmpty && l.clusters.forall(_.connectors.isEmpty))
    assertEquals(l.clusters.size, 3)

  test("the 3D adapter builds exactly the scene's gears: id, centre, radius, teeth, tone and angle"):
    for (name, d) <- (new GearboxSceneSpec).presets do
      val sc = GearboxScene.of(d)
      val l  = Scene3D.build(sc)
      val a  = Kinematics.at(d, 0.9)
      val built = (l.clusters.flatMap(_.prims) ++ l.clusters.flatMap(_.planets.flatMap(_.prims))).collect { case e: Extruded => e.gear }
      assertEquals(built.map(_.id).sorted, sc.gears.map(_.id).sorted, name)
      for g <- built do assertEquals(g, sc.gears.find(_.id == g.id).get, s"$name ${g.id}") // the very same scene data
      for c <- l.clusters; p <- c.planets do
        // a planet group sits at the scene centre, y mirrored (scene y is down, 3D y is up)
        assertEquals((p.x, p.y), (p.gear.centre._1, -p.gear.centre._2), name)
      for c <- l.clusters; b <- c.bodies do // welded bodies turn as one
        assertEqualsDouble(GearboxScene.clusterAngle(c.bodies, a), a.of(b), 1e-9, s"$name $b")
      assertEquals(l.clusters.flatMap(_.bodies).toSet, d.topology.presentBodies, name)

  // ── orbit maths ──────────────────────────────────────────────────────────

  private def orbit() = new OrbitState(OrbitState.Home((0.0, 0.0, 10.0), 100.0))

  test("orbit: starts above the model on the -y side, pitch is clamped"):
    val o = orbit()
    val (x, y, z) = o.position
    assertEqualsDouble(x, 0.0, 1e-9)
    assert(y < 0.0 && z > 10.0)
    o.orbit(0, 1e6)
    assertEqualsDouble(o.pitch, OrbitState.MaxPitch, 1e-12)
    o.orbit(0, -1e7)
    assertEqualsDouble(o.pitch, OrbitState.MinPitch, 1e-12)

  test("orbit: horizontal drag changes yaw only, the distance stays"):
    val o = orbit()
    val (p0, d0) = (o.pitch, o.distance)
    o.orbit(100, 0)
    assertEqualsDouble(o.pitch, p0, 1e-12)
    assertEqualsDouble(o.distance, d0, 1e-12)
    assert(o.yaw != OrbitState.DefaultYaw)

  test("orbit: zoom is clamped; reset restores the home view"):
    val o = orbit()
    o.zoom(1e6)
    assertEqualsDouble(o.distance, 100.0 * OrbitState.MaxZoom, 1e-9)
    o.zoom(-1e6)
    assertEqualsDouble(o.distance, 100.0 * OrbitState.MinZoom, 1e-9)
    o.orbit(50, 50)
    o.pan(20, 20, 400, 0.6)
    o.reset()
    assertEquals((o.yaw, o.pitch, o.distance, o.target), (OrbitState.DefaultYaw, OrbitState.DefaultPitch, 100.0, (0.0, 0.0, 10.0)))

  test("orbit: dragging right pans the target left, dragging down pans it up"):
    val o = orbit()
    o.pan(100, 0, 400, 0.6)
    assert(o.target._1 < 0.0)
    val o2 = orbit()
    o2.pan(0, 100, 400, 0.6)
    assert(o2.target._3 > 10.0)

  test("orbit: rehome keeps angles, zoom ratio and pan offset"):
    val o = orbit()
    o.orbit(30, 20)
    o.zoomBy(0.5)
    o.pan(10, 0, 400, 0.6)
    val (yaw, pitch) = (o.yaw, o.pitch)
    val tx = o.target._1
    o.rehome(OrbitState.Home((0.0, 0.0, 50.0), 200.0))
    assertEqualsDouble(o.yaw, yaw, 1e-12)
    assertEqualsDouble(o.pitch, pitch, 1e-12)
    assertEqualsDouble(o.distance, 100.0, 1e-9)
    assertEqualsDouble(o.target._1, tx, 1e-12)

  // ── URL ──────────────────────────────────────────────────────────────────

  test("view round-trips through the URL; the schematic is the default and is omitted"):
    val f = UrlState.initialForm("")
    for sort <- SortBy.values.toSeq; v <- View.values.toSeq do
      val q = UrlState.toQuery(f, sort, v)
      assertEquals(UrlState.initialView(q), v, q)
      assertEquals(UrlState.initialForm(q), f)
      assertEquals(UrlState.initialSort(q), sort)
    assert(UrlState.toQuery(f, SortBy.Ratio, View.ThreeD).contains("view=3d"))
    assert(!UrlState.toQuery(f, SortBy.Ratio).contains("view"))
    assertEquals(UrlState.initialView("?view=3D"), View.ThreeD)
    assertEquals(UrlState.initialView("?view=bogus"), View.Schematic)
    assertEquals(UrlState.initialView(""), View.Schematic)
