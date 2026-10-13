package cpgc.web

import cpgc.constraints.{Body, Topology}
import cpgc.domain.{CompoundDesign, GearSet, Module, Ratio, Stage, Teeth}
import cpgc.kinematics.{BodyAngles, Kinematics}
import cpgc.search.Scorer.SortBy
import cpgc.web.Layout3D.*
import cpgc.web.three.OrbitState
import munit.FunSuite

// DOM-free checks of the 3D layout and orbit maths.
class Layout3DSpec extends FunSuite:

  private def stage(sun: Int, planet: Int, n: Int, m: Double = 2.0) =
    Stage(GearSet(Teeth(sun), Teeth(planet), n), Module(m))

  private def design(t: Topology, s1: Stage = stage(25, 11, 3), s2: Stage = stage(26, 10, 3, 2.5)) =
    CompoundDesign(s1, Some(s2), t, Ratio(5.0), 0.9)

  private val eps = 1e-6

  private def tones(l: Layout) = l.allPrims.map(_.tone).toSet

  test("stage 2 sits above stage 1 with a gap; gears of each stage stay in their band"):
    val l = build(design(Topology.ravigneaux))
    assertEquals(l.stage1Z, 0.0)
    val z2 = l.stage2Z.get
    assertEqualsDouble(z2, l.faceWidth + l.stageGap, eps)
    assert(l.stageGap > 0.0)
    for p <- l.allPrims do p.tone match
      case Tone.Sun1 | Tone.Ring1 | Tone.Planet1 => assert(p.z0 >= -eps && p.z1 <= l.faceWidth + eps, p.toString)
      case Tone.Sun2 | Tone.Ring2 | Tone.Planet2 => assert(p.z0 >= z2 - eps && p.z1 <= z2 + l.faceWidth + eps, p.toString)
      case _ => ()
    assert(l.zMin < 0.0 && l.zMax > z2 + l.faceWidth)

  test("face width is 0.12 x the larger ring outer diameter"):
    val s1 = stage(25, 11, 3, 2.0)
    val s2 = stage(26, 10, 3, 3.0)
    val d  = design(Topology.ravigneaux, s1, s2)
    val od = math.max(s1.ringOuterDiameter, s2.ringOuterDiameter)
    assertEqualsDouble(build(d).faceWidth, Layout3D.FaceWidthFactor * od, eps)
    assertEqualsDouble(Layout3D.FaceWidthFactor, 0.12, 0.0)
    assertEqualsDouble(build(d).outerRadius, od / 2.0, eps)

  test("planets sit at k*360/N clockwise from the top on the carrier circle"):
    for n <- 2 to 6 do
      val s = stage(25, 11, n)
      val l = build(CompoundDesign(s, None, Topology.singleStage, Ratio(3.0), 0.9))
      val planets = l.clusters.flatMap(_.planets)
      assertEquals(planets.size, n)
      planets.foreach { p =>
        assertEqualsDouble(math.hypot(p.x, p.y), s.centerToCenter, 1e-9)
        val phi = math.toRadians(p.index * 360.0 / n)
        assertEqualsDouble(p.x, s.centerToCenter * math.sin(phi), 1e-9)
        assertEqualsDouble(p.y, s.centerToCenter * math.cos(phi), 1e-9)
      }

  test("a stacked planet is one part spanning both stages"):
    val l = build(design(Topology.standardCompound))
    val planets = l.clusters.flatMap(_.planets)
    assertEquals(planets.size, 3) // N planets, not 2N
    assert(planets.forall(_.stacked))
    for p <- planets do
      assertEqualsDouble(p.prims.map(_.z0).min, 0.0, eps)
      assertEqualsDouble(p.prims.map(_.z1).max, l.stage2Z.get + l.faceWidth, eps)
    assert(!tones(l).contains(Tone.Planet1) && !tones(l).contains(Tone.Planet2))
    assert(planets.flatMap(_.prims).forall(_.tone == Tone.Cpl2)) // PlanetStack is the second coupling

  test("without PlanetStack each stage has its own planets"):
    val l = build(design(Topology.ravigneaux))
    val planets = l.clusters.flatMap(_.planets)
    assertEquals(planets.count(_.stage == 1), 3)
    assertEquals(planets.count(_.stage == 2), 3)
    assert(planets.forall(!_.stacked))

  test("absent suns are not drawn"):
    val sunless = build(design(Topology.sunlessCdcpCompound))
    assert(!tones(sunless).exists(t => t == Tone.Sun1 || t == Tone.Sun2))
    val nss = build(design(Topology.nssCompound))
    assert(tones(nss).contains(Tone.Input)) // S1 is the input body and is drawn
    assert(!nss.clusters.flatMap(_.bodies).contains(Body.S2))
    assert(!tones(nss).contains(Tone.Sun2))
    val std = build(design(Topology.standardCompound))
    assertEquals(std.clusters.flatMap(_.bodies).toSet, Body.values.toSet)

  test("welded bodies share a cluster"):
    assertEquals(clusterBodies(design(Topology.standardCompound)).find(_.contains(Body.C1)), Some(Vector(Body.C1, Body.C2)))
    val rs = clusterBodies(design(Topology.ringToSunCascade))
    assert(rs.contains(Vector(Body.R1, Body.S2)))
    assert(rs.contains(Vector(Body.C1, Body.C2)))
    val rav = clusterBodies(design(Topology.ravigneaux))
    assert(rav.contains(Vector(Body.C1, Body.S2)) && rav.contains(Vector(Body.R1, Body.R2)))

  private def welds(l: Layout, body: Body) =
    l.clusters.find(_.bodies.contains(body)).get.connectors

  test("Ring1-Sun2 weld: spokes on top of ring 1 and a shaft up to sun 2"):
    val l = build(design(Topology.ringToSunCascade))
    val w = welds(l, Body.R1)
    val shaft = w.collect { case c: Cyl if c.x == 0.0 && c.y == 0.0 => c }
    assertEquals(shaft.size, 1)
    assertEqualsDouble(shaft.head.z0, l.faceWidth, eps)                       // top of ring 1
    assertEqualsDouble(shaft.head.z1, l.stage2Z.get + l.faceWidth / 2.0, eps) // inside sun 2
    val bars = w.collect { case b: Bar => b }
    assertEquals(bars.size, 3)
    assert(bars.forall(b => b.z0 >= l.faceWidth - eps && b.z1 <= l.stage2Z.get + eps))
    assert(l.clusters.find(_.bodies.contains(Body.S2)).exists(_.bodies.contains(Body.R1)))

  test("Carrier1-Sun2 weld: a shaft from carrier 1's plate to sun 2"):
    val l = build(design(Topology.ravigneaux))
    val shaft = welds(l, Body.C1).collect { case c: Cyl => c }.head
    assert(shaft.z0 < 0.0) // below stage 1: carrier plate
    assertEqualsDouble(shaft.z1, l.stage2Z.get + l.faceWidth / 2.0, eps)

  test("Ring1-Ring2 weld is a sleeve across the gap"):
    val l = build(design(Topology.ravigneaux))
    val sleeve = welds(l, Body.R1).collect { case c: Cyl if c.openEnded => c }.head
    assertEqualsDouble(sleeve.z0, l.faceWidth, eps)
    assertEqualsDouble(sleeve.z1, l.stage2Z.get, eps)

  test("coupled bodies and their connectors take the coupling's colour, pinned roles win"):
    val l = build(design(Topology.ringToSunCascade)) // coupling 1 = CarrierToCarrier, 2 = RingToSun
    val rs = l.clusters.find(_.bodies.contains(Body.R1)).get
    assert(rs.connectors.forall(_.tone == Tone.Cpl2))
    assertEquals(rs.prims.map(_.tone).toSet, Set(Tone.Fixed, Tone.Cpl2)) // R1 is fixed (role wins), S2 is coupled
    assert(l.clusters.find(_.bodies.contains(Body.C1)).get.prims.forall(_.tone == Tone.Cpl1))
    assertEquals(l.couplings.map(_._1), Vector(1, 2))

  test("a single stage has no weld"):
    val l = build(CompoundDesign(stage(25, 11, 3), None, Topology.singleStage, Ratio(3.0), 0.9))
    assert(l.couplings.isEmpty && l.stage2Z.isEmpty && l.clusters.forall(_.connectors.isEmpty))
    assertEquals(l.clusters.size, 3)

  test("role tones: pinned clusters take their role, the rest their stage tint"):
    val l = build(design(Topology.standardCompound))
    assertEquals(l.clusters.find(_.bodies.contains(Body.S1)).get.prims.head.tone, Tone.Input)
    assertEquals(l.clusters.find(_.bodies.contains(Body.R1)).get.prims.head.tone, Tone.Fixed)
    assertEquals(l.clusters.find(_.bodies.contains(Body.R2)).get.prims.head.tone, Tone.Output)
    assertEquals(l.clusters.find(_.bodies.contains(Body.S2)).get.prims.head.tone, Tone.Sun2)

  test("the fixed body never turns; planets turn relative to their carrier"):
    for t <- Seq(Topology.standardCompound, Topology.ravigneaux, Topology.nssCompound) do
      val d = design(t)
      val l = build(d)
      val a: BodyAngles = Kinematics.at(d, 1.234)
      val fixed = l.clusters.find(_.bodies.contains(t.fixed)).get
      assertEqualsDouble(clusterAngle(fixed, a), 0.0, 1e-9)
      for p <- l.clusters.flatMap(_.planets) do
        val spin = if p.stage == 1 then a.planet1Spin else a.planet2Spin
        assertEqualsDouble(planetAngle(p, a), spin - a.of(p.carrier), 1e-12)

  // ── every preset, checked numerically against the design and the 2D rules ──

  private val presetDesigns: Vector[(String, CompoundDesign)] =
    def two(t: Topology, s1: Stage, s2: Stage) = CompoundDesign(s1, Some(s2), t, Ratio(5.0), 0.9)
    val stackedA = stage(19, 17, 3, 2.0)  // S19 P17 R53
    val stackedB = stage(19, 14, 3, 2.5)  // S19 P14 R47
    val rav1     = stage(10, 17, 3, 2.0)  // S10 P17 R44
    val rav2     = stage(10, 17, 4, 2.5)  // different N: possible when the planets are not stacked
    Vector(
      "standard"  -> two(Topology.standardCompound, stackedA, stackedB),
      "cdcp"      -> two(Topology.cdcpCompound, stackedA, stackedB),
      "nss"       -> two(Topology.nssCompound, stackedA, stackedB),
      "sunless"   -> two(Topology.sunlessCdcpCompound, stackedA, stackedB),
      "ringToSun" -> two(Topology.ringToSunCascade, rav1, rav2),
      "ravigneaux" -> two(Topology.ravigneaux, rav1, rav2),
      "ravigneaux N=3" -> two(Topology.ravigneaux, rav1, rav1.copy(module = Module(2.5))),
      "simpson"   -> two(Topology.simpson, rav1, rav2),
      "single"    -> CompoundDesign(stackedA, None, Topology.singleStage, Ratio(3.0), 0.9)
    )

  /** The schematic's rule for a body's colour: pinned role, else its coupling, else its stage tint. */
  private def expectedTone(d: CompoundDesign, b: Body): Tone =
    import cpgc.constraints.BodyRole
    val i = if Set(Body.S1, Body.C1, Body.R1).contains(b) then 1 else 2
    d.topology.roles.get(b) match
      case Some(BodyRole.Input)  => Tone.Input
      case Some(BodyRole.Output) => Tone.Output
      case Some(BodyRole.Fixed)  => Tone.Fixed
      case _ =>
        SchematicLayout.couplingMarks(d.topology).bodies.get(b) match
          case Some(1) => Tone.Cpl1
          case Some(_) => Tone.Cpl2
          case None =>
            b match
              case Body.S1 | Body.S2 => if i == 1 then Tone.Sun1 else Tone.Sun2
              case Body.R1 | Body.R2 => if i == 1 then Tone.Ring1 else Tone.Ring2
              case _                 => Tone.Carrier

  test("every preset: planets, carrier arms, gears, ring holes and colours match the design"):
    for (name, d) <- presetDesigns do
      val l       = build(d)
      val stages  = Vector(d.stage1) ++ d.stage2
      val planets = l.clusters.flatMap(_.planets)
      val bars    = l.clusters.flatMap(_.prims).collect { case b: Bar => b }
      val ext     = l.allPrims.collect { case e: Extruded => e }
      val stacked = d.topology.stacked && d.stage2.isDefined
      for (st, idx) <- stages.zipWithIndex do
        val i = idx + 1
        val n = st.set.numPlanets
        val mine = if stacked then planets else planets.filter(_.stage == i)
        assertEquals(mine.size, n, s"$name stage $i planets")
        val arms = if i == 1 then bars.count(_.z1 <= 0.0) else bars.count(_.z0 >= l.stage2Z.get + l.faceWidth)
        assertEquals(arms, n, s"$name stage $i carrier arms")
        val m = st.module.value
        // gears of this stage, by body
        for b <- Body.values if d.topology.presentBodies.contains(b) && (if Set(Body.S1, Body.C1, Body.R1).contains(b) then 1 else 2) == i do
          b match
            case Body.S1 | Body.S2 =>
              val g = ext.find(_.body.contains(b)).getOrElse(fail(s"$name: no sun for $b"))
              assertEquals(g.teeth, st.set.sun.toInt, s"$name $b teeth")
              assertEqualsDouble(g.outline.map((x, y) => math.hypot(x, y)).max, st.set.sun.toInt * m / 2.0 + m, 1e-6, s"$name $b pitch radius")
              assertEquals(g.outerRadius, None)
            case Body.R1 | Body.R2 =>
              val g = ext.find(_.body.contains(b)).get
              assertEquals(g.teeth, st.set.ring.toInt, s"$name $b teeth")
              val rs = g.outline.map((x, y) => math.hypot(x, y))
              assertEqualsDouble(rs.min, st.set.ring.toInt * m / 2.0 - m, 1e-6, s"$name $b pitch radius")
              val outer = g.outerRadius.getOrElse(fail(s"$name $b: ring is a solid disc"))
              assertEqualsDouble(outer, st.ringOuterDiameter / 2.0, 1e-9)
              assert(rs.min < outer && rs.max < outer, s"$name $b: hole not inside the disc")
            case _ => ()
        // planet halves
        val halves = mine.flatMap(_.prims).collect { case e: Extruded => e }.filter(_.teeth == st.set.planet.toInt)
        assert(halves.nonEmpty, s"$name stage $i planet gears")
        for h <- halves do
          assertEqualsDouble(h.outline.map((x, y) => math.hypot(x, y)).max, st.set.planet.toInt * m / 2.0 + m, 1e-6, s"$name planet pitch radius")
      // sun-less / absent: no gear for an absent sun
      for b <- Seq(Body.S1, Body.S2) if !d.topology.presentBodies.contains(b) do
        assert(!ext.exists(_.body.contains(b)), s"$name draws absent $b")
      // colours per body, as the 2D schematic decides them
      for e <- ext; b <- e.body do assertEquals(e.tone, expectedTone(d, b), s"$name $b colour")
      for c <- l.clusters; b <- c.bodies if b == Body.C1 || b == Body.C2 do
        val plate = c.prims.collect { case x: Bar => x }.filter(x => if b == Body.C1 then x.z1 <= 0.0 else x.z0 > 0.0)
        assert(plate.forall(_.tone == expectedTone(d, b)), s"$name $b arm colour")

  test("planet centres equal the schematic's planet positions (mirrored) at any phase"):
    for (name, d) <- presetDesigns do
      val a = Kinematics.at(d, 1.234)
      val l = build(d)
      for c <- l.clusters; p <- c.planets do
        val st = if p.stage == 1 then d.stage1 else d.stage2.get
        val ang = -clusterAngle(c, a) // the group turns by minus the schematic angle
        val wx  = p.x * math.cos(ang) - p.y * math.sin(ang)
        val wy  = p.x * math.sin(ang) + p.y * math.cos(ang)
        val (sx, sy) = SchematicLayout.planetPositionAt(p.index, st.set.numPlanets, 0.0, st.centerToCenter, a.of(p.carrier))
        assertEqualsDouble(wx, sx, 1e-9, s"$name planet ${p.index} x")
        assertEqualsDouble(wy, -sy, 1e-9, s"$name planet ${p.index} y")
      // and at rest, against the layout the schematic draws
      for c <- l.clusters; p <- c.planets do
        val st  = if p.stage == 1 then d.stage1 else d.stage2.get
        val lay = SchematicLayout.stageLayout(st, 0.0)
        assertEqualsDouble(p.x, lay.planets(p.index)._1 / lay.scale, 1e-9)
        assertEqualsDouble(p.y, -lay.planets(p.index)._2 / lay.scale, 1e-9)

  private def selfIntersections(p: Vector[(Double, Double)]): Int =
    def cross(a: (Double, Double), b: (Double, Double), c: (Double, Double)) =
      (b._1 - a._1) * (c._2 - a._2) - (b._2 - a._2) * (c._1 - a._1)
    val n = p.size
    (for
      i <- 0 until n
      j <- i + 2 until n if !(i == 0 && j == n - 1)
      (a, b, c, d) = (p(i), p((i + 1) % n), p(j), p((j + 1) % n))
      if cross(a, b, c) * cross(a, b, d) < 0 && cross(c, d, a) * cross(c, d, b) < 0
    yield 1).size

  test("gear outlines are simple polygons (extrudable) spanning root to tip"):
    for (teeth, m, rot) <- Seq((8, 3.0, 0.0), (11, 2.0, 0.3), (25, 2.0, 0.7), (47, 2.0, 1.1)) do
      for (internal, pts) <- Seq(
        false -> SchematicLayout.externalGearPoints(teeth, m, rot),
        true  -> SchematicLayout.internalGearPoints(teeth, m, rot)
      ) do
        assertEquals(selfIntersections(pts), 0, s"teeth=$teeth internal=$internal")
        val r  = cpgc.render.GearGeometry.gearRadii(teeth, m, internal)
        val rs = pts.map((x, y) => math.hypot(x, y))
        assertEqualsDouble(rs.max, if internal then r.root else r.outer, 1e-6)
        assertEqualsDouble(rs.min, if internal then r.outer else r.root, 1e-6)

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
