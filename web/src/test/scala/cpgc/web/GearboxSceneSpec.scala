package cpgc.web

import cpgc.constraints.{Body, BodyRole, Topology}
import cpgc.domain.{CompoundDesign, GearSet, Module, Ratio, Stage, Teeth}
import cpgc.kinematics.Kinematics
import cpgc.render.GearGeometry
import cpgc.web.GearboxScene.*
import cpgc.web.Palette.Tone
import munit.FunSuite

/** DOM-free checks of the shared scene model, every preset against its design and the 2D colour rules. */
class GearboxSceneSpec extends FunSuite:

  private def stage(sun: Int, planet: Int, n: Int, m: Double = 2.0) =
    Stage(GearSet(Teeth(sun), Teeth(planet), n), Module(m))

  private def two(t: Topology, s1: Stage, s2: Stage) = CompoundDesign(s1, Some(s2), t, Ratio(5.0), 0.9)

  private val stackedA = stage(19, 17, 3, 2.0) // S19 P17 R53
  private val stackedB = stage(19, 14, 3, 2.5) // S19 P14 R47
  private val rav1     = stage(10, 17, 3, 2.0) // S10 P17 R44
  private val rav2     = stage(10, 17, 4, 2.5) // N differs: possible when planets are not stacked

  private[web] val presets: Vector[(String, CompoundDesign)] = Vector(
    "standard"       -> two(Topology.standardCompound, stackedA, stackedB),
    "cdcp"           -> two(Topology.cdcpCompound, stackedA, stackedB),
    "nss"            -> two(Topology.nssCompound, stackedA, stackedB),
    "sunless"        -> two(Topology.sunlessCdcpCompound, stackedA, stackedB),
    "ringToSun"      -> two(Topology.ringToSunCascade, rav1, rav2),
    "ravigneaux"     -> two(Topology.ravigneaux, rav1, rav2),
    "ravigneaux N=3" -> two(Topology.ravigneaux, rav1, rav1.copy(module = Module(2.5))),
    "simpson"        -> two(Topology.simpson, rav1, rav2),
    "single"         -> CompoundDesign(stackedA, None, Topology.singleStage, Ratio(3.0), 0.9)
  )

  private def stageOf(b: Body) = GearboxScene.stageOfBody(b)

  /** The schematic's rule for a body's colour: pinned role, else its coupling, else its stage tint. */
  private def expectedTone(d: CompoundDesign, b: Body): Tone =
    d.topology.roles.get(b) match
      case Some(BodyRole.Input)  => Tone.Input
      case Some(BodyRole.Output) => Tone.Output
      case Some(BodyRole.Fixed)  => Tone.Fixed
      case _ =>
        SchematicLayout.couplingMarks(d.topology).bodies.get(b) match
          case Some(1) => Tone.Cpl1
          case Some(_) => Tone.Cpl2
          case None =>
            val i = stageOf(b)
            b match
              case Body.S1 | Body.S2 => if i == 1 then Tone.Sun1 else Tone.Sun2
              case Body.R1 | Body.R2 => if i == 1 then Tone.Ring1 else Tone.Ring2
              case _                 => Tone.Carrier

  test("every preset: counts, tooth counts, pitch radii, ring holes and absent suns"):
    for (name, d) <- presets do
      val sc = GearboxScene.of(d)
      assertEquals(sc.stages.size, if d.stage2.isDefined then 2 else 1, name)
      for s <- sc.stages do
        val st = s.stage
        val m  = st.module.value
        assertEquals(s.planets.size, st.set.numPlanets, s"$name ${s.carrier.body} planets")
        assertEquals(s.carrier.arms.size, st.set.numPlanets, s"$name ${s.carrier.body} carrier arms")
        assertEquals(s.carrier.arms, s.planets.map(_.centre), s"$name arms end at the planets")
        assertEqualsDouble(s.carrier.radius, st.centerToCenter, 1e-9)
        s.planets.foreach { p =>
          assertEquals(p.teeth, st.set.planet.toInt)
          assertEqualsDouble(p.pitchR, st.set.planet.toInt * m / 2.0, 1e-9)
          assertEqualsDouble(p.outline.map((x, y) => math.hypot(x, y)).max, p.pitchR + m, 1e-6, s"$name planet tip")
          assertEqualsDouble(math.hypot(p.centre._1, p.centre._2), st.centerToCenter, 1e-9)
        }
        s.sun.foreach { g =>
          assertEquals(g.teeth, st.set.sun.toInt)
          assertEqualsDouble(g.pitchR, st.set.sun.toInt * m / 2.0, 1e-9)
          assertEqualsDouble(g.outline.map((x, y) => math.hypot(x, y)).max, g.pitchR + m, 1e-6, s"$name sun tip")
          assertEquals(g.outerRadius, None)
        }
        val r  = s.ring
        val rs = r.outline.map((x, y) => math.hypot(x, y))
        assertEquals(r.teeth, st.set.ring.toInt)
        assertEqualsDouble(r.pitchR, st.set.ring.toInt * m / 2.0, 1e-9)
        assertEqualsDouble(rs.min, r.pitchR - m, 1e-6, s"$name ring tip")
        val outer = r.outerRadius.getOrElse(fail(s"$name: ring has no outer radius"))
        assertEqualsDouble(outer, st.ringOuterDiameter / 2.0, 1e-9)
        assert(rs.max < outer && rs.min < rs.max, s"$name: ring hole must lie inside the disc")
      for b <- Seq(Body.S1, Body.S2) if !d.topology.presentBodies.contains(b) do
        assert(!sc.gears.exists(_.body.contains(b)), s"$name draws absent $b")
      val sunless = Seq(Body.S1, Body.S2).count(b => d.topology.stages >= stageOf(b) && !d.topology.presentBodies.contains(b))
      assertEquals(sc.stages.count(_.sun.isEmpty), sunless, name)

  test("every preset: colours follow role > coupling > stage tint, per body"):
    for (name, d) <- presets do
      val sc = GearboxScene.of(d)
      for g <- sc.gears; b <- g.body do assertEquals(g.tone, expectedTone(d, b), s"$name $b")
      for s <- sc.stages do assertEquals(s.carrier.tone, expectedTone(d, s.carrier.body), s"$name ${s.carrier.body}")
      val stackSlot = SchematicLayout.couplingMarks(d.topology).planets
      for p <- sc.gears.filter(_.kind == Kind.Planet) do
        assertEquals(p.coupling, stackSlot, name)
        assertEquals(p.tone, stackSlot.fold(if p.stage == 1 then Tone.Planet1 else Tone.Planet2)(Palette.slotTone), name)

  test("Ravigneaux: Ring 2 is free, so it takes the ring-ring coupling colour while fixed Ring 1 stays fixed"):
    val sc = GearboxScene.of(presets.find(_._1 == "ravigneaux").get._2)
    assertEquals(sc.stages(0).ring.tone, Tone.Fixed)
    assertEquals(sc.stages(1).ring.tone, Tone.Cpl2)
    assertEquals(sc.stages(1).sun.get.tone, Tone.Cpl1) // Sun 2 is welded to Carrier 1 (coupling 1)

  test("only a stacked design shares planet parts between the stages"):
    for (name, d) <- presets do
      val sc = GearboxScene.of(d)
      assertEquals(sc.stacked, SchematicLayout.couplingMarks(d.topology).planets.isDefined, name)
      if sc.stacked then assertEquals(sc.stages(0).planets.size, sc.stages(1).planets.size, name)

  /** a - b folded into (-π, π]. */
  private def angleDiff(a: Double, b: Double): Double = math.atan2(math.sin(a - b), math.cos(a - b))

  test("planet k sits at k*360/N on the carrier circle (planet 0 at the top) and follows its carrier"):
    for (name, d) <- presets do
      val a  = Kinematics.at(d, 1.234)
      val sc = GearboxScene.of(d)
      for s <- sc.stages; (p, k) <- s.planets.zipWithIndex do
        val n = s.planets.size
        val (x, y) = p.centre
        assertEqualsDouble(math.hypot(x, y), s.stage.centerToCenter, 1e-9, name)
        assertEqualsDouble(angleDiff(math.atan2(y, x), 2.0 * math.Pi * k / n - math.Pi / 2.0), 0.0, 1e-9, s"$name planet $k") // y down: clockwise on screen
        val c = carrierAngle(s, a)
        val (wx, wy) = planetCentreAt(p, c)
        assertEqualsDouble(math.hypot(wx, wy), s.stage.centerToCenter, 1e-9, name)
        assertEqualsDouble(angleDiff(math.atan2(wy, wx), math.atan2(y, x) + c), 0.0, 1e-9, s"$name planet $k turned")

  test("the schematic places every gear from the scene: centre and outline scale with the stage"):
    for (name, d) <- presets do
      val sc = GearboxScene.of(d)
      for s <- sc.stages; g <- s.gears do
        val lay = SchematicLayout.stageLayout(s.stage, 110.0)
        val (x, y) = SchematicLayout.gearCentre(g, lay.scale, 110.0)
        assertEqualsDouble(x, 110.0 + lay.scale * g.centre._1, 1e-9, name)
        assertEqualsDouble(y, lay.scale * g.centre._2, 1e-9, name)
        val path = SchematicLayout.gearPath(g, lay.scale, 110.0)
        assert(path.startsWith("M ") && path.endsWith("Z"), name)
        assertEquals(path.count(_ == 'L') + 1, g.outline.size, s"$name ${g.id}")

  test("gear outlines are simple polygons (extrudable) spanning root to tip"):
    def selfIntersections(p: Vector[(Double, Double)]): Int =
      def cross(a: (Double, Double), b: (Double, Double), c: (Double, Double)) =
        (b._1 - a._1) * (c._2 - a._2) - (b._2 - a._2) * (c._1 - a._1)
      val n = p.size
      (for
        i <- 0 until n
        j <- i + 2 until n if !(i == 0 && j == n - 1)
        (a, b, c, d) = (p(i), p((i + 1) % n), p(j), p((j + 1) % n))
        if cross(a, b, c) * cross(a, b, d) < 0 && cross(c, d, a) * cross(c, d, b) < 0
      yield 1).size
    for (teeth, m, rot) <- Seq((8, 3.0, 0.0), (11, 2.0, 0.3), (25, 2.0, 0.7), (47, 2.0, 1.1)) do
      for (internal, pts) <- Seq(
        false -> SchematicLayout.externalGearPoints(teeth, m, rot),
        true  -> SchematicLayout.internalGearPoints(teeth, m, rot)
      ) do
        assertEquals(selfIntersections(pts), 0, s"teeth=$teeth internal=$internal")
        val r  = GearGeometry.gearRadii(teeth, m, internal)
        val rs = pts.map((x, y) => math.hypot(x, y))
        assertEqualsDouble(rs.max, if internal then r.root else r.outer, 1e-6)
        assertEqualsDouble(rs.min, if internal then r.outer else r.root, 1e-6)

  test("the fixed body never turns; planets turn relative to their carrier"):
    for (name, d) <- presets if d.stage2.isDefined do
      val a  = Kinematics.at(d, 1.234)
      val sc = GearboxScene.of(d)
      assertEqualsDouble(a.of(d.topology.fixed), 0.0, 1e-9, name)
      for s <- sc.stages; p <- s.planets do
        val spin = if s.carrier.body == Body.C1 then a.planet1Spin else a.planet2Spin
        assertEqualsDouble(angle(p, a), spin - a.of(s.carrier.body), 1e-12)
      for g <- sc.gears; b <- g.body do assertEqualsDouble(angle(g, a), a.of(b), 1e-12)

  test("welded bodies share a cluster"):
    def cl(t: Topology) = clusterBodies(two(t, rav1, rav1))
    assert(cl(Topology.standardCompound).contains(Vector(Body.C1, Body.C2)))
    assert(cl(Topology.ringToSunCascade).contains(Vector(Body.R1, Body.S2)))
    assert(cl(Topology.ravigneaux).contains(Vector(Body.C1, Body.S2)) && cl(Topology.ravigneaux).contains(Vector(Body.R1, Body.R2)))

  test("Palette: themed tones differ between themes, tints do not; css() names a variable only for themed tones"):
    for t <- Seq(Tone.Input, Tone.Output, Tone.Fixed, Tone.Cpl1, Tone.Cpl2) do
      assertNotEquals(Palette.hex(t, light = false), Palette.hex(t, light = true), t.toString)
      assert(Palette.css(t).startsWith("var(--cpgc-"))
      assert(Palette.styleBlock(".x").contains(f"${Palette.hex(t, light = true)}%06x"))
    for t <- Seq(Tone.Sun1, Tone.Ring2, Tone.Carrier) do
      assertEquals(Palette.hex(t, light = false), Palette.hex(t, light = true))
      assert(Palette.css(t).startsWith("#"))
    assertEquals((Palette.slotTone(1), Palette.slotTone(2)), (Tone.Cpl1, Tone.Cpl2))
