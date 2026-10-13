package cpgc.`export`

import cpgc.constraints.{Body, BodyRole, Coupling, Topology}
import cpgc.domain.{CompoundDesign, GearSet, Module, Ratio, Stage, Teeth}
import munit.FunSuite

/** P3: OpenSCAD phasing, bores, ring OD, stage placement.
  *
  * The phase checker below is deliberately independent of `OpenScad`: it
  * re-derives the mesh conditions of docs/verification/assembly-conditions.md
  * from the numbers parsed out of the generated script. */
class OpenScadSpec extends FunSuite:
  import ExportFixtures.*

  // -- parsing ------------------------------------------------------------------

  private final case class Parsed(
      vars: Map[String, Double], planetRot: Vector[Double], z: Double, sunGears: Int,
      text: String
  )

  private val VarRx = """([a-z_]+) = (-?\d+(?:\.\d+)?);.*""".r
  private val VecRx = """planet_rot = \[([^\]]*)\];.*""".r
  private val ZRx   = """translate\(\[0, 0, (-?\d+(?:\.\d+)?)\]\)""".r

  private def parseStage(scad: String, name: String): Parsed =
    val start = scad.indexOf(s"module $name()")
    assert(start >= 0, s"no module $name")
    val next  = scad.indexOf("\nmodule ", start + 1)
    val end   = if next < 0 then scad.indexOf("\nstage1();", start) else next
    val text  = scad.substring(start, if end < 0 then scad.length else end)
    val lines = text.linesIterator.map(_.trim).toVector
    val vars  = lines.flatMap(l => VarRx.findPrefixMatchOf(l)).map(m => m.group(1) -> m.group(2).toDouble).toMap
    val vec   = lines.flatMap(l => VecRx.findPrefixMatchOf(l)).head.group(1).split(",").map(_.trim.toDouble).toVector
    val z     = ZRx.findFirstMatchIn(text).get.group(1).toDouble
    Parsed(vars, vec, z, text.linesIterator.count(_.contains("sun_gear")), text)

  // -- independent mesh checker -----------------------------------------------

  private def frac(x: Double) = x - math.floor(x)
  private def isInt(x: Double) = { val f = frac(x); math.min(f, 1 - f) < 2e-3 }

  /** Library convention (see OpenScad.scala): external gear = tooth centred on +x at
    * rotation 0; ring_gear = void of an external gear, so its material tooth is centred
    * at half a pitch. Angles in turns, phases in tooth pitches. */
  private def meshErrors(s: Stage, n: Int, p: Parsed, sunPresent: Boolean): Seq[String] =
    val (ts, tp, tr) = (s.set.sun.toInt, s.set.planet.toInt, s.set.ring.toInt)
    val sigma  = p.vars.getOrElse("sun_rot", 0.0) / 360.0
    val rhoEff = p.vars("ring_rot") / 360.0 + 1.0 / (2.0 * tr)
    (0 until n).flatMap { k =>
      val th = k.toDouble / n
      val x  = p.planetRot(k) / 360.0
      val sunOk  = !sunPresent || isInt(ts * (th - sigma) + tp * (th + 0.5 - x) - 0.5)
      val ringOk = isInt(tr * (th - rhoEff) - tp * (th - x) - 0.5)
      Seq(if sunOk then "" else s"sun mesh k=$k", if ringOk then "" else s"ring mesh k=$k").filter(_.nonEmpty)
    }

  private def cross(d: CompoundDesign): Int =
    val s1 = d.stage1.set; val s2 = d.stage2.get.set
    val a = s1.planet.toInt * s2.ring.toInt - s2.planet.toInt * s1.ring.toInt
    val g = BigInt(s1.planet.toInt).gcd(BigInt(s2.planet.toInt)).toInt
    a / g

  // -- goldens ----------------------------------------------------------------

  private val scad = OpenScad.emit(standard)
  private val st1  = parseStage(scad, "stage1")
  private val st2  = parseStage(scad, "stage2")

  test("golden: Standard design, stage 1 numbers") {
    assertEquals(st1.vars("center_distance"), 36.0)
    assertEquals(st1.vars("ring_od"), 103.0) // ADR 0016: 2·(47+2.5) + 2·2
    assertEquals(st1.vars("sun_rot"), 0.0)   // sigma = 72° = 5 pitches of 14.4°
    assertEquals(st1.vars("ring_rot"), 0.0)
    assertEquals(st1.planetRot, Vector(0.0, 0.0, 0.0))
    assertEquals(st1.z, 0.0)
    assert(st1.text.contains("rotate([0, 0, i * 360 / 3])"))
    assert(st1.text.contains("translate([center_distance, 0, 0])"))
  }

  test("golden: Standard design, stage 2 numbers and z placement") {
    assertEquals(st2.vars("center_distance"), 36.0)
    assertEquals(st2.vars("ring_od"), 101.0)
    assertEqualsDouble(st2.vars("sun_rot"), 6.923077, 1e-5) // (10/2 − ½)/26 turn, mod 360/26 deg
    assertEquals(st2.vars("ring_rot"), 0.0)
    assertEquals(st2.planetRot, st1.planetRot) // stacked pinion: one body angle per planet
    assertEquals(st2.z, 14.0)                  // stage height 12 + 2 gap, coaxial
    assert(scad.contains("coaxial"))
  }

  test("golden: ring rim width follows the wall (ADR 0016)") {
    val wall = cpgc.constraints.SearchConfig.DefaultRingWall
    assert(st1.text.contains(s"rim_width=${Numeric.fixed(wall, 6)}"), st1.text)
  }

  test("ring OD equals Stage.ringOuterDiameter") {
    assertEqualsDouble(st1.vars("ring_od"), standard.stage1.ringOuterDiameter, 1e-9)
    assertEqualsDouble(st2.vars("ring_od"), standard.stage2.get.ringOuterDiameter, 1e-9)
  }

  test("golden: sun-less stage has neither sun module call nor sun_rot") {
    val sl = OpenScad.emit(sunless)
    val a  = parseStage(sl, "stage1")
    val b  = parseStage(sl, "stage2")
    assertEquals((a.sunGears, b.sunGears), (0, 0))
    assert(!a.vars.contains("sun_rot") && !b.vars.contains("sun_rot"))
    // planet centre distance still uses the virtual sun
    assertEquals(a.vars("center_distance"), 36.0)
  }

  // -- bores ------------------------------------------------------------------

  test("bores scale with the root diameter and vanish on tiny gears") {
    val big = OpenScad.emit(standard)
    assert(big.contains("bore=4.000000") && big.contains("bore=3.000000"))
    // m = 0.5, planet 8: root diameter 0.5·5.5 = 2.75 -> default 3 mm bore would eat the gear
    val tiny = CompoundDesign(
      Stage(GearSet(Teeth(10), Teeth(8), 3), Module(0.5)), None, Topology.singleStage, Ratio(3.0), 0.97)
    val t = OpenScad.emit(tiny)
    val bores = """bore=(\d+\.\d+)""".r.findAllMatchIn(t).map(_.group(1).toDouble).toVector
    val sunRoot = 0.5 * (10 - 2.5); val planetRoot = 0.5 * (8 - 2.5)
    assertEquals(bores.length, 2)
    bores.zip(Vector(sunRoot, planetRoot)).foreach((b, root) => assert(b == 0.0 || b < 0.5 * root, s"$b vs $root"))
  }

  // -- phase property -----------------------------------------------------------

  test("every planet position is tooth-in-gap (stacked topologies)") {
    var checked = 0
    for
      top    <- Seq(Topology.standardCompound, Topology.nssCompound, Topology.sunlessCdcpCompound,
                    Topology.withAbsentSuns(Topology.cdcpCompound, true, false).toOption.get)
      n      <- 2 to 5
      tp1    <- 6 to 12; tp2 <- 6 to 12
      ts1    <- 6 to 14; ts2 <- 6 to 14
    do
      val d = CompoundDesign(
        Stage(GearSet(Teeth(ts1), Teeth(tp1), n), Module(1.5)),
        Some(Stage(GearSet(Teeth(ts2), Teeth(tp2), n), Module(1.5))), top, Ratio(5.0), 0.9)
      val ok1 = !top.s1Present || (ts1 + d.stage1.set.ring.toInt) % n == 0
      val ok2 = !top.s2Present || (ts2 + d.stage2.get.set.ring.toInt) % n == 0
      if ok1 && ok2 && cross(d) % n == 0 then
        val out = OpenScad.emit(d)
        val a = parseStage(out, "stage1"); val b = parseStage(out, "stage2")
        val errs = meshErrors(d.stage1, n, a, top.s1Present) ++ meshErrors(d.stage2.get, n, b, top.s2Present)
        assert(errs.isEmpty, s"${top.legacyMode} n=$n $ts1/$tp1 $ts2/$tp2: ${errs.mkString(",")}\n$out")
        assertEquals(a.planetRot, b.planetRot)
        checked += 1
    assert(checked > 200, s"only $checked cases checked")
  }

  test("every planet position is tooth-in-gap (single stage and non-stacked compound)") {
    val nonStacked = Topology(
      stages = 2,
      roles = Map(Body.C1 -> BodyRole.Input, Body.R2 -> BodyRole.Output, Body.R1 -> BodyRole.Fixed,
        Body.S1 -> BodyRole.Free, Body.S2 -> BodyRole.Free, Body.C2 -> BodyRole.Free),
      firstCoupling = Coupling.CarrierToCarrier, secondCoupling = Coupling.SunToRing)
    var checked = 0
    for
      n1 <- 2 to 5; n2 <- 2 to 5; tp1 <- 6 to 11; tp2 <- 6 to 11; ts1 <- 6 to 16; ts2 <- 6 to 16
    do
      val s1 = Stage(GearSet(Teeth(ts1), Teeth(tp1), n1), Module(1.5))
      val s2 = Stage(GearSet(Teeth(ts2), Teeth(tp2), n2), Module(1.5))
      if s1.set.evenSpacing && s2.set.evenSpacing then
        val d   = CompoundDesign(s1, Some(s2), nonStacked, Ratio(5.0), 0.9)
        val out = OpenScad.emit(d)
        val errs = meshErrors(s1, n1, parseStage(out, "stage1"), true) ++
          meshErrors(s2, n2, parseStage(out, "stage2"), true)
        assert(errs.isEmpty, s"$errs\n$out")
        checked += 1
    assert(checked > 100)
    val one = OpenScad.emit(single)
    assertEquals(meshErrors(single.stage1, 3, parseStage(one, "stage1"), true), Seq.empty)
  }

  // -- ADR 0018: individually phased pinions -----------------------------------

  private val phasedCfg = cpgc.constraints.SearchConfig(
    topology = Topology.standardCompound,
    sunTeeth = cpgc.constraints.TeethBounds(8, 16), planetTeeth = cpgc.constraints.TeethBounds(8, 12),
    ringTeeth = cpgc.constraints.TeethBounds(30, 60), planets = cpgc.constraints.TeethBounds(3, 4))

  test("phased pinions: designs valid only with phasing mesh in the export, without a warning") {
    val identical = cpgc.Calculator.search(phasedCfg).map(cpgc.search.Scorer.canonicalKey).toSet
    val phased    = cpgc.Calculator.search(phasedCfg.copy(phasedPinions = true)).toVector
    val onlyPhased = phased.filterNot(d => identical.contains(cpgc.search.Scorer.canonicalKey(d)))
    assertEquals(onlyPhased.size, 128)
    for d <- onlyPhased do
      assert(d.phasedPinions)
      val out = OpenScad.emit(d)
      assert(!out.contains("WARNING"), out)
      val a = parseStage(out, "stage1"); val b = parseStage(out, "stage2")
      val errs = meshErrors(d.stage1, d.stage1.set.numPlanets, a, true) ++
        meshErrors(d.stage2.get, d.stage2.get.set.numPlanets, b, true)
      assert(errs.isEmpty, s"${d.stage1.set} ${d.stage2.get.set}: ${errs.mkString(",")}\n$out")
      assert(out.contains("planet_twist = ["), out)
  }

  test("identical-pinion designs stay identical: no twist, one body angle per planet, no warning") {
    val ds = cpgc.Calculator.search(phasedCfg).take(200).toVector
    assert(ds.nonEmpty)
    for d <- ds do
      assert(!d.phasedPinions)
      val out = OpenScad.emit(d)
      assert(!out.contains("WARNING") && !out.contains("planet_twist"), out)
      assertEquals(parseStage(out, "stage1").planetRot, parseStage(out, "stage2").planetRot)
  }

  test("header carries the topology") {
    assert(scad.contains("input S1, output R2, fixed R1"))
    assert(scad.contains("CarrierToCarrier + PlanetStack"))
    assert(OpenScad.emit(nss).contains("absent suns: S2"))
    assert(OpenScad.emit(sunless).contains("absent suns: S1, S2"))
    assert(scad.contains("Mode: Standard"))
  }
