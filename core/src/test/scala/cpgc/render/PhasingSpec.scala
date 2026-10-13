package cpgc.render

import cpgc.constraints.{Body, BodyRole, Coupling, SearchConfig, TeethBounds, Topology}
import cpgc.domain.{CompoundDesign, GearSet, Module, Ratio, Stage, Teeth}
import cpgc.search.Compounder
import munit.FunSuite

/** The shared phase solver (OpenSCAD export and SVG schematic) against an independent mesh check:
  * a mesh is valid when a tooth of one gear sits centred in a gap of the other along the line of
  * centres (docs/verification/assembly-conditions.md). The check uses its own formulas and
  * an arbitrary common origin `shift` (the schematic draws planet 0 at a quarter turn). */
class PhasingSpec extends FunSuite:

  private def frac(x: Double) = x - math.floor(x)
  private def isInt(x: Double) = { val f = frac(x); math.min(f, 1 - f) < 2e-3 }

  private def stageErrors(
      s: Stage, sunPresent: Boolean, ph: Phasing.StagePhasing, shift: Double, tag: String
  ): Seq[String] =
    val (ts, tp, tr, n) = (s.set.sun.toInt, s.set.planet.toInt, s.set.ring.toInt, s.set.numPlanets)
    (0 until n).flatMap { k =>
      val th = k.toDouble / n + shift
      val x  = ph.planets(k) + shift
      val sunOk  = !sunPresent || isInt(ts * (th - (ph.sun.get + shift)) + tp * (th + 0.5 - x) - 0.5)
      val ringOk = isInt(tr * (th - (ph.ring + shift)) - tp * (th - x) - 0.5)
      Seq(if sunOk then "" else s"$tag sun k=$k", if ringOk then "" else s"$tag ring k=$k").filter(_.nonEmpty)
    }

  private def errors(d: CompoundDesign, shift: Double): Seq[String] =
    val r = Phasing.solve(d)
    stageErrors(d.stage1, d.topology.s1Present, r.stage1, shift, "s1") ++
      d.stage2.zip(r.stage2).toSeq.flatMap((s, p) => stageErrors(s, d.topology.s2Present, p, shift, "s2"))

  private def design(top: Topology, a: (Int, Int), b: (Int, Int), n: Int, phased: Boolean = false) =
    CompoundDesign(
      Stage(GearSet(Teeth(a._1), Teeth(a._2), n), Module(1.5)),
      Some(Stage(GearSet(Teeth(b._1), Teeth(b._2), n), Module(1.5))),
      top, Ratio(5.0), 0.9, phasedPinions = phased)

  private val stackedTops = Seq(
    Topology.standardCompound, Topology.nssCompound, Topology.sunlessCdcpCompound,
    Topology.withAbsentSuns(Topology.cdcpCompound, true, false).toOption.get)

  test("identical pinions: every valid stacked design meshes, in any frame, with no twist") {
    var checked = 0
    for
      top <- stackedTops; n <- 2 to 4
      tp1 <- 6 to 11; tp2 <- 6 to 11; ts1 <- 6 to 13; ts2 <- 6 to 13
    do
      val d = design(top, (ts1, tp1), (ts2, tp2), n)
      if Compounder.satisfiesSpacing(top, false, d.stage1.set, Some(d.stage2.get.set)) then
        for shift <- Seq(0.0, -0.25, 0.1234) do
          assertEquals(errors(d, shift), Seq.empty, s"${top.legacyMode} n=$n $ts1/$tp1 $ts2/$tp2 shift=$shift")
        assert(Phasing.solve(d).meshes && Phasing.solve(d).twists.isEmpty)
        checked += 1
    assert(checked > 200, s"only $checked cases")
  }

  test("phased pinions: designs that need phasing mesh with a per-planet twist") {
    var checked = 0
    for
      top <- stackedTops; n <- 2 to 4
      tp1 <- 6 to 11; tp2 <- 6 to 11; ts1 <- 6 to 13; ts2 <- 6 to 13
    do
      val d = design(top, (ts1, tp1), (ts2, tp2), n, phased = true)
      val s1 = d.stage1.set; val s2 = d.stage2.get.set
      if Compounder.satisfiesSpacing(top, true, s1, Some(s2)) && !Compounder.satisfiesSpacing(top, false, s1, Some(s2)) then
        assertEquals(errors(d, -0.25), Seq.empty, s"${top.legacyMode} n=$n $ts1/$tp1 $ts2/$tp2")
        val r = Phasing.solve(d)
        assert(r.meshes)
        assertEquals(r.twists.map(_.size), Some(n))
        // identical pinions cannot mesh these, so the solver without the flag reports it
        assert(!Phasing.solve(d.copy(phasedPinions = false)).meshes)
        checked += 1
    assert(checked > 100, s"only $checked cases")
  }

  test("single stage and non-stacked compound mesh in any frame") {
    val nonStacked = Topology(
      stages = 2,
      roles = Map(Body.C1 -> BodyRole.Input, Body.R2 -> BodyRole.Output, Body.R1 -> BodyRole.Fixed,
        Body.S1 -> BodyRole.Free, Body.S2 -> BodyRole.Free, Body.C2 -> BodyRole.Free),
      firstCoupling = Coupling.CarrierToCarrier, secondCoupling = Coupling.SunToRing)
    var checked = 0
    for
      n1 <- 2 to 4; n2 <- 2 to 4; tp1 <- 6 to 9; tp2 <- 6 to 9; ts1 <- 6 to 14; ts2 <- 6 to 14
    do
      val s1 = Stage(GearSet(Teeth(ts1), Teeth(tp1), n1), Module(1.5))
      val s2 = Stage(GearSet(Teeth(ts2), Teeth(tp2), n2), Module(1.5))
      if s1.set.evenSpacing && s2.set.evenSpacing then
        val d = CompoundDesign(s1, Some(s2), nonStacked, Ratio(5.0), 0.9)
        assertEquals(errors(d, -0.25), Seq.empty)
        checked += 1
    assert(checked > 100)
    val one = CompoundDesign(Stage(GearSet(Teeth(24), Teeth(12), 3), Module(2.5)), None, Topology.singleStage, Ratio(3.0), 0.97)
    assertEquals(errors(one, -0.25), Seq.empty)
  }

  test("engine output meshes for every sun-less / stacked search result (the schematic's inputs)") {
    for top <- stackedTops; phased <- Seq(false, true) do
      val cfg = SearchConfig(
        topology = top, sunTeeth = TeethBounds(8, 12), planetTeeth = TeethBounds(8, 11),
        ringTeeth = TeethBounds(30, 40), planets = TeethBounds(3, 4), phasedPinions = phased)
      val ds = cpgc.Calculator.search(cfg).take(300).toVector
      assert(ds.nonEmpty, s"${top.legacyMode}")
      for d <- ds do assertEquals(errors(d, -0.25), Seq.empty, s"${top.legacyMode} phased=$phased ${d.stage1.set} ${d.stage2.get.set}")
  }
