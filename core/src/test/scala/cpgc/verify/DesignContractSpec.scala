package cpgc.verify

import cpgc.Calculator
import cpgc.constraints.*
import cpgc.domain.*
import cpgc.search.RoleSolver
import cpgc.verify.ContractSupport.*
import munit.FunSuite

import scala.collection.mutable
import scala.util.Random

/** Per-design contract: every design emitted by `search` / the engine must satisfy
  * the intended invariants, for each topology built the way the product builds configs (see `ContractSupport.Case`).
  *
  * One test per (case, path, category) so a failure names the violated category.
  */
class DesignContractSpec extends FunSuite:

  private val Tol      = 1e-9
  private val NConfigs = 14
  private val paths    = Vector("search", "engine")
  private val cats     = Vector("ratio", "teeth", "geometry", "spacing", "module", "stacked", "efficiency", "single")

  private final case class Violation(cat: String, msg: String)
  private final case class Outcome(designs: Int, violations: Map[String, Vector[String]])

  private def run(path: String, cfg: SearchConfig): Vector[CompoundDesign] = path match
    case "search"   => Calculator.search(cfg).toVector
    case "engine"   => Calculator.engine(cfg, cpgc.search.Scorer.SortBy.Ratio, Int.MaxValue).run().top

  private def check(cfg: SearchConfig, d: CompoundDesign): Vector[Violation] =
    val t   = cfg.topology
    val out = Vector.newBuilder[Violation]
    val s1  = d.stage1.set
    def v(cat: String, msg: String): Unit = out += Violation(cat, s"$msg | ${fmt(d)} | ${cfgSummary(cfg)}")

    // single-stage topology <=> no stage 2
    if (t.stages == 1) != d.stage2.isEmpty then
      v("single", s"stages=${t.stages} but stage2=${d.stage2.map(_ => "Some").getOrElse("None")}")

    // ratio vs. forward solver
    RoleSolver.ratio(t, s1, d.stage2.map(_.set).filter(_ => t.stages == 2)) match
      case Some(r) if math.abs(r.value - d.ratio.value) <= Tol => ()
      case other => v("ratio", s"expected ${other.map(_.value)}")

    // geometry: r = s + 2p, planets fit
    val stages = Vector(d.stage1) ++ d.stage2
    stages.foreach { st =>
      if st.set.ring.toInt != st.set.sun.toInt + 2 * st.set.planet.toInt then v("geometry", "r != s + 2p")
      if !planetsFit(st.set, cfg) then v("geometry", s"planets overlap ${st.set}")
    }

    // teeth within bounds (where the body exists)
    def inB(x: Int, b: TeethBounds) = x >= b.min && x <= b.max
    def teethOk(g: GearSet, sunExists: Boolean): Option[String] =
      if sunExists && !inB(g.sun.toInt, cfg.sunTeeth) then Some(s"sun ${g.sun.toInt} out of ${cfg.sunTeeth}")
      else if !inB(g.planet.toInt, cfg.planetTeeth) then Some(s"planet ${g.planet.toInt} out of ${cfg.planetTeeth}")
      else if !inB(g.ring.toInt, cfg.ringTeeth) then Some(s"ring ${g.ring.toInt} out of ${cfg.ringTeeth}")
      else if !inB(g.numPlanets, cfg.planets) then Some(s"N ${g.numPlanets} out of ${cfg.planets}")
      else None
    teethOk(s1, sunExists = t.s1Present).foreach(m => v("teeth", s"stage1: $m"))
    d.stage2.foreach(s => teethOk(s.set, t.s2Present).foreach(m => v("teeth", s"stage2: $m")))
    // sun/planet factor applies to every stage with a sun
    if t.s1Present && s1.planet.toDouble >= cfg.sunPlanetFactor * s1.sun.toDouble then v("teeth", "stage1 violates sunPlanetFactor")
    d.stage2.foreach { s =>
      if t.s2Present && s.set.planet.toDouble >= cfg.sunPlanetFactor * s.set.sun.toDouble then
        v("teeth", "stage2 violates sunPlanetFactor")
    }

    // spacing
    if !specSpacingOk(t, cfg.phasedPinions, s1, d.stage2.map(_.set)) then v("spacing", "violates intended spacing rule")

    // modules
    specModule1(cfg, s1) match
      case Some(m1) =>
        if math.abs(d.stage1.module.value - m1) > 1e-9 then v("module", s"m1 != (OD - 2 wall)/(R1 + 2.5) = $m1")
      case None => v("module", "design emitted although no positive finite module exists")
    stages.foreach { st =>
      val od = st.module.value * (st.set.ring.toDouble + 2.5) + 2.0 * cfg.ringWall
      if math.abs(st.ringOuterDiameter - od) > 1e-9 then v("module", s"ringOuterDiameter ${st.ringOuterDiameter} != $od")
    }
    if math.abs(d.stage1.ringOuterDiameter - cfg.firstStageOD) > 1e-9 then
      v("module", s"stage-1 ring OD ${d.stage1.ringOuterDiameter} != firstStageOD ${cfg.firstStageOD}")
    stages.foreach(st => if st.module.value < cfg.minModule then v("module", s"module ${st.module.value} < minModule"))

    // stacked => N1 == N2, equal centre distance
    if stacked(t) then d.stage2.foreach { s2 =>
      if s1.numPlanets != s2.set.numPlanets then v("stacked", s"N1=${s1.numPlanets} != N2=${s2.set.numPlanets}")
      val c1 = d.stage1.module.value * (s1.sun.toInt + s1.planet.toInt)
      val c2 = s2.module.value * (s2.set.sun.toInt + s2.set.planet.toInt)
      if math.abs(c1 - c2) > 1e-9 * math.max(1.0, c1) then v("stacked", s"c2c mismatch $c1 vs $c2")
    }

    // efficiency
    if !(d.efficiency > 0.0 && d.efficiency <= 1.0) then v("efficiency", s"efficiency ${d.efficiency}")
    // 0.985 per mesh: 2 per stage with a sun, 1 per sun-less stage (ADR 0015 §7)
    val meshes = (if t.s1Present then 2 else 1) + (if t.stages == 2 then (if t.s2Present then 2 else 1) else 0)
    if math.abs(d.efficiency - math.pow(0.985, meshes)) > 1e-12 then v("efficiency", s"efficiency ${d.efficiency} != 0.985^$meshes")
    out.result()

  private val memo = mutable.HashMap.empty[(String, String), Outcome]

  private def outcome(c: Case, path: String): Outcome =
    memo.getOrElseUpdate((c.name, path), {
      val rng   = new Random(20240607L)
      val viol  = mutable.HashMap.empty[String, Vector[String]]
      var count = 0
      for i <- 0 until NConfigs do
        val cfg = c.config(randomConfig(rng, withTargets = i % 3 != 0))
        val ds  = run(path, cfg)
        count += ds.size
        ds.foreach(d => check(cfg, d).foreach { x =>
          val cur = viol.getOrElse(x.cat, Vector.empty)
          if cur.size < 3 then viol(x.cat) = cur :+ x.msg
        })
      Outcome(count, viol.toMap)
    })

  for
    c    <- cases
    path <- paths
  do
    test(s"contract[${c.name}/$path] is non-vacuous") {
      assert(outcome(c, path).designs > 0, "no designs generated: contract checks would be vacuous")
    }
    for cat <- cats do
      val name0 = s"contract[${c.name}/$path/$cat]"
      def body(): Unit =
        val vs = outcome(c, path).violations.getOrElse(cat, Vector.empty)
        assert(vs.isEmpty, vs.mkString("\n"))
      test(name0)(body())

  test("design.topology is the config's topology and modeName names the legacy alias") {
    for c <- cases do
      val cfg = c.config(randomConfig(new Random(6L), withTargets = false))
      Calculator.search(cfg).take(20).foreach { d =>
        assertEquals(d.topology, c.topology, c.name)
        assertEquals(d.modeName, c.topology.legacyMode.fold("Custom")(_.toString), c.name)
      }
  }
