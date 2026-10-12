package cpgc.verify

import cpgc.Calculator
import cpgc.constraints.*
import cpgc.domain.*
import cpgc.search.RoleSolver
import cpgc.verify.ContractSupport.*
import munit.FunSuite

import scala.collection.mutable
import scala.util.Random

/** Per-design contract: every design emitted by `search` / `searchTargeted` /
  * `searchSteps` must satisfy the intended invariants, for each topology built
  * the way the product builds configs (see `ContractSupport.Case`).
  *
  * One test per (case, path, category) so an expected-failure marker (`.fail`)
  * only masks the category it documents. Known production bugs are listed in
  * `known`; when one is fixed munit flags the `.fail` test as passing.
  */
class DesignContractSpec extends FunSuite:

  private val Tol      = 1e-9
  private val NConfigs = 14
  private val paths    = Vector("search", "targeted", "steps")
  private val cats     = Vector("ratio", "teeth", "geometry", "spacing", "module", "stacked", "efficiency", "single")

  private final case class Violation(cat: String, msg: String)
  private final case class Outcome(designs: Int, violations: Map[String, Vector[String]])

  private def run(path: String, cfg: SearchConfig): Vector[CompoundDesign] = path match
    case "search"   => Calculator.search(cfg).toVector
    case "targeted" => Calculator.searchTargeted(cfg).toVector
    case "steps"    => Calculator.searchSteps(cfg).flatten.toVector

  private def check(cfg: SearchConfig, d: CompoundDesign): Vector[Violation] =
    val t   = cfg.effectiveTopology
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
      if !planetsFit(st.set) then v("geometry", s"planets overlap ${st.set}")
    }

    // teeth within bounds (where the body exists)
    def inB(x: Int, b: TeethBounds) = x >= b.min && x <= b.max
    def teethOk(g: GearSet, sunExists: Boolean): Option[String] =
      if sunExists && !inB(g.sun.toInt, cfg.sunTeeth) then Some(s"sun ${g.sun.toInt} out of ${cfg.sunTeeth}")
      else if !inB(g.planet.toInt, cfg.planetTeeth) then Some(s"planet ${g.planet.toInt} out of ${cfg.planetTeeth}")
      else if !inB(g.ring.toInt, cfg.ringTeeth) then Some(s"ring ${g.ring.toInt} out of ${cfg.ringTeeth}")
      else if !inB(g.numPlanets, cfg.planets) then Some(s"N ${g.numPlanets} out of ${cfg.planets}")
      else None
    teethOk(s1, sunExists = true).foreach(m => v("teeth", s"stage1: $m"))
    d.stage2.foreach(s => teethOk(s.set, t.s2Present).foreach(m => v("teeth", s"stage2: $m")))
    // sun/planet factor applies to every stage with a sun
    if s1.planet.toDouble >= cfg.sunPlanetFactor * s1.sun.toDouble then v("teeth", "stage1 violates sunPlanetFactor")
    d.stage2.foreach { s =>
      if t.s2Present && s.set.planet.toDouble >= cfg.sunPlanetFactor * s.set.sun.toDouble then
        v("teeth", "stage2 violates sunPlanetFactor")
    }

    // spacing
    if !specSpacingOk(t, s1, d.stage2.map(_.set)) then v("spacing", "violates intended spacing rule")

    // modules
    val m1Expected = cfg.firstStageOD / (s1.ring.toDouble + 2.0)
    if math.abs(d.stage1.module.value - m1Expected) > 1e-9 then v("module", s"m1 != OD/(R1+2) = $m1Expected")
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

  /** (case, path, category) -> (bug id, observed). Only real production bugs. */
  private val known: Map[(String, String, String), (String, String)] =
    val b4 = for
      m <- Vector("cdcp[topology]", "cdcp[mode-only]")
      p <- Vector("targeted", "steps")
    yield (m, p, "teeth") -> ("B4", "CDCP stage2ForLegacy skips sun range: cfg sun=7..9 planet=8..9 ring=24..27 N=4..5 spf=2, targets 3.41/12.64 emits S2=10 P2=8 N2=4 (sun 10 > 9)")
    val b5 = for
      m <- Vector("PS+CC std-roles", "PS+RingToSun std-roles")
      p <- paths
    yield (m, p, "stacked") -> ("B5", "first=PlanetStack: c2c not matched: S1=10 P1=10 N1=4 m1=4.6875, S2=12 P2=10 N2=4 m2=4.6875 -> c2c 93.75 vs 103.125")
    val b3 = for
      m <- Vector("single S1->R1 C1fixed", "single C1->S1 R1fixed")
      p <- paths
    yield (m, p, "single") -> ("B3", "single-stage topology with Mode.Standard goes down the compound path: stage2 = Some(S2=11 P2=9 N2=4) for S1=10 P1=10 N1=4")
    (b4 ++ b5 ++ b3).toMap

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
      known.get((c.name, path, cat)) match
        case Some((id, _)) => test(s"$id $name0".fail)(body())   // observed: see `known`
        case None          => test(name0)(body())

  // -- B2: efficiency for novel topologies is driven by cfg.mode (Standard) --

  // Observed: efficiency = 0.985^4 = 0.9413 (Standard, 4 meshes); with S2 absent only the
  // planet-ring mesh exists in stage 2 (as for legacy NSS) -> expected 0.985^3.
  test("B2 NSS-like novel topology (S2 absent) gets the 3-mesh NSS efficiency".fail) {
    val c   = cases.find(_.name == "NSS-like (C1 fixed)").get
    val cfg = c.config(randomConfig(new Random(5L), withTargets = false))
    val ds  = Calculator.search(cfg).take(50).toVector
    assert(ds.nonEmpty)
    ds.foreach(d => assertEqualsDouble(d.efficiency, math.pow(0.985, 3), 1e-12, fmt(d)))
  }

  test("legacy topologies: design.mode equals the topology's legacy mode") {
    for c <- cases.filter(_.topology.legacyMode.isDefined) do
      val cfg = c.config(randomConfig(new Random(6L), withTargets = false))
      Calculator.search(cfg).take(20).foreach(d => assertEquals(Option(d.mode), c.topology.legacyMode, c.name))
  }
