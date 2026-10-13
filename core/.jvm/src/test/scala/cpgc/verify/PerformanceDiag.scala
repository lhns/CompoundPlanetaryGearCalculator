package cpgc.verify

import cpgc.Calculator
import cpgc.constraints.*
import cpgc.domain.Ratio
import cpgc.search.Scorer.SortBy
import munit.FunSuite

/** Timing diagnostic for the ADR 0017 budget: JVM CLI defaults under ~5 s, and
  * the web default ranges for all seven presets under ~2 s on the JVM (the
  * browser is ~2-3x slower). Excluded by default; run with
  *   sbt "project coreJVM" "set Test / testOptions := Seq()" "testOnly cpgc.verify.PerformanceDiag"
  * Prints cold (first) and warm (best of 3 more) wall times per case. */
class PerformanceDiag extends FunSuite:
  private val diagnostic = munit.Tag("diagnostic")
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  private def roles(in: Body, out: Body, fixed: Body): Map[Body, BodyRole] =
    val pins = Map(in -> BodyRole.Input, out -> BodyRole.Output, fixed -> BodyRole.Fixed)
    Body.values.map(b => b -> pins.getOrElse(b, BodyRole.Free)).toMap

  // The seven web presets (mirrors cpgc.web.UiLogic; core cannot see web).
  private val presets: Vector[(String, Topology)] = Vector(
    "single"       -> Topology.singleStage,
    "standard"     -> Topology.standardCompound,
    "cdcp"         -> Topology.cdcpCompound,
    "nss"          -> Topology.nssCompound,
    "ring-to-sun"  -> Topology(2, roles(Body.S1, Body.R2, Body.R1), Coupling.CarrierToCarrier, Coupling.RingToSun),
    "ravigneaux"   -> Topology(2, roles(Body.S1, Body.C2, Body.R1), Coupling.CarrierToSun, Coupling.RingToRing),
    "simpson"      -> Topology(2, roles(Body.S1, Body.C2, Body.R1), Coupling.CarrierToRing, Coupling.SunToSun)
  )

  private def time[A](body: => A): (A, Double) =
    val t0 = System.nanoTime()
    val r  = body
    (r, (System.nanoTime() - t0) / 1e9)

  private def measure(label: String, cfg: SearchConfig, sort: SortBy = SortBy.BestFit, k: Int = 10): Double =
    def runOnce() = { val e = Calculator.engine(cfg, sort, k).run(); e.top; e.perTarget; e }
    val (e0, cold) = time(runOnce())
    val warm = (1 to 3).map(_ => time(runOnce())._2).min
    println(f"  $label%-34s cold=$cold%6.2fs warm=$warm%6.2fs examined=${e0.examined}%9d found=${e0.found}%8d")
    cold

  test("diagnostic: CLI default ranges (sun 8-80, planet 8-20, ring 40-200, planets 3-4), target 53.12".tag(diagnostic)) {
    println("CLI defaults:")
    val base = SearchConfig(planets = TeethBounds(3, 4), targets = Seq(Ratio(53.12)))
    val cases = Vector("standard" -> Topology.standardCompound, "cdcp" -> Topology.cdcpCompound,
      "nss" -> Topology.nssCompound, "cdcp sun-less" -> Topology.sunlessCdcpCompound, "single" -> Topology.singleStage)
    val worst = cases.map((n, t) => measure(s"$n", base.copy(topology = t))).max
    measure("standard, phasedPinions", base.copy(topology = Topology.standardCompound, phasedPinions = true))
    measure("standard, no targets (ratio rank)", base.copy(topology = Topology.standardCompound, targets = Seq.empty), SortBy.Ratio)
    println(f"  worst cold = $worst%.2fs (budget ~5 s)")
  }

  test("diagnostic: web default ranges (sun 10-40, planet 10-18, ring 40-120, planets 3-4), all presets".tag(diagnostic)) {
    println("Web defaults, TopK = 16000 (JVM proxy; browser ~2-3x slower):")
    val base = SearchConfig(
      sunTeeth = TeethBounds(10, 40), planetTeeth = TeethBounds(10, 18), ringTeeth = TeethBounds(40, 120),
      planets = TeethBounds(3, 4), sunPlanetFactor = 1e9, targets = Seq(Ratio(53.12)))
    val worst = presets.map((n, t) => measure(n, base.copy(topology = t), k = 16000)).max
    val sunless = measure("cdcp sun-less", base.copy(topology = Topology.sunlessCdcpCompound), k = 16000)
    println(f"  worst cold = ${worst max sunless}%.2fs (budget ~2 s)")
  }
