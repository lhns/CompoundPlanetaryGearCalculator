package cpgc.verify

import cpgc.Calculator
import cpgc.constraints.{SearchConfig, TeethBounds, Topology}
import cpgc.domain.{GearSet, Teeth}
import cpgc.physics.PlanetSpacing
import munit.FunSuite

import scala.io.Source

/** Differential test against fixtures generated from the author's reference
  * Python program (see tools/reference/README.md). JVM-only: reads classpath files.
  *
  * Mapping (ADR 0015/0016): Standard -> standardCompound, NSS/NSSFIX ->
  * nssCompound, and the reference's CDCP -> the CDCP with **both suns absent**
  * (`sunlessCdcpCompound`; the reference has no suns).
  *
  * Our module formula differs from the reference's (`m1 = OD/(R1+2)`), so we
  * run with ringWall 0, minModule 0 and planetClearance 0 and post-filter both
  * sides with the reference's module rule computed here (`m1 = OD/(R1+2) >
  * MinModule`, `m2 = m1·(S1+P1)/(S2+P2) > MinModule`). Both sides are also
  * restricted to the intersection of the two planet-fit predicates (reference:
  * atan; ours: chord sin). Teeth sets and ratios are then compared keyed by
  * (s1,p1,s2,p2); the module values themselves are not compared.
  */
class ReferenceDifferentialSpec extends FunSuite:

  private final case class Row(
      ratio: Double, n: Int, m1: Double, s1: Int, p1: Int, r1: Int,
      m2: Double, s2: Int, p2: Int, r2: Int):
    def key: (Int, Int, Int, Int) = (s1, p1, s2, p2)

  private final case class Fixture(
      file: String, mode: String, bounds: String, n: Int, minTeeth: Int, maxTeeth: Int,
      maxSun: Int, minRing: Int, maxRing: Int, minModule: Double, spf: Double, od: Double)

  private final case class Result(
      refTotal: Int, refKept: Int, oursTotal: Int, oursKept: Int,
      missing: Vector[Row], extra: Vector[Row], mismatched: Vector[(Row, Row, String)]):
    def common: Int = refKept - missing.size

  private def lines(path: String): Vector[String] =
    val is = Option(getClass.getResourceAsStream(path))
      .getOrElse(fail(s"missing classpath resource $path"))
    try Source.fromInputStream(is, "UTF-8").getLines().toVector.drop(1).filter(_.nonEmpty)
    finally is.close()

  private def fixtures: Vector[Fixture] =
    lines("/reference/manifest.csv").map { l =>
      val c = l.split(',')
      Fixture(c(0), c(1), c(2), c(3).toInt, c(4).toInt, c(5).toInt, c(6).toInt, c(7).toInt,
        c(8).toInt, c(9).toDouble, c(10).toDouble, c(11).toDouble)
    }

  private def readRef(f: Fixture): Vector[Row] =
    lines(s"/reference/${f.file}").map { l =>
      val c = l.split(',')
      Row(c(0).toDouble, c(1).toInt, c(2).toDouble, c(3).toInt, c(4).toInt, c(5).toInt,
        c(6).toDouble, c(7).toInt, c(8).toInt, c(9).toInt)
    }

  private def topology(f: Fixture): Topology = f.mode match
    case "Standard"        => Topology.standardCompound
    case "NSS" | "NSSFIX"  => Topology.nssCompound
    case "CDCP"            => Topology.sunlessCdcpCompound

  /** Translate the reference's settings (exclusive range upper bounds, strict ring
    * bounds, CDCP overrides) to our inclusive bounds. Module, wall and clearance
    * are neutralised (see class doc); the module rule is applied in `run`. */
  private def config(f: Fixture): SearchConfig =
    val cdcp = f.mode == "CDCP"
    SearchConfig(
      topology        = topology(f),
      sunTeeth        = TeethBounds(if cdcp then 2 else f.minTeeth, f.maxSun - 1),
      planetTeeth     = TeethBounds(f.minTeeth, f.maxTeeth - 1),
      ringTeeth       = TeethBounds(f.minRing + 1, f.maxRing - 1),
      planets         = TeethBounds(f.n, f.n),
      minModule       = 0.0,
      firstStageOD    = f.od,
      sunPlanetFactor = if cdcp then 100000.0 else f.spf,
      ringWall        = 0.0,
      planetClearance = 0.0
    )

  /** The reference's module rule: `m1 = OD/(R1+2)`, `m2 = m1·(S1+P1)/(S2+P2)`, both strictly above MinModule. */
  private def refModuleOk(f: Fixture, r: Row): Boolean =
    val m1 = f.od / (r.r1 + 2.0)
    val m2 = m1 * (r.s1 + r.p1).toDouble / (r.s2 + r.p2).toDouble
    m1 > f.minModule && m2 > f.minModule

  /** The reference still bounds *every* stage's sun by its sun range and
    * sun/planet factor, also where our model has no sun (the virtual sun of a
    * sun-less stage; ADR 0015 §6 drops these limits for us). */
  private def refSunsOk(f: Fixture, r: Row): Boolean =
    val cdcp = f.mode == "CDCP"
    def ok(s: Int, p: Int) =
      s >= (if cdcp then 2 else f.minTeeth) && s <= f.maxSun - 1 && p < (if cdcp then 100000.0 else f.spf) * s
    ok(r.s1, r.p1) && ok(r.s2, r.p2)

  /** Reference fit predicate (note its truncated 2*pi constant). */
  private def fitAtan(s: Int, p: Int, n: Int): Boolean =
    p + 2.0 < math.atan(6.28319 / n / 2.0) * ((p + s) / 2.0) * 2.0

  private def fitSin(s: Int, p: Int, n: Int): Boolean =
    PlanetSpacing.planetsFitWithoutOverlap(GearSet(Teeth(s), Teeth(p), n), 0.0)

  private def close(a: Double, b: Double): Boolean =
    math.abs(a - b) <= 1e-9 * math.max(1.0, math.max(math.abs(a), math.abs(b)))

  private def run(f: Fixture): Result =
    val cfg  = config(f)
    val ref  = readRef(f)
    val ours = Calculator.search(cfg).map { d =>
      val s2 = d.stage2.get
      Row(d.ratio.toDouble, f.n, d.stage1.module.value, d.stage1.set.sun.toInt,
        d.stage1.set.planet.toInt, d.stage1.set.ring.toInt, s2.module.value,
        s2.set.sun.toInt, s2.set.planet.toInt, s2.set.ring.toInt)
    }.toVector
    val refKept  = ref.filter(r => fitSin(r.s1, r.p1, f.n) && fitSin(r.s2, r.p2, f.n) && refModuleOk(f, r))
    val oursKept = ours.filter(r =>
      fitAtan(r.s1, r.p1, f.n) && fitAtan(r.s2, r.p2, f.n) && refModuleOk(f, r) && refSunsOk(f, r))
    val refMap  = refKept.map(r => r.key -> r).toMap
    val oursMap = oursKept.map(r => r.key -> r).toMap
    val missing = refKept.filterNot(r => oursMap.contains(r.key))
    val extra   = oursKept.filterNot(r => refMap.contains(r.key))
    val mism = refKept.flatMap { r =>
      oursMap.get(r.key).flatMap { o =>
        val bad =
          if close(r.ratio, o.ratio) then "" else s"ratio ${r.ratio} vs ${o.ratio}; "
        if bad.isEmpty then None else Some((r, o, bad))
      }
    }
    Result(ref.size, refKept.size, ours.size, oursKept.size, missing, extra, mism)

  private lazy val results: Map[String, Result] =
    val rs = fixtures.map(f => f.file -> run(f)).toMap
    val hdr = f"${"fixture"}%-20s ${"ref"}%6s ${"refKept"}%7s ${"ours"}%6s ${"oursKept"}%8s " +
      f"${"common"}%6s ${"missing"}%7s ${"extra"}%6s ${"mismatch"}%8s"
    println(hdr)
    fixtures.foreach { f =>
      val r = rs(f.file)
      println(f"${f.file}%-20s ${r.refTotal}%6d ${r.refKept}%7d ${r.oursTotal}%6d ${r.oursKept}%8d " +
        f"${r.common}%6d ${r.missing.size}%7d ${r.extra.size}%6d ${r.mismatched.size}%8d")
      def ex(label: String, rows: Vector[Row]): Unit =
        if rows.nonEmpty then
          println(s"    $label e.g. " + rows.take(5).map(r => s"(s1=${r.s1},p1=${r.p1},s2=${r.s2},p2=${r.p2})").mkString(" "))
      ex("missing", r.missing)
      ex("extra", r.extra)
      if r.mismatched.nonEmpty then
        println("    mismatch e.g. " + r.mismatched.take(5).map { case (a, _, w) => s"${a.key}: $w" }.mkString(" | "))
    }
    rs

  // Known disagreements (kept as `.fail` so the build stays green).
  //  - NSS (literal reference): the reference reads PlanetSpacingOK at the stale global
  //    Planet1/Planet2 (= index 0), so it never really filters stage-1 spacing (or rejects
  //    everything when gearset 0 is unspaced). Ours applies stage-1 spacing
  //    (Compounder.scala:168), so it lacks the reference's unspaced designs. The NSSFIX
  //    fixtures (reference repaired to stage-1-only) are the real oracle for NSS.
  private def expectFail(f: Fixture, category: String): Boolean =
    (f.mode, category) match
      case ("NSS", "extra") if f.file == "nss_A_N3.csv" => true // reference yields 0 rows
      case ("NSS", "missing") => f.file != "nss_A_N3.csv"
      case _                             => false

  fixtures.foreach { f =>
    val name = f.file
    val nm = s"$name: no missing designs"
    val ne = s"$name: no extra designs"
    val nv = s"$name: ratios agree"
    def sample(rows: Vector[Row]) = rows.take(5).map(_.key).mkString(", ")
    if expectFail(f, "missing") then
      test((nm + " (known disagreement)").fail) {
        val r = results(name); assertEquals(r.missing.size, 0, sample(r.missing))
      }
    else
      test(nm) { val r = results(name); assertEquals(r.missing.size, 0, sample(r.missing)) }
    if expectFail(f, "extra") then
      test((ne + " (known disagreement)").fail) {
        val r = results(name); assertEquals(r.extra.size, 0, sample(r.extra))
      }
    else
      test(ne) { val r = results(name); assertEquals(r.extra.size, 0, sample(r.extra)) }
    if expectFail(f, "mismatch") then
      test((nv + " (known disagreement)").fail) {
        val r = results(name); assertEquals(r.mismatched.size, 0)
      }
    else
      test(nv) { val r = results(name); assertEquals(r.mismatched.size, 0) }
  }

  // Cause checks for the NSS disagreement (these pass).
  test("NSS (literal reference): reference output contains unevenly spaced stage-1 sets") {
    // Stale global Planet1 (= 0) in the reference means stage-1 spacing is never really tested.
    val f = fixtures.find(_.file == "nss_A_N4.csv").get
    assert(readRef(f).exists(r => (r.s1 + r.r1) % f.n != 0))
  }

  test("sun-less CDCP ignores the sun-teeth range and the sun/planet factor (ADR 0015 §2, §6)") {
    fixtures.filter(_.mode == "CDCP").foreach { f =>
      val forced = config(f)
      val dflt   = forced.copy(sunTeeth = TeethBounds(8, f.maxSun - 1), sunPlanetFactor = 3.0)
      def keys(c: SearchConfig) = Calculator.search(c).map(d =>
        (d.stage1.set.planet.toInt, d.stage1.set.ring.toInt, d.stage2.get.set.planet.toInt, d.stage2.get.set.ring.toInt)).toSet
      assertEquals(keys(dflt), keys(forced), f.file)
    }
  }
