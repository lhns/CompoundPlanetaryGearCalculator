package cpgc.search

import cpgc.Calculator
import cpgc.constraints.{SearchConfig, TeethBounds, Topology}
import cpgc.domain.{CompoundDesign, Ratio}
import cpgc.search.Scorer.SortBy
import munit.FunSuite

/** ADR 0017: one engine, bounded top-K accumulators, chunked driving, truncation. */
class SearchEngineSpec extends FunSuite:

  private def cfgFor(t: Topology, targets: Seq[Double] = Seq(53.12)) = SearchConfig(
    topology = t, sunTeeth = TeethBounds(10, 24), planetTeeth = TeethBounds(8, 14), ringTeeth = TeethBounds(36, 70),
    planets = TeethBounds(3, 4), minModule = 0.0, targets = targets.map(Ratio(_)), topPerTarget = 6)

  private val topologies = Seq(Topology.singleStage, Topology.standardCompound, Topology.cdcpCompound,
    Topology.nssCompound, Topology.sunlessCdcpCompound)

  // -- TopK ----------------------------------------------------------------------

  test("TopK keeps the k best, evicts the worst, breaks ties by arrival and ignores duplicate keys") {
    val t = new TopK[String](3)
    for (s, k) <- Seq(5.0 -> "a", 1.0 -> "b", 3.0 -> "c", 3.0 -> "d", 2.0 -> "e", 3.0 -> "f", 1.0 -> "b") do t.offer(s, k, k)
    assertEquals(t.result, Vector("b", "e", "c")) // d, f lose the tie to c; the second "b" is a duplicate key
    assert(t.accepts(1.5) && !t.accepts(3.0) && !t.accepts(Double.NaN))
    assertEquals(new TopK[Int](0).accepts(0.0), false)
  }

  test("TopK materialises a value only when it is kept") {
    val t = new TopK[Int](1)
    t.offer(1.0, "x", 1)
    var built = false
    t.offer(2.0, "y", { built = true; 2 })
    assert(!built)
  }

  // -- ranking against an exhaustive sort ---------------------------------------------

  for t <- topologies; sort <- SortBy.values do
    test(s"engine top-K by $sort == exhaustive ranking [${t.legacyMode.fold("sun-less CDCP")(_.toString)}]") {
      val cfg = cfgFor(t)
      val all = Calculator.search(cfg).toVector
      val k   = 8
      val got = Calculator.top(cfg, sort, k)
      def score(d: CompoundDesign): Double = sort match
        case SortBy.BestFit    => Scorer.bestTargetError(cfg.targets, d.ratio)
        case SortBy.Ratio      => math.abs(d.ratio.value)
        case SortBy.Efficiency => -d.efficiency
        case SortBy.Simplicity => Scorer.toothCount(d).toDouble
      val ref = all.map(d => Scorer.canonicalKey(d) -> score(d)).toMap.values.toVector.sorted.take(k)
      assertEquals(got.size, math.min(k, all.map(Scorer.canonicalKey).distinct.size))
      got.map(score).zip(ref).foreach((a, b) => assertEqualsDouble(a, b, 1e-12, s"$sort"))
    }

  test("no targets: BestFit ranks by smallest |ratio| (B8)") {
    val cfg = cfgFor(Topology.standardCompound, Nil)
    val top = Calculator.top(cfg, SortBy.BestFit, 5).map(d => math.abs(d.ratio.value))
    val ref = Calculator.search(cfg).map(d => math.abs(d.ratio.value)).sorted.take(5).toVector
    assertEquals(top.size, 5)
    top.zip(ref).foreach((a, b) => assertEqualsDouble(a, b, 1e-12))
    assertEquals(top, top.sorted)
  }

  test("dedup keeps designs that differ only in planet count (B6)") {
    val cfg = cfgFor(Topology.singleStage).copy(planets = TeethBounds(2, 6))
    val n = Calculator.engine(cfg, SortBy.Ratio, Int.MaxValue).run().top
    assertEquals(n.size, Calculator.search(cfg).size)
    assertEquals(n.map(Scorer.canonicalKey).distinct.size, n.size)
  }

  // -- chunked driving, snapshots, truncation ----------------------------------------------

  test("stepping in small chunks gives the same result as running to completion") {
    val cfg = cfgFor(Topology.nssCompound)
    val whole = Calculator.engine(cfg, SortBy.BestFit, 6).run()
    val e = Calculator.engine(cfg, SortBy.BestFit, 6)
    var chunks = 0
    while !e.step(997) do chunks += 1
    assert(chunks > 5)
    assertEquals(e.top.map(Scorer.canonicalKey), whole.top.map(Scorer.canonicalKey))
    assertEquals(e.perTarget.map((t, v) => t -> v.map(_.error)), whole.perTarget.map((t, v) => t -> v.map(_.error)))
    assertEquals((e.examined, e.found), (whole.examined, whole.found))
    assert(e.done && !e.truncated)
  }

  test("snapshots only improve while stepping, and stop() marks a truncated run") {
    val cfg = cfgFor(Topology.standardCompound)
    val e = Calculator.engine(cfg, SortBy.BestFit, 4)
    var last = Double.PositiveInfinity
    var snapshots = 0
    while snapshots < 3 && !e.step(500) do
      val top = e.top
      if top.nonEmpty then
        val worst = Scorer.bestTargetError(cfg.targets, top.last.ratio)
        if top.size == 4 then { assert(worst <= last + 1e-12); last = worst }
      snapshots += 1
    assert(!e.done)
    assert(!e.truncated) // not until the caller gives up
    e.stop()
    assert(e.truncated)
    assert(e.top.size <= 4)
    e.run()
    assert(e.done && !e.truncated)
  }

  test("stop() after completion is not a truncation") {
    val e = Calculator.engine(cfgFor(Topology.singleStage), SortBy.Ratio, 3).run()
    e.stop()
    assert(!e.truncated)
  }

  // -- per-target lists and the Calculator wrappers ----------------------------------------

  test("best(): per-target errors are the k smallest over all designs (B1)") {
    for t <- topologies do
      val cfg = cfgFor(t, Seq(53.12, 7.5))
      val all = Calculator.search(cfg).toVector
      val got = Calculator.best(cfg)
      for target <- cfg.targets do
        val ref = all.map(d => Scorer.canonicalKey(d) -> Scorer.absoluteError(target, d.ratio)).toMap.values.toVector.sorted.take(cfg.topPerTarget)
        val errs = got(target).map(_.error)
        assertEquals(errs.size, ref.size, s"$t")
        errs.zip(ref).foreach((a, b) => assertEqualsDouble(a, b, 1e-12, s"$t target ${target.value}"))
        assertEquals(errs, errs.sorted)
  }

  test("Scorer.topPerTarget evicts the worst, not the best (B1)") {
    val ds = Calculator.search(cfgFor(Topology.standardCompound, Nil)).take(300).toVector
    val target = Ratio(12.5)
    val got = Scorer.topPerTarget(ds, Seq(target), 3)(target).map(_.error)
    assertEquals(got, ds.map(d => Scorer.absoluteError(target, d.ratio)).sorted.take(3))
  }

  // -- hot path ---------------------------------------------------------------------------

  test("the reusable Workspace gives the same ratios as one-shot RoleSolver calls") {
    for t <- topologies do
      val cfg = cfgFor(t)
      val ws = new RoleSolver.Workspace(t)
      val s1s = Enumerator.stage1Sets(cfg).take(60).toVector
      val s2s = Enumerator.stage2Sets(cfg).take(60).toVector
      for a <- s1s; b <- s2s do
        val fresh = RoleSolver.ratio(t, a, if t.stages == 2 then Some(b) else None)
        val ok = ws.solve(a, if t.stages == 2 then b else null, 1.0)
        val q = if ok then ws.ratioOrNaN else Double.NaN
        assertEquals(fresh.map(_.value).getOrElse(Double.NaN).isNaN, q.isNaN)
        fresh.foreach(r => assertEqualsDouble(q, r.value, 1e-12 * math.max(1.0, math.abs(r.value))))
  }

  // -- assembly (ADR 0018) in the engine -------------------------------------------------

  test("phasedPinions widens the stacked design space; identical pinions satisfy the cross term") {
    val cfg = cfgFor(Topology.standardCompound, Nil)
    val strict = Calculator.search(cfg).toVector
    val phased = Calculator.search(cfg.copy(phasedPinions = true)).toVector
    assert(strict.nonEmpty && phased.size > strict.size)
    assert(strict.map(Scorer.canonicalKey).toSet.subsetOf(phased.map(Scorer.canonicalKey).toSet))
    strict.foreach { d =>
      val a = d.stage1.set; val b = d.stage2.get.set
      val g = BigInt(a.planet.toInt).gcd(BigInt(b.planet.toInt)).toInt
      assertEquals(((a.planet.toInt * b.ring.toInt - b.planet.toInt * a.ring.toInt) / g) % a.numPlanets, 0)
    }
  }

  test("non-stacked topologies are unaffected by phasedPinions") {
    val t = Topology(2, Topology.standardCompound.roles, cpgc.constraints.Coupling.CarrierToCarrier, cpgc.constraints.Coupling.RingToSun)
    val cfg = cfgFor(t, Nil)
    assertEquals(Calculator.search(cfg).size, Calculator.search(cfg.copy(phasedPinions = true)).size)
  }
