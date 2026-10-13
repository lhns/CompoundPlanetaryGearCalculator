package cpgc.verify

import cpgc.Calculator
import cpgc.constraints.*
import cpgc.domain.Ratio
import cpgc.search.Scorer
import cpgc.verify.ContractSupport.*
import munit.FunSuite

import scala.util.Random

/** The entry points are views of one engine (ADR 0017) and must agree on what a
  * valid design is:
  *   - `search` and `searchSteps` emit identical design sets, with or without targets;
  *   - everything `Calculator.best` / the engine reports is a design `search` emits;
  *   - identical-stage pairs `(a, a)` are searched;
  *   - negative-ratio designs are found when targeting their magnitude;
  *   - non-stacked topologies pair `N1 != N2`, stacked ones only equal N.
  * Designs are keyed by teeth + planet count of both stages.
  */
class CrossPathSpec extends FunSuite:

  private val NConfigs = 8

  private def keys(ds: Iterable[cpgc.domain.CompoundDesign]): Set[Key] = ds.map(key).toSet

  for c <- cases do
    test(s"crosspath[${c.name}/no targets: search == searchSteps]") {
      val rng = new Random(777L)
      for _ <- 0 until NConfigs do
        val cfg = c.config(randomConfig(rng, withTargets = false))
        val a   = keys(Calculator.search(cfg))
        val b   = keys(Calculator.searchSteps(cfg).flatten.toVector)
        assertEquals(b -- a, Set.empty[Key], s"steps-only designs ${cfgSummary(cfg)}")
        assertEquals(a -- b, Set.empty[Key], s"search-only designs ${cfgSummary(cfg)}")
    }

    test(s"crosspath[${c.name}/targets do not prune: searchSteps == search]") {
      val rng = new Random(889L)
      for _ <- 0 until NConfigs do
        val cfg = c.config(randomConfig(rng, withTargets = true))
        val all = keys(Calculator.search(cfg))
        val st  = keys(Calculator.searchSteps(cfg).flatten.toVector)
        assertEquals(st, all, s"${cfgSummary(cfg)}")
    }

    test(s"crosspath[${c.name}/best() and engine results are designs of search]") {
      val rng = new Random(888L)
      for _ <- 0 until NConfigs do
        val cfg = c.config(randomConfig(rng, withTargets = true))
        val all = keys(Calculator.search(cfg))
        val bst = keys(Calculator.best(cfg).values.flatten.map(_.design))
        val eng = keys(Calculator.top(cfg, Scorer.SortBy.BestFit, 7))
        assertEquals(bst -- all, Set.empty[Key], s"best-only designs ${cfgSummary(cfg)}")
        assertEquals(eng -- all, Set.empty[Key], s"engine-only designs ${cfgSummary(cfg)}")
    }

    // (a,a) pairs are meaningless for single-stage topologies
    if c.topology.stages == 2 then test(s"crosspath[${c.name}/identical-stage pairs (a,a) accepted by compound() are searched]") {
      val rng = new Random(901L)
      for _ <- 0 until NConfigs do
        val cfg   = c.config(randomConfig(rng, withTargets = false))
        val all   = keys(Calculator.search(cfg))
        val pool2 = cpgc.search.Enumerator.stage2Sets(cfg).toSet
        val same  = cpgc.search.Enumerator.stage1Sets(cfg).filter(pool2.contains)
          .flatMap(a => cpgc.search.Compounder.compound(cfg, a, a)).toVector
        assertEquals(keys(same) -- all, Set.empty[Key], s"(a,a) designs missing from search ${cfgSummary(cfg)}")
    }

    test(s"crosspath[${c.name}/negative-ratio designs are found when targeting |ratio|]") {
      val rng = new Random(902L)
      var sawAny = false
      for _ <- 0 until NConfigs do
        val cfg = c.config(randomConfig(rng, withTargets = false))
        val neg = Calculator.search(cfg).filter(d => d.stage2.isDefined && d.ratio.value < -1e-6).take(6).toVector
        sawAny ||= neg.nonEmpty
        neg.foreach { d =>
          val t    = Ratio(math.abs(d.ratio.value))
          val best = Calculator.best(cfg.copy(targets = Seq(t)))(t)
          assertEqualsDouble(best.head.error, 0.0, 1e-9, s"missed ${fmt(d)} ${cfgSummary(cfg)}")
        }
      assume(sawAny, "topology produced no negative-ratio designs")
    }

    test(s"crosspath[${c.name}/designs with N1==N2 are found when targeting their ratio (control)]") {
      val rng = new Random(903L)
      for _ <- 0 until NConfigs do
        val cfg = c.config(randomConfig(rng, withTargets = false))
        val pos = Calculator.search(cfg)
          .filter(d => d.stage2.exists(_.set.numPlanets == d.stage1.set.numPlanets) && d.ratio.value > 1e-6)
          .take(6).toVector
        pos.foreach { d =>
          val best = Calculator.best(cfg.copy(targets = Seq(d.ratio)))(d.ratio)
          assertEqualsDouble(best.head.error, 0.0, 1e-9, s"missed ${fmt(d)} ${cfgSummary(cfg)}")
        }
    }

    if c.topology.stages == 2 then test(s"crosspath[${c.name}/planet counts: stacked pairs equal N, non-stacked may differ]") {
      val rng = new Random(904L)
      var mixed = false
      for _ <- 0 until NConfigs do
        val cfg = c.config(randomConfig(rng, withTargets = false)).copy(planets = TeethBounds(3, 4))
        val ds  = Calculator.search(cfg).toVector
        if c.topology.stacked then assert(ds.forall(d => d.stage1.set.numPlanets == d.stage2.get.set.numPlanets))
        else mixed ||= ds.exists(d => d.stage1.set.numPlanets != d.stage2.get.set.numPlanets)
      if !c.topology.stacked then assert(mixed, "non-stacked topology produced no N1 != N2 design")
    }
