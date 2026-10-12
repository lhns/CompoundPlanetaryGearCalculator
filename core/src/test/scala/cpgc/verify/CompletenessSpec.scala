package cpgc.verify

import cpgc.Calculator
import cpgc.constraints.*
import cpgc.domain.*
import cpgc.search.Scorer
import cpgc.verify.ContractSupport.*
import munit.FunSuite

/** Completeness: the top-K errors the product reports must equal those of an
  * exhaustive reference computed here from the intended rules (not via
  * `Scorer.topPerTarget` and not via `Compounder`). Error *values* are compared
  * (multiset, 1e-9), not design identity, so ties are not an issue.
  *
  * Targets are derived per topology from the reference ratios so they sit
  * inside the reachable range (otherwise the novel-topology brute-force
  * window would trivially return nothing).
  *
  * Kinds (chosen so a failure can be attributed):
  *   - `sort(search)`            exhaustive stream + Scorer.sort dedup      -> B6, and any search/reference divergence
  *   - `searchTargeted best`     closest design found by the targeted path   -> B4 (+ divergences)
  *   - `searchTargeted best, positive-ratio designs only` same, ignoring negative-ratio designs -> isolates the sign bug
  *   - `Scorer.sort(searchSteps)`                                            -> B4/B6
  *   - `best()`                  Calculator.best                              -> B1 + the above
  */
class CompletenessSpec extends FunSuite:

  private val K = 5

  private def bounded(targets: Seq[Double]): SearchConfig =
    SearchConfig(
      sunTeeth = TeethBounds(8, 14),
      planetTeeth = TeethBounds(8, 10),
      ringTeeth = TeethBounds(24, 34),
      planets = TeethBounds(3, 4),
      minModule = 0.5,
      firstStageOD = 100.0,
      sunPlanetFactor = 3.0,
      targets = targets.map(Ratio(_)),
      topPerTarget = K
    )

  private def assertErrors(actual: Seq[Double], expected: Seq[Double], clue: => String): Unit =
    assertEquals(actual.size, expected.size, s"count differs: $clue\n actual=$actual\n expected=$expected")
    actual.zip(expected).foreach((a, e) => assertEqualsDouble(a, e, 1e-9, s"$clue\n actual=$actual\n expected=$expected"))

  private val known = CompletenessKnown.table

  for c <- cases.filterNot(x => x.name.contains("mode-only") && !x.name.startsWith("std")) do
    val baseCfg = c.config(bounded(Nil))
    lazy val ref = referenceDesigns(baseCfg)
    lazy val absRatios = ref.map(x => math.abs(x._3)).sorted
    def q(p: Double): Double = absRatios(math.min(absRatios.size - 1, (p * absRatios.size).toInt))
    lazy val targetSets: Vector[Seq[Double]] =
      Vector(Seq(q(0.35) + 0.137), Seq(q(0.15) - 0.071, q(0.8) + 0.213))

    test(s"completeness[${c.name}] reference is non-empty") {
      assert(ref.nonEmpty, "reference found no designs: tests would be vacuous")
    }

    for ti <- 0 to 1 do
      def cfg = c.config(bounded(targetSets(ti)))
      def ts  = targetSets(ti)
      def refTop: Vector[Double] = ref.map((_, _, r) => ts.map(err(_, r)).min).sorted.take(K)

      def reg(kind: String)(body: => Unit): Unit =
        val name = s"completeness[${c.name}/targets#$ti/$kind]"
        known.get((c.name, ti, kind)) match
          case Some(id) => test(s"$id $name".fail)(body)
          case None     => test(name)(body)

      reg("sort(search) top-K") {
        val ds  = Calculator.search(cfg).toVector
        val top = Scorer.sort(ds, Scorer.SortBy.BestFit, cfg.targets).take(K)
        assertErrors(top.map(d => Scorer.bestTargetError(cfg.targets, d.ratio)), refTop, s"sort(search) ${cfgSummary(cfg)}")
      }

      reg("searchTargeted best error") {
        val ds = Calculator.searchTargeted(cfg).toVector
        ts.foreach { t =>
          val expected = ref.map((_, _, r) => err(t, r)).min
          val got      = if ds.isEmpty then Double.PositiveInfinity else ds.map(d => err(t, d.ratio.value)).min
          assertEqualsDouble(got, expected, 1e-9, s"searchTargeted best for target $t ${cfgSummary(cfg)}")
        }
      }

      reg("searchTargeted best error, positive-ratio designs only") {
        val ds = Calculator.searchTargeted(cfg).toVector
        ts.foreach { t =>
          val pos = ref.filter(_._3 > 0).map((_, _, r) => err(t, r))
          assume(pos.nonEmpty, "no positive-ratio designs")
          val got = if ds.isEmpty then Double.PositiveInfinity else ds.map(d => err(t, d.ratio.value)).min
          // searchTargeted may return a better (negative-ratio) design; it must not be worse.
          assert(got <= pos.min + 1e-9, s"searchTargeted best $got worse than positive-only best ${pos.min} for target $t ${cfgSummary(cfg)}")
        }
      }

      reg("Scorer.sort(searchSteps) top-K") {
        val ds  = Calculator.searchSteps(cfg).flatten.toVector
        val top = Scorer.sort(ds, Scorer.SortBy.BestFit, cfg.targets).take(K)
        assertErrors(top.map(d => Scorer.bestTargetError(cfg.targets, d.ratio)), refTop, s"sort(steps) ${cfgSummary(cfg)}")
      }

      reg("best() top-K per target") {
        val got = Calculator.best(cfg)
        ts.foreach { t =>
          val expected = ref.map((_, _, r) => err(t, r)).sorted.take(K)
          assertErrors(got.get(Ratio(t)).toVector.flatten.map(_.error), expected, s"best() target=$t ${cfgSummary(cfg)}")
        }
      }

  // -- Minimal unit tests ------------------------------------------------------

  private def sampleDesigns(n: Int): Vector[CompoundDesign] =
    Calculator.search(bounded(Nil)).take(n).toVector

  // Observed: topPerTarget keeps the first-seen k designs instead of the k closest.
  test("B1 topPerTarget keeps the k smallest errors".fail) {
    val ds     = sampleDesigns(300)
    val target = Ratio(12.5)
    val k      = 3
    val got    = Scorer.topPerTarget(ds.to(LazyList), Seq(target), k)(target).map(_.error)
    val exp    = ds.map(d => Scorer.absoluteError(target, d.ratio)).sorted.take(k)
    assertErrors(got, exp, "topPerTarget")
  }

  // Observed: designs differing only in numPlanets share a canonicalKey, so Scorer.sort drops one.
  test("B6 canonicalKey distinguishes designs that differ only in numPlanets".fail) {
    val cfg = SearchConfig(mode = Mode.SingleStage, planets = TeethBounds(2, 6),
      sunTeeth = TeethBounds(8, 14), planetTeeth = TeethBounds(8, 10), ringTeeth = TeethBounds(24, 34))
    val ds  = Calculator.search(cfg).toVector
    val byTeeth = ds.groupBy(d => (d.stage1.set.sun.toInt, d.stage1.set.planet.toInt))
    val (_, group) = byTeeth.find((_, g) => g.map(_.stage1.set.numPlanets).distinct.size > 1).getOrElse(fail("no pair found"))
    val keys = group.map(Scorer.canonicalKey).distinct
    assertEquals(keys.size, group.size, s"keys collapse for N=${group.map(_.stage1.set.numPlanets)}")
  }
