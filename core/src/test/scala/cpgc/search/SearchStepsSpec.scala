package cpgc.search

import cpgc.Calculator
import cpgc.constraints.{Body, BodyRole, SearchConfig, TeethBounds, Topology}
import cpgc.domain.{CompoundDesign, Mode, Ratio}
import munit.FunSuite

/** Round-trip equivalence between the step-grained `Calculator.searchSteps`
  * iterator and the existing LazyList APIs (`Calculator.search`,
  * `Calculator.best`). They share the same candidate space and the
  * same per-pair `Compounder.compound` decisions; the step iterator only
  * changes the **rate** at which work is exposed to the caller. Drain both
  * surfaces and compare canonical-key sets. */
class SearchStepsSpec extends FunSuite:

  private def drain(cfg: SearchConfig): Vector[CompoundDesign] =
    val it = Calculator.searchSteps(cfg)
    val buf = Vector.newBuilder[CompoundDesign]
    while it.hasNext do it.next().foreach(buf.+=)
    buf.result()

  private def keys(ds: Iterable[CompoundDesign]): Set[?] =
    ds.iterator.map(Scorer.canonicalKey).toSet

  private def baseConfig(mode: Mode): SearchConfig = SearchConfig(
    topology    = Topology.fromMode(mode),
    sunTeeth    = TeethBounds(10, 20),
    planetTeeth = TeethBounds(10, 14),
    ringTeeth   = TeethBounds(40, 60),
    planets     = TeethBounds(3, 3)
  )

  test("searchSteps == search for SingleStage") {
    val cfg = baseConfig(Mode.SingleStage)
    assertEquals(keys(drain(cfg)), keys(Calculator.search(cfg)))
  }

  test("searchSteps == search for Standard (no targets)") {
    val cfg = baseConfig(Mode.Standard)
    assertEquals(keys(drain(cfg)), keys(Calculator.search(cfg)))
  }

  test("searchSteps == search for CDCP (no targets)") {
    val cfg = baseConfig(Mode.CDCP)
    assertEquals(keys(drain(cfg)), keys(Calculator.search(cfg)))
  }

  test("searchSteps == search for NSS (no targets)") {
    val cfg = baseConfig(Mode.NSS)
    assertEquals(keys(drain(cfg)), keys(Calculator.search(cfg)))
  }

  test("targets don't change the candidate space (one engine for all searches)") {
    val cfg = baseConfig(Mode.Standard).copy(targets = Seq(Ratio(40.0), Ratio(80.0)))
    assertEquals(keys(drain(cfg)), keys(Calculator.search(cfg.copy(targets = Seq.empty))))
  }

  test("searchSteps for a novel topology == search") {
    // A non-legacy role tuple: ring1 as input, S2 as output, carrier fixed.
    val novel = Topology(
      stages = 2,
      roles  = Map(
        Body.S1 -> BodyRole.Free,
        Body.R1 -> BodyRole.Input,
        Body.C1 -> BodyRole.Fixed,
        Body.S2 -> BodyRole.Output,
        Body.C2 -> BodyRole.Free,
        Body.R2 -> BodyRole.Free
      )
    )
    assume(novel.legacyMode.isEmpty, "test setup expects a non-legacy topology")
    val cfg = SearchConfig(
      topology    = novel,
      sunTeeth    = TeethBounds(10, 18),
      planetTeeth = TeethBounds(10, 14),
      ringTeeth   = TeethBounds(40, 56),
      planets     = TeethBounds(3, 3),
      targets     = Seq(Ratio(5.0)) // useStage2For=false because legacyMode is None
    )
    assertEquals(keys(drain(cfg)), keys(Calculator.search(cfg)))
  }
