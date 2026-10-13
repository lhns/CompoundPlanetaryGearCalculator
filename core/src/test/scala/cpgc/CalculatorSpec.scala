package cpgc

import cpgc.constraints.{SearchConfig, TeethBounds, Topology}
import cpgc.domain.{Mode, Ratio}
import munit.FunSuite

class CalculatorSpec extends FunSuite:

  test("single-stage search yields at least one design with default config") {
    val cfg = SearchConfig(
      topology        = Topology.fromMode(Mode.SingleStage),
      sunTeeth    = TeethBounds(8, 40),
      planetTeeth = TeethBounds(8, 20),
      ringTeeth   = TeethBounds(40, 100),
      planets     = TeethBounds(3, 4)
    )
    val results = Calculator.search(cfg).take(10).toVector
    assert(results.nonEmpty)
    results.foreach(d => assert(d.stage2.isEmpty))
  }

  test("compound search produces ranked best-matches per target") {
    val cfg = SearchConfig(
      topology        = Topology.fromMode(Mode.Standard),
      sunTeeth    = TeethBounds(10, 30),
      planetTeeth = TeethBounds(10, 18),
      ringTeeth   = TeethBounds(40, 80),
      planets     = TeethBounds(3, 3),
      targets     = Seq(Ratio(50.0)),
      topPerTarget = 3
    )
    val best = Calculator.best(cfg)
    assertEquals(best.size, 1)
    val scoreds = best.values.head
    assert(scoreds.nonEmpty)
    assert(scoreds.sliding(2).forall(p => p.head.error <= p.last.error))
  }

  test("determinism: identical config produces identical CSV") {
    val cfg = SearchConfig(
      topology        = Topology.fromMode(Mode.SingleStage),
      sunTeeth    = TeethBounds(10, 20),
      planetTeeth = TeethBounds(10, 14),
      ringTeeth   = TeethBounds(40, 60),
      planets     = TeethBounds(3, 3)
    )
    val a = `export`.Csv.render(Calculator.search(cfg).take(20))
    val b = `export`.Csv.render(Calculator.search(cfg).take(20))
    assertEquals(a, b)
  }
