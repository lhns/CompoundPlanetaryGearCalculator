package cpgc.verify

import cpgc.Calculator
import cpgc.constraints.*
import cpgc.domain.*
import cpgc.verify.ContractSupport.*
import munit.FunSuite

import scala.util.{Failure, Success, Try}

/** User-controlled numeric inputs must never make the search throw: invalid
  * values should yield no results (or results), not an exception. */
class InputRobustnessSpec extends FunSuite:

  private val base = SearchConfig(
    sunTeeth = TeethBounds(8, 12), planetTeeth = TeethBounds(8, 10), ringTeeth = TeethBounds(24, 32),
    planets = TeethBounds(3, 4), targets = Seq(Ratio(10.0)), topPerTarget = 3
  )

  /** Exercise every public entry point to completion. */
  private def exhaust(cfg: SearchConfig): Try[Int] = Try {
    Calculator.search(cfg).size +
      Calculator.top(cfg, cpgc.search.Scorer.SortBy.Efficiency, 5).size +
      Calculator.searchSteps(cfg).flatten.size +
      Calculator.best(cfg).values.map(_.size).sum
  }

  private def noThrow(cfg: SearchConfig, label: String): Unit = exhaust(cfg) match
    case Success(_) => ()
    case Failure(e) => fail(s"$label threw ${e.getClass.getSimpleName}: ${e.getMessage}")

  for c <- cases do
    // Observed: java.lang.IllegalArgumentException "Module must be positive and finite, got 0.0" from Module.apply
    test(s"B7 firstStageOD = 0 does not throw [${c.name}]") {
      noThrow(c.config(base.copy(firstStageOD = 0.0)), "OD=0")
    }
    // Observed: IllegalArgumentException "Module must be positive and finite, got -0.xx"
    test(s"B7 firstStageOD < 0 does not throw [${c.name}]") {
      noThrow(c.config(base.copy(firstStageOD = -50.0)), "OD<0")
    }
    test(s"minModule = 0 does not throw [${c.name}]") {
      noThrow(c.config(base.copy(minModule = 0.0)), "minModule=0")
    }
    test(s"minModule < 0 does not throw [${c.name}]") {
      noThrow(c.config(base.copy(minModule = -1.0)), "minModule<0")
    }
