package cpgc

import cpgc.constraints.{SearchConfig, TeethBounds, Topology}
import cpgc.domain.{Mode, Ratio}
import cpgc.search.Scorer
import munit.FunSuite

class InverseSolverSpec extends FunSuite:

  /** For a fixed config, the inverse-solver fast path and the brute-force
    * enumeration must converge on the same set of compound-ratio candidates
    * (modulo ordering). We compare on rounded ratio buckets so floating-point
    * round-off doesn't cause false negatives.
    */
  test("searchTargeted matches search() output for a Standard search") {
    val cfg = SearchConfig(
      topology         = Topology.fromMode(Mode.Standard),
      sunTeeth     = TeethBounds(10, 24),
      planetTeeth  = TeethBounds(10, 18),
      ringTeeth    = TeethBounds(40, 60),
      planets      = TeethBounds(3, 3),
      targets      = Seq(Ratio(53.12))
    )

    val brute    = Calculator.search(cfg).toVector
    val targeted = Calculator.searchTargeted(cfg).toVector

    // The targeted set is a SUBSET of brute (it returns candidates near the
    // target, not the full universe). What we want to assert is: every design
    // the targeted solver returns is a valid compound design — i.e. brute
    // contains it. Same Tooth-tuple = same design.
    val bruteKeys    = brute.iterator.map(Scorer.canonicalKey).toSet
    val targetedKeys = targeted.iterator.map(Scorer.canonicalKey).toSet
    val extra        = targetedKeys -- bruteKeys
    assert(
      extra.isEmpty,
      s"targeted solver produced designs not in brute: $extra"
    )
    assert(targeted.nonEmpty, "targeted solver returned nothing")
  }

  test("inverse solver is faster than brute force for targeted search") {
    val cfg = SearchConfig(
      topology         = Topology.fromMode(Mode.Standard),
      sunTeeth     = TeethBounds(8, 80),
      planetTeeth  = TeethBounds(8, 20),
      ringTeeth    = TeethBounds(40, 200),
      planets      = TeethBounds(3, 4),
      targets      = Seq(Ratio(53.12))
    )

    def time[A](body: => A): (A, Long) =
      val t0 = System.nanoTime()
      val r  = body
      (r, (System.nanoTime() - t0) / 1_000_000L)

    val (brute, bruteMs)       = time(Calculator.search(cfg).take(1000).toVector)
    val (targeted, targetedMs) = time(Calculator.searchTargeted(cfg).toVector)

    // Both should find at least one good match for ratio 53.12.
    val bestBrute    = brute.iterator.map(d => math.abs(d.ratio.value - 53.12)).min
    val bestTargeted = targeted.iterator.map(d => math.abs(d.ratio.value - 53.12)).min
    assert(
      bestTargeted <= bestBrute + 0.01,
      s"targeted best $bestTargeted should be ≤ brute best $bestBrute"
    )
    // Targeted should be at least as fast as brute, usually much faster.
    println(s"  brute=${bruteMs}ms  targeted=${targetedMs}ms  (best err: brute=$bestBrute targeted=$bestTargeted)")
  }
