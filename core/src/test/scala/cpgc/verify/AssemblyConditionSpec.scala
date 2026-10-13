package cpgc.verify

import cpgc.Calculator
import cpgc.constraints.{Body, BodyRole, SearchConfig, TeethBounds, Topology}
import cpgc.domain.{GearSet, Teeth}
import cpgc.search.{Compounder, Enumerator, RoleSolver, Scorer}
import cpgc.verify.AssemblyOracle.{B, Pinion, Stage, Weld}
import munit.FunSuite

/** Planet assembly (equal spacing) conditions, checked against the independent
  * tooth-phase oracle in `AssemblyOracle`. See
  * `docs/verification/assembly-conditions.md` for the derivation.
  *
  * Verified rule for N equally spaced planets:
  *  - every stage with a sun: `(Ts + Tr) % N == 0`;
  *  - stacked pinions that are identical across planets (any fixed offset
  *    between the halves), additionally:
  *    `((Tp1·Tr2 − Tp2·Tr1) / gcd(Tp1, Tp2)) % N == 0`, with or without suns.
  *
  * Since ADR 0018 the production predicate (`Compounder.satisfiesSpacing`)
  * enforces the second condition unless `phasedPinions` is set; the tests below
  * check it against the oracle both ways. `currentRule` is the pre-ADR-0018
  * rule, kept for the historical counterexamples.
  */
class AssemblyConditionSpec extends FunSuite:
  import AssemblyConditionSpec.*

  // The diagnostic wide-range run takes about a minute on the JVM.
  override val munitTimeout = scala.concurrent.duration.Duration(10, "min")

  override def munitTestTransforms = super.munitTestTransforms ++ List(
    new TestTransform("diagnostic", t =>
      if t.name.startsWith("diagnostic") then t.withTags(t.tags + munit.Tag("diagnostic")) else t)
  )

  private val g0      = Rat.Zero        // tooth centres of both halves on one radial line
  private val gOdd    = Rat.of(5, 37)   // an arbitrary non-aligned offset
  private val offsets = Seq(g0, gOdd)

  // ── Oracle sanity ─────────────────────────────────────────────────────────

  test("oracle: textbook single stages") {
    assert(AssemblyOracle.single(Stage.std(20, 15), 5))   // 70 % 5 == 0
    assert(!AssemblyOracle.single(Stage.std(20, 14), 3))  // 68 % 3 != 0
    assert(AssemblyOracle.single(Stage.std(20, 14), 4))   // 68 % 4 == 0
    assert(AssemblyOracle.single(Stage(None, 14, 47), 5)) // no sun: always
  }

  test("oracle: verdict does not depend on the pinned planet-0 angle") {
    for (a, b, n) <- stackedCases(6, 8); variant <- Variants; g <- offsets do
      val (s1, s2) = variant.apply(a, b)
      val ref      = AssemblyOracle.stacked(Seq(s1, s2), n, Pinion.Identical(g))
      for x0 <- Seq(Rat.of(1, 3), Rat.of(7, 19)) do
        assertEquals(AssemblyOracle.stacked(Seq(s1, s2), n, Pinion.Identical(g), x0), ref,
          s"$variant $a $b N=$n g=$g x0=$x0")
  }

  test("oracle: stacked verdict does not depend on the pinion half offset") {
    for (a, b, n) <- stackedCases(6, 9); variant <- Variants do
      val (s1, s2) = variant.apply(a, b)
      val gs  = Seq(g0, gOdd, Rat.of(1, 2 * s2.planet), Rat.of(1, 3 * s2.planet), Rat.of(11, 13))
      val res = gs.map(g => AssemblyOracle.stacked(Seq(s1, s2), n, Pinion.Identical(g)))
      assert(res.distinct.size == 1, s"$variant $a $b N=$n: $res")
  }

  // ── Single stage ──────────────────────────────────────────────────────────

  test("single stage: oracle == (Ts + Tr) % N == 0 == GearSet.evenSpacing") {
    for ts <- 6 to 30; tp <- 6 to 30; n <- 2 to 6 do
      val st = Stage.std(ts, tp)
      val o  = AssemblyOracle.single(st, n)
      assertEquals(o, sumOk(st, n), s"$st N=$n")
      assertEquals(o, GearSet(Teeth(ts), Teeth(tp), n).evenSpacing, s"$st N=$n")
  }

  test("single stage without sun: always assemblable") {
    for ts <- 6 to 30; tp <- 6 to 30; n <- 2 to 6 do
      assert(AssemblyOracle.single(Stage.std(ts, tp).withoutSun, n))
  }

  // ── Stacked (stepped) planets ─────────────────────────────────────────────

  for variant <- Variants do
    test(s"stacked $variant, identical pinions: oracle == verified rule") {
      for (a, b, n) <- stackedCases(Lo, Hi); g <- offsets do
        val (s1, s2) = variant.apply(a, b)
        assertEquals(AssemblyOracle.stacked(Seq(s1, s2), n, Pinion.Identical(g)),
          stackedRule(s1, s2, n, identical = true), s"$s1 / $s2 N=$n g=$g")
    }

    test(s"stacked $variant, individually phased pinions: oracle == per-stage rule") {
      for (a, b, n) <- stackedCases(Lo, Hi) do
        val (s1, s2) = variant.apply(a, b)
        assertEquals(AssemblyOracle.stacked(Seq(s1, s2), n, Pinion.Individual),
          stackedRule(s1, s2, n, identical = false), s"$s1 / $s2 N=$n")
        assertEquals(stackedRule(s1, s2, n, identical = false), currentRule(s1, s2, n))
    }

  // Production predicate (ADR 0018) vs the oracle, for every stacked sun variant.
  for variant <- Variants do
    test(s"production assembly rule == oracle, stacked $variant, identical pinions (phasedPinions = false)") {
      for (a, b, n) <- stackedCases(Lo, Hi) do
        val (s1, s2) = variant.apply(a, b)
        assertEquals(productionRule(variant, s1, s2, n, phased = false),
          AssemblyOracle.stacked(Seq(s1, s2), n, Pinion.Identical(g0)), s"$s1 / $s2 N=$n")
    }

    test(s"production assembly rule == oracle, stacked $variant, individually phased pinions (phasedPinions = true)") {
      for (a, b, n) <- stackedCases(Lo, Hi) do
        val (s1, s2) = variant.apply(a, b)
        assertEquals(productionRule(variant, s1, s2, n, phased = true),
          AssemblyOracle.stacked(Seq(s1, s2), n, Pinion.Individual), s"$s1 / $s2 N=$n")
    }

  // Engine output (PairSearch + Evaluator, the production path) vs the exact oracle, on small ranges.
  for variant <- Variants; phased <- Seq(false, true) do
    test(s"engine output == oracle-assemblable pairs, stacked $variant, phasedPinions = $phased") {
      val cfg = SearchConfig(
        topology = topologyOf(variant), sunTeeth = TeethBounds(6, 11), planetTeeth = TeethBounds(6, 9),
        ringTeeth = TeethBounds(18, 30), planets = TeethBounds(2, 4), minModule = 0.0,
        planetClearance = 0.0, sunPlanetFactor = 10.0, phasedPinions = phased)
      def oracleStage(st: cpgc.domain.Stage, sunPresent: Boolean) =
        Stage(Option.when(sunPresent)(st.set.sun.toInt), st.set.planet.toInt, st.set.ring.toInt)
      def oracle(s1: Stage, s2: Stage, n: Int) =
        AssemblyOracle.stacked(Seq(s1, s2), n, if phased then Pinion.Individual else Pinion.Identical(g0))
      val found = Calculator.search(cfg).toVector
      assert(found.nonEmpty)
      for d <- found do
        val (a, b) = (d.stage1.set, d.stage2.get.set)
        assertEquals(a.numPlanets, b.numPlanets)
        assert(oracle(oracleStage(d.stage1, variant.sun1), oracleStage(d.stage2.get, variant.sun2), a.numPlanets),
          s"engine accepted an unassemblable design: $a / $b")
      // and nothing assemblable that the enumerator offers is missing
      val keys = found.map(d => Scorer.dedupKey(d.stage1.set, d.stage2.get.set)).toSet
      for
        a <- Enumerator.stage1Sets(cfg); b <- Enumerator.stage2Sets(cfg)
        if a.numPlanets == b.numPlanets && RoleSolver.ratio(cfg.topology, a, Some(b)).isDefined // solvable (not degenerate)
      do
        val ok = oracle(
          Stage(Option.when(variant.sun1)(a.sun.toInt), a.planet.toInt, a.ring.toInt),
          Stage(Option.when(variant.sun2)(b.sun.toInt), b.planet.toInt, b.ring.toInt), a.numPlanets)
        assertEquals(keys.contains(Scorer.dedupKey(a, b)), ok, s"$a / $b")
    }

  test("minimal counterexample: Tp1 = Tp2 = 10, Ts 20 / 21, N = 2") {
    val a = Stage.std(20, 10) // R1 = 40
    val b = Stage.std(21, 10) // R2 = 41
    for (variant, s1, s2) <- Seq(
           ("Standard", a, b), ("NSS", a, b.withoutSun), ("sun-less", a.withoutSun, b.withoutSun))
    do
      assert(currentRule(s1, s2, 2), variant)
      assert(!AssemblyOracle.stacked(Seq(s1, s2), 2, Pinion.Identical(g0)), variant)
      assert(AssemblyOracle.stacked(Seq(s1, s2), 2, Pinion.Individual), variant)
  }

  test("counterexamples: a Standard design assembles, an NSS design the old rule accepted does not") {
    val a1 = Stage.std(19, 17); val a2 = Stage.std(19, 14)
    assert(AssemblyOracle.stacked(Seq(a1, a2), 3, Pinion.Identical(g0)))
    val b1 = Stage.std(53, 11); val b2 = Stage.std(72, 14).withoutSun
    assert(currentRule(b1, b2, 4))
    assert(!AssemblyOracle.stacked(Seq(b1, b2), 4, Pinion.Identical(g0)))
    assert(!stackedRule(b1, b2, 4, identical = true))
  }

  // ── Non-stacked: stages joined by two central welds ───────────────────────

  test("non-stacked (all 18 weld pairs): oracle == per-stage rule, any weld offsets") {
    for
      welds         <- WeldPairs
      (sun1, sun2)  <- Seq((true, true), (true, false), (false, true))
      if (sun1 || !welds.exists(_.a == B.S)) && (sun2 || !welds.exists(_.b == B.S))
      ts1 <- 6 to 8; tp1 <- 6 to 8; ts2 <- 6 to 8; tp2 <- 6 to 8
      n1 <- 2 to 4; n2 <- 2 to 4
    do
      val s1 = if sun1 then Stage.std(ts1, tp1) else Stage.std(ts1, tp1).withoutSun
      val s2 = if sun2 then Stage.std(ts2, tp2) else Stage.std(ts2, tp2).withoutSun
      val rule = sumOk(s1, n1) && sumOk(s2, n2)
      assertEquals(AssemblyOracle.nonStacked(s1, n1, s2, n2, welds), rule,
        s"$welds $s1 N1=$n1 / $s2 N2=$n2")
      // Necessity, independently of the constructive search: a compound can only
      // be assembled if each stage can be assembled on its own.
      if !rule then assert(!AssemblyOracle.single(s1, n1) || !AssemblyOracle.single(s2, n2))
  }

  // ── Diagnostic: wide-range oracle runs and counts for the doc ─────────────
  // Excluded by default (slow). Run with:
  //   sbt "project coreJVM" "set Test / testOptions := Seq()" "testOnly cpgc.verify.AssemblyConditionSpec -- *diagnostic*"

  test("diagnostic: wide-range oracle, production rule false accepts") {
    for variant <- Variants do
      var cases, verified, production, falseAccepts = 0
      var minimal: Option[(Int, Int, String)] = None
      for (a, b, n) <- stackedCases(6, 16) do
        val (s1, s2) = variant.apply(a, b)
        val o = AssemblyOracle.stacked(Seq(s1, s2), n, Pinion.Identical(g0))
        assertEquals(AssemblyOracle.stacked(Seq(s1, s2), n, Pinion.Identical(gOdd)), o)
        assertEquals(o, stackedRule(s1, s2, n, identical = true), s"$s1 / $s2 N=$n")
        assertEquals(AssemblyOracle.stacked(Seq(s1, s2), n, Pinion.Individual),
          stackedRule(s1, s2, n, identical = false), s"$s1 / $s2 N=$n")
        val r = currentRule(s1, s2, n)
        assert(o <= r, "production rule rejects an assemblable design")
        cases += 1
        if o then verified += 1
        if r then production += 1
        if r && !o then
          falseAccepts += 1
          val key = (n, a._1 + a._2 + b._1 + b._2, s"$s1 / $s2 N=$n")
          if minimal.forall(m => key._1 < m._1 || (key._1 == m._1 && key._2 < m._2)) then minimal = Some(key)
      println(f"teeth 6..16 $variant%-8s cases=$cases%6d oracle-ok=$verified%6d production-ok=$production%6d " +
        f"false-accepts=$falseAccepts%6d minimal=${minimal.map(_._3).getOrElse("-")}")
    var nsCases = 0
    for
      welds         <- WeldPairs
      (sun1, sun2)  <- Seq((true, true), (true, false), (false, true))
      if (sun1 || !welds.exists(_.a == B.S)) && (sun2 || !welds.exists(_.b == B.S))
      ts1 <- 6 to 10; tp1 <- 6 to 10; ts2 <- 6 to 10; tp2 <- 6 to 10
      n1 <- 2 to 5; n2 <- 2 to 5
    do
      val s1 = if sun1 then Stage.std(ts1, tp1) else Stage.std(ts1, tp1).withoutSun
      val s2 = if sun2 then Stage.std(ts2, tp2) else Stage.std(ts2, tp2).withoutSun
      assertEquals(AssemblyOracle.nonStacked(s1, n1, s2, n2, welds), sumOk(s1, n1) && sumOk(s2, n2))
      nsCases += 1
    println(s"non-stacked teeth 6..10, N 2..5, 18 weld pairs: $nsCases cases, 0 disagreements")
  }

object AssemblyConditionSpec:

  /** Teeth range of the exhaustive stacked tests (kept small for Scala.js). */
  val Lo = 6
  val Hi = 11

  def gcd(a: Int, b: Int): Int = if b == 0 then math.abs(a) else gcd(b, a % b)

  /** Every stage with a sun: `(Ts + Tr) % N == 0`. */
  def sumOk(st: Stage, n: Int): Boolean = st.sun.forall(ts => (ts + st.ring) % n == 0)

  /** Stacked pinions identical across planets: `(Tp1·Tr2 − Tp2·Tr1)/gcd(Tp1,Tp2) ≡ 0 (mod N)`. */
  def crossOk(s1: Stage, s2: Stage, n: Int): Boolean =
    ((s1.planet * s2.ring - s2.planet * s1.ring) / gcd(s1.planet, s2.planet)) % n == 0

  /** The verified rule for a stacked compound. */
  def stackedRule(s1: Stage, s2: Stage, n: Int, identical: Boolean): Boolean =
    sumOk(s1, n) && sumOk(s2, n) && (!identical || crossOk(s1, s2, n))

  /** The pre-ADR-0018 rule (ADR 0012 / 0015): each stage whose sun is present
    * must satisfy `GearSet.evenSpacing`; sun-less stages impose nothing. */
  def currentRule(s1: Stage, s2: Stage, n: Int): Boolean =
    Seq(s1, s2).forall(st => st.sun.forall(ts => GearSet(Teeth(ts), Teeth(st.planet), n).evenSpacing))

  /** Which suns a stacked compound has. Rings use the calculator's convention
    * `R = S + 2P` (a virtual sun for a sun-less stage). */
  enum Variant(val sun1: Boolean, val sun2: Boolean):
    case Standard extends Variant(true, true)
    case Nss      extends Variant(true, false)
    case NoSun1   extends Variant(false, true)
    case Sunless  extends Variant(false, false)
    def apply(a: (Int, Int), b: (Int, Int)): (Stage, Stage) =
      def mk(t: (Int, Int), sun: Boolean) =
        val st = Stage.std(t._1, t._2)
        if sun then st else st.withoutSun
      (mk(a, sun1), mk(b, sun2))

  val Variants: Seq[Variant] = Variant.values.toSeq

  private val noSun1Compound: Topology = Topology(2, Map(
    Body.S1 -> BodyRole.Absent, Body.C1 -> BodyRole.Input, Body.R1 -> BodyRole.Fixed,
    Body.S2 -> BodyRole.Free, Body.C2 -> BodyRole.Free, Body.R2 -> BodyRole.Output))

  /** The stacked topology with the variant's suns. */
  def topologyOf(v: Variant): Topology = v match
    case Variant.Standard => Topology.standardCompound
    case Variant.Nss      => Topology.nssCompound
    case Variant.NoSun1   => noSun1Compound
    case Variant.Sunless  => Topology.sunlessCdcpCompound

  /** `Compounder.satisfiesSpacing` on the oracle's stages (virtual suns `R − 2P`). */
  def productionRule(v: Variant, s1: Stage, s2: Stage, n: Int, phased: Boolean): Boolean =
    def gs(st: Stage) = GearSet(Teeth(st.ring - 2 * st.planet), Teeth(st.planet), n)
    Compounder.satisfiesSpacing(topologyOf(v), phased, gs(s1), Some(gs(s2)))

  /** All `((Ts1, Tp1), (Ts2, Tp2), N)` with teeth in `lo..hi` and N in 2..6. */
  def stackedCases(lo: Int, hi: Int): Iterator[((Int, Int), (Int, Int), Int)] =
    for
      ts1 <- Iterator.range(lo, hi + 1); tp1 <- Iterator.range(lo, hi + 1)
      ts2 <- Iterator.range(lo, hi + 1); tp2 <- Iterator.range(lo, hi + 1)
      n   <- Iterator.range(2, 7)
    yield ((ts1, tp1), (ts2, tp2), n)

  /** Two welds joining distinct stage-1 bodies to distinct stage-2 bodies, with
    * arbitrary fixed offsets: the 18 non-stacked compounds. */
  val WeldPairs: Seq[Seq[Weld]] =
    val all = for a <- B.values.toSeq; b <- B.values.toSeq yield (a, b)
    for
      i <- all.indices; j <- all.indices if i < j
      (a1, b1) = all(i); (a2, b2) = all(j)
      if a1 != a2 && b1 != b2
    yield Seq(Weld(a1, b1, Rat.of(3, 17)), Weld(a2, b2, Rat.of(2, 11)))
