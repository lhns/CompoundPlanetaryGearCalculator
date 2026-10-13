package cpgc.verify

import cpgc.Calculator
import cpgc.constraints.*
import cpgc.domain.Ratio
import cpgc.verify.ContractSupport.*
import munit.FunSuite

import scala.util.Random

/** The three search entry points must agree on what a valid design is:
  *   - no targets: `search` and `searchSteps` emit identical design sets;
  *   - with targets: everything `searchTargeted` / `searchSteps` emits is also
  *     emitted by the exhaustive `search`.
  * Designs are keyed by teeth + planet count of both stages.
  */
class CrossPathSpec extends FunSuite:

  private val NConfigs = 8

  private def keys(ds: Iterable[cpgc.domain.CompoundDesign]): Set[Key] = ds.map(key).toSet

  private val IdKind  = "identical-stage pairs (a,a) accepted by compound() are searched"
  private val NegKind  = "negative-ratio designs are found when targeting |ratio|"

  /** (case, kind) -> bug ids for expected failures (observed failures, attributed by cluster analysis). */
  private val known: Map[(String, String), String] =
    val sign = (for c <- Vector("std[topology]", "cdcp[topology]", "nss[topology]",
                  "PS+CC std-roles", "CC+RingToRing C1fixed", "CC+SunToSun std-roles", "NSS-like (C1 fixed)",
                  "CDCP sun-less (both)", "CDCP S1-absent") yield (c, NegKind) -> "B4") ++ Vector(
      ("single S1->R1 C1fixed", NegKind) -> "B4")
    val ident = for c <- Vector("CC+RingToSun std-roles", "SunToCarrier+RingToSun", "CC+SunToRing std-roles",
                  "CC+RingToRing C1fixed", "CDCP-like (S1 fixed)") yield (c, IdKind) -> "B4"
    (sign ++ ident).toMap

  for c <- cases do
    def reg(kind: String)(body: => Unit): Unit =
      val name = s"crosspath[${c.name}/$kind]"
      known.get((c.name, kind)) match
        case Some(id) => test(s"$id $name".fail)(body)   // observed: expected design set missing from the targeted/steps output
        case None     => test(name)(body)

    reg("no targets: search == searchSteps") {
      val rng = new Random(777L)
      for _ <- 0 until NConfigs do
        val cfg = c.config(randomConfig(rng, withTargets = false))
        val a   = keys(Calculator.search(cfg))
        val b   = keys(Calculator.searchSteps(cfg).flatten.toVector)
        assertEquals(b -- a, Set.empty[Key], s"steps-only designs ${cfgSummary(cfg)}")
        assertEquals(a -- b, Set.empty[Key], s"search-only designs ${cfgSummary(cfg)}")
    }

    reg("targets: searchTargeted subset of search") {
      val rng = new Random(888L)
      for _ <- 0 until NConfigs do
        val cfg = c.config(randomConfig(rng, withTargets = true))
        val all = keys(Calculator.search(cfg))
        val tg  = keys(Calculator.searchTargeted(cfg))
        assertEquals(tg -- all, Set.empty[Key], s"targeted-only designs ${cfgSummary(cfg)}")
    }

    reg("targets: searchSteps subset of search") {
      val rng = new Random(889L)
      for _ <- 0 until NConfigs do
        val cfg = c.config(randomConfig(rng, withTargets = true))
        val all = keys(Calculator.search(cfg))
        val st  = keys(Calculator.searchSteps(cfg).flatten.toVector)
        assertEquals(st -- all, Set.empty[Key], s"steps-only designs ${cfgSummary(cfg)}")
    }

    // (a,a) pairs are meaningless for single-stage topologies on the single path
    if c.topology.stages == 2 then reg("identical-stage pairs (a,a) accepted by compound() are searched") {
      val rng = new Random(901L)
      for _ <- 0 until NConfigs do
        val cfg  = c.config(randomConfig(rng, withTargets = false))
        val all  = keys(Calculator.search(cfg))
        val pool2 = cpgc.search.Enumerator.stage2Sets(cfg).toSet
        val same  = cpgc.search.Enumerator.stage1Sets(cfg).filter(pool2.contains)
          .flatMap(a => cpgc.search.Compounder.compound(cfg, a, a)).toVector
        assertEquals(keys(same) -- all, Set.empty[Key], s"(a,a) designs missing from search ${cfgSummary(cfg)}")
    }

    reg("negative-ratio designs are found when targeting |ratio|") {
      val rng = new Random(902L)
      var sawAny = false
      for _ <- 0 until NConfigs do
        val cfg = c.config(randomConfig(rng, withTargets = false))
        val neg = Calculator.search(cfg).filter(d => d.stage2.isDefined && d.ratio.value < -1e-6).take(6).toVector
        sawAny ||= neg.nonEmpty
        neg.foreach { d =>
          val tcfg = cfg.copy(targets = Seq(Ratio(math.abs(d.ratio.value))))
          assert(keys(Calculator.searchTargeted(tcfg)).contains(key(d)), s"missed ${fmt(d)} ${cfgSummary(tcfg)}")
        }
      assume(sawAny, "topology produced no negative-ratio designs")
    }

    reg("positive-ratio designs with N1==N2 are found when targeting their ratio (control)") {
      val rng = new Random(903L)
      for _ <- 0 until NConfigs do
        val cfg = c.config(randomConfig(rng, withTargets = false))
        val pos = Calculator.search(cfg)
          .filter(d => d.stage2.exists(_.set.numPlanets == d.stage1.set.numPlanets) && d.ratio.value > 1e-6)
          .take(6).toVector
        pos.foreach { d =>
          val tcfg = cfg.copy(targets = Seq(d.ratio))
          assert(keys(Calculator.searchTargeted(tcfg)).contains(key(d)), s"missed ${fmt(d)} ${cfgSummary(tcfg)}")
        }
    }
