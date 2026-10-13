package cpgc.verify

import cpgc.constraints.SearchConfig
import cpgc.domain.{GearSet, Teeth}
import cpgc.search.Compounder
import munit.FunSuite
import scala.util.Random

/** Property + golden coverage for the module derivation (ADR 0016) and
  * `Compounder.matchModules`:
  *   - `m1 = (OD − 2·ringWall) / (Tr1 + 2.5)`;
  *   - stacked planets: `m2` is scaled so both stages have the same carrier
  *     centre distance, `m2 = m1·(S1+P1)/(S2+P2)`;
  *   - otherwise `m2 = m1`;
  *   - an OD too small for the wall yields no module (never throws, bug B7).
  */
class ModuleMatchingSpec extends FunSuite:

  private val cfg = SearchConfig(firstStageOD = 150.0)

  test("both stage modules are strictly positive for every in-range gear pair") {
    val rng = new Random(11L)
    for _ <- 1 to 500 do
      val s1 = GearSet(Teeth(rng.between(8, 60)), Teeth(rng.between(8, 30)), rng.between(3, 6))
      val s2 = GearSet(Teeth(rng.between(8, 60)), Teeth(rng.between(8, 30)), rng.between(3, 6))
      for stacked <- Seq(true, false) do
        val (m1, m2) = Compounder.matchModules(cfg, s1, s2, stacked).get
        assert(m1.value > 0.0, s"m1 ${m1.value} not positive, stacked=$stacked")
        assert(m2.value > 0.0, s"m2 ${m2.value} not positive, stacked=$stacked")
  }

  test("stacked: the two stages share an equal carrier centre distance") {
    val rng = new Random(13L)
    for _ <- 1 to 300 do
      val s1 = GearSet(Teeth(rng.between(8, 60)), Teeth(rng.between(8, 30)), 3)
      val s2 = GearSet(Teeth(rng.between(8, 60)), Teeth(rng.between(8, 30)), 3)
      val (m1, m2) = Compounder.matchModules(cfg, s1, s2, stacked = true).get
      val c2c1 = m1.value * (s1.sun.toDouble + s1.planet.toDouble)
      val c2c2 = m2.value * (s2.sun.toDouble + s2.planet.toDouble)
      assertEqualsDouble(c2c1, c2c2, 1e-9, s"c2c mismatch for s1=$s1 s2=$s2")
  }

  test("not stacked: m2 == m1 (independent stage geometry)") {
    val s1 = GearSet(Teeth(24), Teeth(12), 3)
    val s2 = GearSet(Teeth(40), Teeth(10), 4)
    val (m1, m2) = Compounder.matchModules(cfg, s1, s2, stacked = false).get
    assertEqualsDouble(m2.value, m1.value, 1e-12)
  }

  test("golden: m1 = (OD − 2·wall) / (Tr1 + 2.5)") {
    // S1=24, P1=12 ⇒ R1 = 48; default wall 2 mm: m1 = (150 − 4) / 50.5.
    val s1 = GearSet(Teeth(24), Teeth(12), 3)
    val s2 = GearSet(Teeth(20), Teeth(16), 3)
    val (m1, m2) = Compounder.matchModules(cfg, s1, s2, stacked = true).get
    assertEqualsDouble(m1.value, 146.0 / 50.5, 1e-12)
    // m2 = m1 · (24 + 12) / (20 + 16) = m1.
    assertEqualsDouble(m2.value, m1.value, 1e-12)
    // zero wall: m1 = 150 / 50.5
    val (m1z, _) = Compounder.matchModules(cfg.copy(ringWall = 0.0), s1, s2, stacked = false).get
    assertEqualsDouble(m1z.value, 150.0 / 50.5, 1e-12)
  }

  test("golden: stacked m2 uses the sun-plus-planet ratio (virtual sun for a sun-less stage)") {
    val s1 = GearSet(Teeth(24), Teeth(12), 3)       // S+P = 36
    val s2 = GearSet(Teeth(10), Teeth(12), 3)       // S+P = 22 (S is the virtual sun R − 2P)
    val (m1, m2) = Compounder.matchModules(cfg.copy(ringWall = 0.0), s1, s2, stacked = true).get
    assertEqualsDouble(m2.value, m1.value * 36.0 / 22.0, 1e-12)
  }

  test("B7: an OD that leaves no room for the ring wall gives no module and does not throw") {
    val s1 = GearSet(Teeth(24), Teeth(12), 3)
    val s2 = GearSet(Teeth(20), Teeth(16), 3)
    for od <- Seq(0.0, -50.0, 4.0, Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity) do
      assertEquals(Compounder.firstModule(cfg.copy(firstStageOD = od), s1), None, s"OD=$od")
      assertEquals(Compounder.matchModules(cfg.copy(firstStageOD = od), s1, s2, stacked = true), None, s"OD=$od")
  }
