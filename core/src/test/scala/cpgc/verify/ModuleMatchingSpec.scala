package cpgc.verify

import cpgc.constraints.{Coupling, SearchConfig}
import cpgc.domain.{GearSet, Teeth}
import cpgc.search.Compounder
import munit.FunSuite
import scala.util.Random

/** Property + golden coverage for `Compounder.matchModules` — previously a
  * tier-D (no coverage) component.
  *
  * `matchModules` picks the module (mm-per-tooth) for each stage:
  *   - `PlanetStack` — `m2` is scaled so both stages have the same carrier
  *     center-to-center distance (a single stacked pinion must bridge them).
  *   - every other coupling — `m2 = m1` (the stages' geometries are independent).
  */
class ModuleMatchingSpec extends FunSuite:

  private val cfg = SearchConfig(firstStageOD = 150.0)

  test("both stage modules are strictly positive for every in-range gear pair") {
    val rng = new Random(11L)
    for _ <- 1 to 500 do
      val s1 = GearSet(Teeth(rng.between(8, 60)), Teeth(rng.between(8, 30)), rng.between(3, 6))
      val s2 = GearSet(Teeth(rng.between(8, 60)), Teeth(rng.between(8, 30)), rng.between(3, 6))
      for c <- Coupling.values do
        val (m1, m2) = Compounder.matchModules(cfg, s1, s2, c)
        assert(m1.value > 0.0, s"m1 ${m1.value} not positive, coupling=$c")
        assert(m2.value > 0.0, s"m2 ${m2.value} not positive, coupling=$c")
  }

  test("PlanetStack: the two stages share an equal carrier center-to-center distance") {
    // c2c (in mm) = module · (sun + planet) / ... — the proportional part is
    // module · (sun + planet); matchModules makes module1·c2c1 == module2·c2c2.
    val rng = new Random(13L)
    for _ <- 1 to 300 do
      val s1 = GearSet(Teeth(rng.between(8, 60)), Teeth(rng.between(8, 30)), 3)
      val s2 = GearSet(Teeth(rng.between(8, 60)), Teeth(rng.between(8, 30)), 3)
      val (m1, m2) = Compounder.matchModules(cfg, s1, s2, Coupling.PlanetStack)
      val c2c1 = m1.value * (s1.sun.toDouble + s1.planet.toDouble)
      val c2c2 = m2.value * (s2.sun.toDouble + s2.planet.toDouble)
      assertEqualsDouble(c2c1, c2c2, 1e-9, s"c2c mismatch for s1=$s1 s2=$s2")
  }

  test("non-stacked couplings: m2 == m1 (independent stage geometry)") {
    val s1 = GearSet(Teeth(24), Teeth(12), 3)
    val s2 = GearSet(Teeth(40), Teeth(10), 4)
    for c <- Seq(Coupling.RingToSun, Coupling.SunToRing, Coupling.RingToRing,
                 Coupling.SunToSun, Coupling.CarrierToSun, Coupling.CarrierToRing) do
      val (m1, m2) = Compounder.matchModules(cfg, s1, s2, c)
      assertEqualsDouble(m2.value, m1.value, 1e-12, s"coupling $c should leave m2 == m1")
  }

  test("golden: m1 is firstStageOD / (ring1 + 2)") {
    // S1=24, P1=12 ⇒ R1 = 48; m1 = 150 / (48 + 2) = 3.0.
    val s1 = GearSet(Teeth(24), Teeth(12), 3)
    val s2 = GearSet(Teeth(20), Teeth(16), 3)
    val (m1, _) = Compounder.matchModules(cfg, s1, s2, Coupling.PlanetStack)
    assertEqualsDouble(m1.value, 3.0, 1e-12)
  }
