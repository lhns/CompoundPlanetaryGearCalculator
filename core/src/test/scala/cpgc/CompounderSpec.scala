package cpgc

import cpgc.constraints.{SearchConfig, Topology}
import cpgc.domain.{GearSet, Teeth}
import cpgc.search.{Compounder, RoleSolver}
import munit.FunSuite

class CompounderSpec extends FunSuite:

  // Compound ratios are computed by the single source of truth, RoleSolver
  // (the legacy `Compounder.compoundRatio` closed form was deleted — see
  // Edge-Case Catalogue #9).

  test("compound ratio is None for a kinematically singular pair") {
    // Tp1·Tr2 − Tr1·Tp2 = 12·96 − 48·24 = 0 ⇒ output velocity 0 ⇒ no finite ratio.
    val a = GearSet(Teeth(24), Teeth(12), 3) // ring=48
    val b = GearSet(Teeth(48), Teeth(24), 3) // ring=96
    assertEquals(RoleSolver.ratio(Topology.standardCompound, a, Some(b)), None)
  }

  test("compound ratio is finite for a typical sun-driven design") {
    val a = GearSet(Teeth(20), Teeth(15), 3) // ring=50
    val b = GearSet(Teeth(22), Teeth(14), 3) // ring=50
    val r = RoleSolver.ratio(Topology.standardCompound, a, Some(b))
    assert(r.isDefined)
    assert(r.get.value.isFinite)
  }

  test("matchModules preserves first stage outer diameter") {
    val cfg = SearchConfig(firstStageOD = 150.0)
    val a = GearSet(Teeth(20), Teeth(15), 3) // ring=50
    val b = GearSet(Teeth(22), Teeth(14), 3) // ring=50
    val (m1, _) = Compounder.matchModules(cfg, a, b)
    assertEqualsDouble(m1.value * (a.ring.toDouble + 2.0), 150.0, 1e-9)
  }

  test("CDCP carrier-driven ratio differs from Standard sun-driven for the same gears") {
    val a = GearSet(Teeth(20), Teeth(15), 3)
    val b = GearSet(Teeth(22), Teeth(14), 3)
    val standard = RoleSolver.ratio(Topology.standardCompound, a, Some(b)).get.value
    val cdcp     = RoleSolver.ratio(Topology.cdcpCompound,     a, Some(b)).get.value
    // CDCP removes the sun-carrier divisor, so |cdcp| < |standard|.
    assert(math.abs(cdcp) < math.abs(standard),
      s"expected |cdcp| ($cdcp) < |standard| ($standard)")
  }
