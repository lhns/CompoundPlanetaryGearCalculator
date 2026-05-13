package cpgc

import cpgc.constraints.SearchConfig
import cpgc.domain.{GearSet, Mode, Teeth}
import cpgc.search.Compounder
import munit.FunSuite

class CompounderSpec extends FunSuite:

  test("compound ratio rejects identical ring/planet ratios") {
    val a = GearSet(Teeth(24), Teeth(12), 3) // ring=48, r/p=4
    val b = GearSet(Teeth(48), Teeth(24), 3) // ring=96, r/p=4
    assertEquals(Compounder.compoundRatio(a, b, Mode.Standard), None)
  }

  test("compound ratio is finite for a typical sun-driven design") {
    val a = GearSet(Teeth(20), Teeth(15), 3) // ring=50
    val b = GearSet(Teeth(22), Teeth(14), 3) // ring=50
    val r = Compounder.compoundRatio(a, b, Mode.Standard)
    assert(r.isDefined)
    assert(math.abs(r.get.value).isFinite)
  }

  test("matchModules preserves first stage outer diameter") {
    val cfg = SearchConfig(firstStageOD = 150.0)
    val a = GearSet(Teeth(20), Teeth(15), 3) // ring=50
    val b = GearSet(Teeth(22), Teeth(14), 3) // ring=50
    val (m1, _) = Compounder.matchModules(cfg, a, b)
    assertEqualsDouble(m1.value * (a.ring.toDouble + 2.0), 150.0, 1e-9)
  }

  test("CDCP mode uses unit sun-carrier factor") {
    val a = GearSet(Teeth(20), Teeth(15), 3)
    val b = GearSet(Teeth(22), Teeth(14), 3)
    val standard = Compounder.compoundRatio(a, b, Mode.Standard).get.value
    val cdcp     = Compounder.compoundRatio(a, b, Mode.CDCP).get.value
    // CDCP removes the sun-carrier divisor, so |cdcp| < |standard|.
    assert(math.abs(cdcp) < math.abs(standard))
  }
