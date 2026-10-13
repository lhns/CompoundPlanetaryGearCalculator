package cpgc

import cpgc.domain.{GearSet, Teeth}
import cpgc.physics.PlanetSpacing
import munit.FunSuite

class PlanetSpacingSpec extends FunSuite:

  test("3 planets fit comfortably with 24/12 teeth") {
    val set = GearSet(Teeth(24), Teeth(12), 3)
    assert(PlanetSpacing.planetsFitWithoutOverlap(set, 0.0))
  }

  test("8 planets do not fit with 12/20 teeth (planet OD > chord)") {
    val set = GearSet(Teeth(12), Teeth(20), 8)
    assert(!PlanetSpacing.planetsFitWithoutOverlap(set, 0.0))
  }
