package cpgc

import cpgc.domain.{GearSet, Teeth}
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class GearSetSpec extends ScalaCheckSuite:

  private val sunGen    = Gen.choose(8, 80)
  private val planetGen = Gen.choose(8, 20)
  private val nGen      = Gen.choose(3, 6)

  property("ring teeth = sun + 2*planet") {
    forAll(sunGen, planetGen, nGen) { (s, p, n) =>
      val set = GearSet(Teeth(s), Teeth(p), n)
      set.ring.toInt == s + 2 * p
    }
  }

  property("evenSpacing implies (sun + ring) divisible by N") {
    forAll(sunGen, planetGen, nGen) { (s, p, n) =>
      val set = GearSet(Teeth(s), Teeth(p), n)
      !set.evenSpacing || (set.sun.toInt + set.ring.toInt) % n == 0
    }
  }

  test("sunCarrierRatio for classic 24/12/48 stage is 1/3") {
    val set = GearSet(Teeth(24), Teeth(12), 3)
    assertEqualsDouble(set.sunCarrierRatio, 24.0 / 72.0, 1e-12)
  }
