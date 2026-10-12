package cpgc.verify

import cpgc.constraints.Coupling
import cpgc.domain.{GearSet, Mode, Teeth}
import cpgc.physics.Efficiency
import munit.FunSuite
import scala.util.Random

/** Property + golden coverage for `Efficiency.overall` — previously a tier-D
  * (no coverage) component. The efficiency model is a coarse mesh-count
  * estimate (each mesh ≈ 0.985), so the tests pin its *structural* properties
  * and snapshot its exact values rather than claiming dyno-grade accuracy.
  */
class EfficiencySpec extends FunSuite:

  private val s1 = GearSet(Teeth(24), Teeth(12), 3)
  private val s2 = GearSet(Teeth(20), Teeth(16), 3)

  test("efficiency is always in (0, 1] for every mode and coupling") {
    val rng = new Random(7L)
    for _ <- 1 to 500 do
      val a = GearSet(Teeth(rng.between(8, 60)), Teeth(rng.between(8, 30)), rng.between(3, 6))
      val b = GearSet(Teeth(rng.between(8, 60)), Teeth(rng.between(8, 30)), rng.between(3, 6))
      for
        mode <- Mode.values
        c    <- Coupling.values
      do
        val e = Efficiency.overall(a, Some(b), mode, c)
        assert(e > 0.0 && e <= 1.0, s"efficiency $e out of (0,1] for mode=$mode coupling=$c")
  }

  test("single-stage efficiency exceeds compound efficiency (fewer meshes)") {
    val single   = Efficiency.overall(s1, None, Mode.SingleStage, Coupling.PlanetStack)
    val compound = Efficiency.overall(s1, Some(s2), Mode.Standard, Coupling.PlanetStack)
    assert(single > compound, s"single $single should beat compound $compound")
  }

  test("NSS compound is more efficient than Standard (one fewer mesh)") {
    // NSS drops stage 2's sun-planet mesh.
    val nss      = Efficiency.overall(s1, Some(s2), Mode.NSS, Coupling.PlanetStack)
    val standard = Efficiency.overall(s1, Some(s2), Mode.Standard, Coupling.PlanetStack)
    assert(nss > standard, s"NSS $nss should beat Standard $standard")
  }

  test("golden: exact efficiency values per coupling on a canonical gear pair") {
    // MeshEff = 0.985. Standard compound = 0.985^4 (2 meshes per stage).
    assertEqualsDouble(
      Efficiency.overall(s1, Some(s2), Mode.Standard, Coupling.PlanetStack),
      math.pow(0.985, 4), 1e-12
    )
    // Single stage = 0.985^2.
    assertEqualsDouble(
      Efficiency.overall(s1, None, Mode.SingleStage, Coupling.PlanetStack),
      math.pow(0.985, 2), 1e-12
    )
    // Novel couplings: each stage contributes a full 2-mesh path → 0.985^4.
    for c <- Seq(Coupling.RingToSun, Coupling.SunToRing, Coupling.RingToRing, Coupling.SunToSun) do
      assertEqualsDouble(
        Efficiency.overall(s1, Some(s2), Mode.Standard, c),
        math.pow(0.985, 4), 1e-12, s"coupling $c"
      )
  }

  // Catalogue #13 fix landed: NSS stage 1 is a full planetary (2 meshes), only
  // stage 2 drops its sun mesh (1 mesh) → 0.985^3, not the old 0.985^2.
  test("NSS efficiency is 0.985^3 — stage 1 keeps both meshes (catalogue #13 fixed)") {
    assertEqualsDouble(
      Efficiency.overall(s1, Some(s2), Mode.NSS, Coupling.PlanetStack),
      math.pow(0.985, 3), 1e-12
    )
  }
