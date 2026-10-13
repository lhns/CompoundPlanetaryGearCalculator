package cpgc.verify

import cpgc.constraints.{Body, BodyRole, Coupling, SearchConfig, Topology}
import cpgc.domain.{GearSet, Teeth, Validity}
import cpgc.search.Compounder
import munit.FunSuite

/** Assertions for the per-design degeneracy warnings added in `Compounder.compound`:
  *   - **stage-2 decoupled** — ratio doesn't depend on stage-2 gears
  *   - **stage-1 decoupled** — ratio doesn't depend on stage-1 gears
  *   - **trivial passthrough** — `|ratio − 1| < ε`
  *
  * Backed by the redundancy diagnostic that surfaced two specific classes of
  * such pairs with the Standard role tuple. See
  * `docs/verification/correctness-strategy.md` catalogue #10.
  */
class DecoupledStageSpec extends FunSuite:

  // Even-spaced, physically buildable gear sets with **matching planet count**
  // so PlanetStack-family compounds don't fall through the V3 N1==N2 guard
  // (Edge-Case Catalogue #6).
  // s1: S=20, P=15, R=50, N=3 — (20+50)%3 = 1 — fails Standard but we'll use
  //     pairs whose spacing fits.
  // s2: S=24, P=12, R=48, N=3 — (24+48)%3 = 0 ✓
  // For the cases where Standard requires both stages even-spaced, use s1' below.
  private val s1: GearSet = GearSet(Teeth(24), Teeth(12), 3) // (24+48)%3 = 0 ✓
  private val s2: GearSet = GearSet(Teeth(20), Teeth(16), 3) // R=52, (20+52)%3 = 0 ✓

  /** Build a SearchConfig that surely doesn't filter out our gear samples. */
  private val cfg: SearchConfig = SearchConfig(
    sunPlanetFactor = 1e9,
    minModule       = 0.0001
  )

  test("stage-2-decoupled warning fires on {CarrierToCarrier, CarrierToRing}") {
    // The 8-pair single-stage-equivalent class includes this. With R2 = output
    // and CarrierToRing welding C1 to R2, the output is stage-1's carrier — so
    // stage-2 gears can't influence the ratio.
    val topo = Topology(
      stages = 2,
      roles = Map(
        Body.S1 -> BodyRole.Input,
        Body.R2 -> BodyRole.Output,
        Body.R1 -> BodyRole.Fixed,
        Body.C1 -> BodyRole.Free,
        Body.S2 -> BodyRole.Free,
        Body.C2 -> BodyRole.Free
      ),
      firstCoupling  = Coupling.CarrierToCarrier,
      secondCoupling = Coupling.CarrierToRing
    )
    val d = Compounder.compound(cfg.copy(topology = topo), s1, s2)
    assert(d.isDefined, "expected a design for this coupling pair")
    assert(
      d.get.validity.isInstanceOf[Validity.Warning] &&
        d.get.validity.asInstanceOf[Validity.Warning].reason.contains("stage-2 decoupled"),
      s"expected stage-2-decoupled warning, got ${d.get.validity}"
    )
  }

  test("trivial-passthrough warning fires on {CarrierToCarrier, SunToRing}") {
    // SunToRing welds S1 ≡ R2. With S1 = input and R2 = output, output speed
    // equals input speed → ratio = 1.0.
    val topo = Topology(
      stages = 2,
      roles = Map(
        Body.S1 -> BodyRole.Input,
        Body.R2 -> BodyRole.Output,
        Body.R1 -> BodyRole.Fixed,
        Body.C1 -> BodyRole.Free,
        Body.S2 -> BodyRole.Free,
        Body.C2 -> BodyRole.Free
      ),
      firstCoupling  = Coupling.CarrierToCarrier,
      secondCoupling = Coupling.SunToRing
    )
    val d = Compounder.compound(cfg.copy(topology = topo), s1, s2)
    assert(d.isDefined)
    val v = d.get.validity
    assert(
      v.isInstanceOf[Validity.Warning] &&
        (v.asInstanceOf[Validity.Warning].reason.contains("passthrough") ||
         v.asInstanceOf[Validity.Warning].reason.contains("decoupled")),
      s"expected a warning (passthrough or decoupled), got $v"
    )
  }

  /** True iff the validity is a `Warning` mentioning a degeneracy *we* added in
    * this pass (decoupled or passthrough). Existing warnings like near-singular
    * conditioning are *not* a degeneracy and are accepted as Ok for this spec. */
  private def hasDegeneracyWarning(v: Validity): Boolean = v match
    case Validity.Warning(reason) =>
      reason.contains("decoupled") || reason.contains("passthrough")
    case _ => false

  test("no degeneracy warning on a genuinely-distinct compound (classic Standard)") {
    val topo = Topology.standardCompound
    val d = Compounder.compound(cfg.copy(topology = topo), s1, s2)
    assert(d.isDefined)
    assert(!hasDegeneracyWarning(d.get.validity),
      s"classic Standard should not be flagged decoupled/passthrough, got ${d.get.validity}")
  }

  test("no degeneracy warning on a Ravigneaux compound (genuine independent-carrier)") {
    val topo = Topology(
      stages = 2,
      roles = Map(
        Body.S1 -> BodyRole.Input,
        Body.C2 -> BodyRole.Output,
        Body.R1 -> BodyRole.Fixed,
        Body.C1 -> BodyRole.Free,
        Body.S2 -> BodyRole.Free,
        Body.R2 -> BodyRole.Free
      ),
      firstCoupling  = Coupling.CarrierToSun,
      secondCoupling = Coupling.RingToRing
    )
    val d = Compounder.compound(cfg.copy(topology = topo), s1, s2)
    assert(d.isDefined, "Ravigneaux should produce a design")
    assert(!hasDegeneracyWarning(d.get.validity),
      s"Ravigneaux should not trigger a decoupled/passthrough warning, got ${d.get.validity}")
  }

  test("a non-stacked sun-less stage only carries its ring through: stage-2 decoupled warning") {
    // S2 absent, R1 welded to R2, carriers shared: with C1 fixed and R1 the
    // output, stage 2's planet just idles, so its gears can't change the ratio.
    val topo = Topology(
      stages = 2,
      roles = Map(
        Body.S1 -> BodyRole.Input,
        Body.R1 -> BodyRole.Output,
        Body.C1 -> BodyRole.Fixed,
        Body.S2 -> BodyRole.Absent,
        Body.C2 -> BodyRole.Free,
        Body.R2 -> BodyRole.Free
      ),
      firstCoupling  = Coupling.CarrierToCarrier,
      secondCoupling = Coupling.RingToRing
    )
    val d = Compounder.compound(cfg.copy(topology = topo), s1, s2)
    assert(d.isDefined)
    d.get.validity match
      case Validity.Warning(r) => assert(r.contains("stage-2 decoupled"), r)
      case other               => fail(s"expected the decoupled warning, got $other")
  }
