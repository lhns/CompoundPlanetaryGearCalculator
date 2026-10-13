package cpgc.kinematics

import cpgc.constraints.{Body, Topology}
import cpgc.domain.*
import cpgc.physics.Efficiency
import cpgc.search.RoleSolver
import munit.FunSuite
import scala.util.Random

class RoleKinematicsSpec extends FunSuite:

  private val Seed = 7L

  /** A complete-enough design builder for kinematic tests: the test only reads
    * tooth counts and the mode, so module / efficiency placeholders
    * are fine. Returns `None` if the ratio formula is degenerate (e.g. `r12 = 0`
    * for the chosen tooth counts); the test caller skips those samples. */
  private def designOpt(mode: Mode, s1: GearSet, s2: Option[GearSet]): Option[CompoundDesign] =
    val topology = Topology.fromMode(mode)
    RoleSolver.ratio(topology, s1, s2).map { ratio =>
      CompoundDesign(
        stage1     = Stage(s1, Module(1.0)),
        stage2     = s2.map(g => Stage(g, Module(1.0))),
        topology   = topology,
        ratio      = ratio,
        efficiency = Efficiency.overall(topology)
      )
    }

  private def design(mode: Mode, s1: GearSet, s2: Option[GearSet]): CompoundDesign =
    designOpt(mode, s1, s2).getOrElse(
      fail(s"degenerate gear set for $mode: $s1, $s2 — caller should filter")
    )

  private def randomStage1(rng: Random): GearSet =
    GearSet(Teeth(rng.between(8, 60)), Teeth(rng.between(8, 30)), 3)

  private def randomStage2WithSun(rng: Random): GearSet =
    GearSet(Teeth(rng.between(8, 60)), Teeth(rng.between(8, 30)), 3)

  private def randomNssStage2(rng: Random): GearSet =
    val p = rng.between(8, 30)
    val r = rng.between(2 * p + 1, 2 * p + 60)
    GearSet(Teeth(r - 2 * p), Teeth(p), 3)

  private def assertAnglesAgree(a: BodyAngles, b: BodyAngles, tol: Double, ctx: String): Unit =
    val pairs = Seq(
      ("sun1",        a.sun1,        b.sun1),
      ("carrier",     a.carrier,     b.carrier),
      ("planet1Spin", a.planet1Spin, b.planet1Spin),
      ("ring1",       a.ring1,       b.ring1),
      ("sun2",        a.sun2,        b.sun2),
      ("planet2Spin", a.planet2Spin, b.planet2Spin),
      ("ring2",       a.ring2,       b.ring2)
    )
    pairs.foreach { (name, x, y) =>
      assert(math.abs(x - y) < tol,
        s"$ctx: $name diverged by ${x - y}: expected $x, got $y")
    }

  // ---- Invariants that hold for ALL topologies. `KinematicsSpec` and
  // `verify/AnimationConsistencySpec` check the observed ratio against
  // `design.ratio`. ----

  test("input body angle equals phi exactly for all four legacy modes") {
    val rng = new Random(Seed)
    for mode <- Mode.values do
      val s1 = randomStage1(rng)
      val s2: Option[GearSet] = mode match
        case Mode.SingleStage => None
        case Mode.NSS         => Some(randomNssStage2(rng))
        case _                => Some(randomStage2WithSun(rng))
      val d = design(mode, s1, s2)
      val phi = 2.71828
      val angles = Kinematics.at(d, phi)
      val inputBody = Topology.fromMode(mode).input
      val inputAngle = inputBody match
        case Body.S1 => angles.sun1
        case Body.C1 => angles.carrier1
        case Body.R1 => angles.ring1
        case Body.S2 => angles.sun2
        case Body.C2 => angles.carrier2
        case Body.R2 => angles.ring2
      assertEqualsDouble(inputAngle, phi, 1e-12, s"input angle for $mode")
  }

  test("fixed body angle equals 0 exactly for all four legacy modes") {
    val rng = new Random(Seed + 1)
    for mode <- Mode.values do
      val s1 = randomStage1(rng)
      val s2: Option[GearSet] = mode match
        case Mode.SingleStage => None
        case Mode.NSS         => Some(randomNssStage2(rng))
        case _                => Some(randomStage2WithSun(rng))
      val d = design(mode, s1, s2)
      val angles = Kinematics.at(d, 1.0)
      val fixedBody = Topology.fromMode(mode).fixed
      val fixedAngle = fixedBody match
        case Body.S1 => angles.sun1
        case Body.C1 => angles.carrier1
        case Body.R1 => angles.ring1
        case Body.S2 => angles.sun2
        case Body.C2 => angles.carrier2
        case Body.R2 => angles.ring2
      assertEqualsDouble(fixedAngle, 0.0, 1e-12, s"fixed angle for $mode")
  }

  test("Kinematics.at is linear in phi") {
    val rng = new Random(Seed + 2)
    for mode <- Mode.values do
      val s1 = randomStage1(rng)
      val s2: Option[GearSet] = mode match
        case Mode.SingleStage => None
        case Mode.NSS         => Some(randomNssStage2(rng))
        case _                => Some(randomStage2WithSun(rng))
      val d = design(mode, s1, s2)
      val a1 = Kinematics.at(d, 1.0)
      val a2 = Kinematics.at(d, 2.0)
      assertAnglesAgree(
        BodyAngles(2*a1.sun1, 2*a1.carrier1, 2*a1.planet1Spin, 2*a1.ring1,
                   2*a1.sun2, 2*a1.carrier2, 2*a1.planet2Spin, 2*a1.ring2),
        a2, 1e-9, s"linearity for $mode"
      )
  }

  test("planet coupling holds within 1e-9 for compound designs with S2 present") {
    val rng = new Random(Seed + 3)
    for _ <- 1 to 50 do
      val s1 = randomStage1(rng)
      val s2 = randomStage2WithSun(rng)
      val d  = design(Mode.Standard, s1, Some(s2))
      val a  = Kinematics.at(d, 1.0)
      assert(math.abs(a.planet1Spin - a.planet2Spin) < 1e-9,
        s"planet coupling broken: P1=${a.planet1Spin} P2=${a.planet2Spin}")
  }

  test("observed ratio carrier-velocity / sun-velocity matches design.ratio for single stage") {
    val rng = new Random(Seed + 4)
    var checked = 0
    while checked < 50 do
      val s1 = randomStage1(rng)
      designOpt(Mode.SingleStage, s1, None).foreach { d =>
        val a = Kinematics.at(d, 1.0)
        assert(math.abs(a.sun1 - 1.0) < 1e-12)
        val observed = 1.0 / a.carrier
        assertEqualsDouble(observed, d.ratio.value, 1e-9, s"single stage $s1")
        checked += 1
      }
  }

  test("observed ratio ring2 / sun1 matches design.ratio for Mode.Standard") {
    val rng = new Random(Seed + 5)
    var checked = 0
    var attempts = 0
    while checked < 50 && attempts < 500 do
      attempts += 1
      val s1 = randomStage1(rng)
      val s2 = randomStage2WithSun(rng)
      designOpt(Mode.Standard, s1, Some(s2)).foreach { d =>
        val a = Kinematics.at(d, 1.0)
        if math.abs(a.ring2) > 1e-12 then
          val observed = 1.0 / a.ring2
          assertEqualsDouble(observed, d.ratio.value, 1e-9, s"standard ($s1, $s2)")
          checked += 1
      }
    assert(checked >= 50, s"only $checked non-degenerate Standard designs in $attempts attempts")
  }

  test("Kinematics.io returns the correct (input, output) for each legacy mode") {
    val rng = new Random(Seed + 6)
    val s1 = randomStage1(rng)
    val s2 = randomStage2WithSun(rng)
    val phi = 1.0
    val designs = Seq(
      designOpt(Mode.SingleStage, s1, None),
      designOpt(Mode.Standard,    s1, Some(s2)),
      designOpt(Mode.CDCP,        s1, Some(s2)),
      designOpt(Mode.NSS,         s1, Some(randomNssStage2(rng)))
    ).flatten
    designs.foreach { d =>
      val (in, _) = Kinematics.io(d, Kinematics.at(d, phi))
      assertEqualsDouble(in, phi, 1e-12, s"io input for ${d.modeName}")
    }
  }
