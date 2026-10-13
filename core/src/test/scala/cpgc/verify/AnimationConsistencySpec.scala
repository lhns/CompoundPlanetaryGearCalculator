package cpgc.verify

import cpgc.constraints.{Body, BodyRole, Coupling, SearchConfig, TeethBounds, Topology}
import cpgc.domain.CompoundDesign
import cpgc.kinematics.{BodyAngles, Kinematics}
import cpgc.search.{Compounder, Enumerator, RoleSolver}
import munit.FunSuite

/** The schematic animates `Kinematics.at(design, phi, topology)`. For every
  * topology the UI can show, that animation must agree with the design's
  * reported ratio and with the forward solver. */
class AnimationConsistencySpec extends FunSuite:

  // Mirrors the web presets (cpgc.web.UiLogic); the core module cannot see web.
  private def compound(
      in: Body, out: Body, fixed: Body, c1: Coupling, c2: Coupling
  ): Topology =
    val pins = Map(in -> BodyRole.Input, out -> BodyRole.Output, fixed -> BodyRole.Fixed)
    Topology(2, Body.values.map(b => b -> pins.getOrElse(b, BodyRole.Free)).toMap, c1, c2)

  private val ringToSun  = compound(Body.S1, Body.R2, Body.R1, Coupling.CarrierToCarrier, Coupling.RingToSun)
  private val ravigneaux = compound(Body.S1, Body.C2, Body.R1, Coupling.CarrierToSun, Coupling.RingToRing)
  private val simpson    = compound(Body.S1, Body.C2, Body.R1, Coupling.CarrierToRing, Coupling.SunToSun)

  private val legacy: Seq[(String, Topology)] = Seq(
    "single"   -> Topology.singleStage,
    "standard" -> Topology.standardCompound,
    "cdcp"     -> Topology.cdcpCompound,
    "nss"      -> Topology.nssCompound
  )
  private val novel: Seq[(String, Topology)] = Seq(
    "ring-to-sun"      -> ringToSun,
    "sunless-cdcp"     -> Topology.sunlessCdcpCompound,
    "ravigneaux"       -> ravigneaux,
    "simpson"          -> simpson
  )

  /** Designs built from valid (enumerated, spacing-checked) gear sets. */
  private def designs(t: Topology): Seq[CompoundDesign] =
    val cfg = SearchConfig(
      topology     = t,
      sunTeeth     = TeethBounds(10, 24),
      planetTeeth  = TeethBounds(8, 14),
      ringTeeth    = TeethBounds(30, 80),
      planets      = TeethBounds(3, 4),
      minModule    = 0.01,
      sunPlanetFactor = 1e9
    )
    val sets1 = Enumerator.stage1Sets(cfg).take(40).toVector
    val sets2 = Enumerator.stage2Sets(cfg).take(40).toVector
    val ds =
      if t.stages == 1 then sets1.flatMap(Compounder.single(cfg, _))
      else for a <- sets1; b <- sets2 if a != b; d <- Compounder.compound(cfg, a, b).toList yield d
    val picked = ds.take(30)
    assert(picked.size >= 3, s"too few designs for $t: ${picked.size}")
    picked

  private def angle(b: BodyAngles, body: Body): Double = body match
    case Body.S1 => b.sun1
    case Body.C1 => b.carrier1
    case Body.R1 => b.ring1
    case Body.S2 => b.sun2
    case Body.C2 => b.carrier2
    case Body.R2 => b.ring2

  private val phis = Seq(0.3, 1.0, 7.5)

  for (name, t) <- legacy ++ novel do
    test(s"$name: animated input/output angles reproduce the design ratio"):
      for d <- designs(t); phi <- phis do
        val b   = Kinematics.at(d, phi, t)
        val in  = angle(b, t.input)
        val out = angle(b, t.output)
        assertEqualsDouble(in, phi, 1e-9, s"input angle for $name")
        assertEqualsDouble(in / out, d.ratio.value, 1e-9 * math.max(1.0, math.abs(d.ratio.value)), s"ratio for $name ${d.stage1.set} ${d.stage2.map(_.set)}")

    test(s"$name: fixed body does not move in the animation"):
      for d <- designs(t); phi <- phis do
        assertEqualsDouble(angle(Kinematics.at(d, phi, t), t.fixed), 0.0, 1e-9)

    test(s"$name: stage-2 carrier angle equals the solver's omegaC2 * phi"):
      for d <- designs(t); phi <- phis do
        val v = RoleSolver.velocities(t, d.stage1.set, d.stage2.map(_.set), omegaIn = 1.0).get
        val c2 = if t.roles.contains(Body.C2) then v.omegaC2 else v.omegaC1
        assertEqualsDouble(Kinematics.at(d, phi, t).carrier2, c2 * phi, 1e-9)

    test(s"$name: a shared carrier (CarrierToCarrier) gives equal carrier angles"):
      if t.stages == 2 && Set(t.firstCoupling, t.secondCoupling).contains(Coupling.CarrierToCarrier) then
        for d <- designs(t); phi <- phis do
          val b = Kinematics.at(d, phi, t)
          assertEqualsDouble(b.carrier2, b.carrier1, 1e-9)

  // Kinematics.io reads the design topology's input/output bodies (P5).
  private def ioMatchesTopology(name: String, t: Topology): Unit =
    for d <- designs(t); phi <- phis do
      val b = Kinematics.at(d, phi, t)
      val (in, out) = Kinematics.io(d, b)
      assertEqualsDouble(in,  angle(b, t.input),  1e-9, s"io input for $name")
      assertEqualsDouble(out, angle(b, t.output), 1e-9, s"io output for $name")

  for (name, t) <- legacy do
    test(s"$name: Kinematics.io returns the topology's input/output bodies"):
      ioMatchesTopology(name, t)

  test("ring-to-sun: Kinematics.io returns the topology's input/output bodies (same roles as Standard)"):
    ioMatchesTopology("ring-to-sun", ringToSun)

  test("ravigneaux: Kinematics.io returns the topology's input/output bodies"):
    ioMatchesTopology("ravigneaux", ravigneaux)

  test("simpson: Kinematics.io returns the topology's input/output bodies"):
    ioMatchesTopology("simpson", simpson)

  test("sun-less CDCP: Kinematics.io returns the topology's input/output bodies"):
    ioMatchesTopology("sunless-cdcp", Topology.sunlessCdcpCompound)
