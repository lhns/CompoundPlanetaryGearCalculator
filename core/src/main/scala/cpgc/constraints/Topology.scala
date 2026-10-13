package cpgc.constraints

import cpgc.domain.Mode

/** Role-assignment topology of a planetary or compound-planetary gearbox: the
  * single source of truth for every rule (ADR 0015). Each `Body` is assigned a
  * `BodyRole`; the forward solver derives every body's (and both planets')
  * angular velocity from the resulting linear system.
  *
  * Construction-time invariants (`require` clauses):
  *   1. `stages` is 1 or 2.
  *   2. `roles` has an entry for every body of the stage count
  *      (`Body.all(stages)`), `Absent` included, and no others.
  *   3. `BodyRole.Absent` only on `S1` / `S2`.
  *   4. Exactly one Input, one Output, one Fixed.
  *   5. Compound couplings differ, `PlanetStack` needs `CarrierToCarrier` as
  *      its partner, and no coupling welds an Absent body.
  *
  * These guarantee the topology is *structurally* valid. They do not guarantee
  * *solvability*: a degenerate tuple may give a singular linear system at
  * certain tooth counts; the solver then returns `None`.
  *
  * `Mode` remains a CLI/URL alias for the four canonical topologies
  * (`Topology.fromMode`, `legacyMode`).
  */
final case class Topology(
    stages: Int,
    roles: Map[Body, BodyRole],
    firstCoupling: Coupling = Coupling.CarrierToCarrier,
    secondCoupling: Coupling = Coupling.PlanetStack
):
  require(stages == 1 || stages == 2, s"stages must be 1 or 2, got $stages")

  // Two identical welds are one constraint, not two. Single
  // stage topologies don't use the coupling fields.
  require(
    stages == 1 || firstCoupling != secondCoupling,
    s"compound couplings must differ, got firstCoupling == secondCoupling == $firstCoupling"
  )
  // A stacked pinion needs a shared carrier (ADR 0015 §4).
  require(
    stages == 1 || !(firstCoupling == Coupling.PlanetStack && secondCoupling != Coupling.CarrierToCarrier) &&
      !(secondCoupling == Coupling.PlanetStack && firstCoupling != Coupling.CarrierToCarrier),
    s"PlanetStack requires CarrierToCarrier as the other coupling, got $firstCoupling + $secondCoupling"
  )

  require(
    roles.keySet == Body.all(stages),
    s"roles must cover exactly ${Body.all(stages).mkString(", ")}, got ${roles.keys.mkString(", ")}"
  )

  private val absent: Set[Body] = roles.collect { case (b, BodyRole.Absent) => b }.toSet
  require(
    absent.subsetOf(Set(Body.S1, Body.S2)),
    s"only S1 / S2 may be Absent, got Absent on: ${absent.mkString(", ")}"
  )

  require(roles.values.count(_ == BodyRole.Input)  == 1, s"exactly one Input required")
  require(roles.values.count(_ == BodyRole.Output) == 1, s"exactly one Output required")
  require(roles.values.count(_ == BodyRole.Fixed)  == 1, s"exactly one Fixed required")

  if stages == 2 then
    for c <- Seq(firstCoupling, secondCoupling); (a, b) <- Coupling.bodyPair(c) do
      require(
        !absent.contains(a) && !absent.contains(b),
        s"coupling $c welds an Absent body (${Seq(a, b).filter(absent.contains).mkString(", ")})"
      )

  val input:  Body = roles.collectFirst { case (b, BodyRole.Input)  => b }.get
  val output: Body = roles.collectFirst { case (b, BodyRole.Output) => b }.get
  val fixed:  Body = roles.collectFirst { case (b, BodyRole.Fixed)  => b }.get

  /** Bodies that physically exist. */
  val presentBodies: Set[Body] = Body.present(stages, absent)

  /** True iff stage 1's sun gear physically exists. */
  def s1Present: Boolean = !absent.contains(Body.S1)

  /** True iff stage 2 exists and its sun gear physically exists. */
  def s2Present: Boolean = stages == 2 && !absent.contains(Body.S2)

  /** Whether stage `k` (1 or 2) has a sun. */
  def sunPresent(stage: Int): Boolean = if stage == 1 then s1Present else s2Present

  /** Stacked planets: a single pinion bridges both stages, so `N1 == N2` and
    * the centre distances must match (ADR 0015 §4). */
  def stacked: Boolean =
    stages == 2 && (firstCoupling == Coupling.PlanetStack || secondCoupling == Coupling.PlanetStack)

  /** Power-path meshes: 2 per stage with a sun, 1 per sun-less stage (ADR 0015 §7). */
  def meshCount: Int =
    (if s1Present then 2 else 1) + (if stages == 2 then (if s2Present then 2 else 1) else 0)

  /** Matching legacy `Mode` for the four canonical role tuples, `None` otherwise. */
  def legacyMode: Option[Mode] = Topology.toLegacyModeOpt(this)

  /** Solver layout (see `RoleSolver`): unknown index of each body by ordinal,
    * -1 when absent. Planets follow the bodies: P1 at `p1Slot`, P2 at `p2Slot`
    * (-1 for a single stage). */
  private[cpgc] val bodySlot: Array[Int] =
    val a = new Array[Int](Body.values.length)
    var k = 0
    for b <- Body.values do
      if presentBodies.contains(b) then { a(b.ordinal) = k; k += 1 } else a(b.ordinal) = -1
    a
  private[cpgc] val p1Slot: Int = presentBodies.size
  private[cpgc] val p2Slot: Int = if stages == 2 then presentBodies.size + 1 else -1
  private[cpgc] val unknowns: Int = presentBodies.size + (if stages == 2 then 2 else 1)

object Topology:

  /** Sun-driven Standard compound: S1 in, R2 out, R1 fixed, carriers shared,
    * planets stacked. */
  val standardCompound: Topology = Topology(
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
    secondCoupling = Coupling.PlanetStack
  )

  /** Carrier-driven compound (CDCP): C1 in, R2 out, R1 fixed, both suns kept. */
  val cdcpCompound: Topology = Topology(
    stages = 2,
    roles = Map(
      Body.C1 -> BodyRole.Input,
      Body.R2 -> BodyRole.Output,
      Body.R1 -> BodyRole.Fixed,
      Body.S1 -> BodyRole.Free,
      Body.S2 -> BodyRole.Free,
      Body.C2 -> BodyRole.Free
    ),
    firstCoupling  = Coupling.CarrierToCarrier,
    secondCoupling = Coupling.PlanetStack
  )

  /** Carrier-driven compound with both suns absent
    * (C1 in, R1 fixed, R2 out, shared carrier, stacked planets). */
  val sunlessCdcpCompound: Topology = Topology(
    stages = 2,
    roles = Map(
      Body.C1 -> BodyRole.Input,
      Body.R2 -> BodyRole.Output,
      Body.R1 -> BodyRole.Fixed,
      Body.S1 -> BodyRole.Absent,
      Body.S2 -> BodyRole.Absent,
      Body.C2 -> BodyRole.Free
    ),
    firstCoupling  = Coupling.CarrierToCarrier,
    secondCoupling = Coupling.PlanetStack
  )

  /** NSS (no second sun): S1 in, R2 out, R1 fixed, shared carrier, stacked
    * planets, S2 absent. */
  val nssCompound: Topology = Topology(
    stages = 2,
    roles = Map(
      Body.S1 -> BodyRole.Input,
      Body.R2 -> BodyRole.Output,
      Body.R1 -> BodyRole.Fixed,
      Body.C1 -> BodyRole.Free,
      Body.C2 -> BodyRole.Free,
      Body.S2 -> BodyRole.Absent
    )
  )

  /** Single-stage reduction: sun in, carrier out, ring fixed. */
  val singleStage: Topology = Topology(
    stages = 1,
    roles = Map(
      Body.S1 -> BodyRole.Input,
      Body.C1 -> BodyRole.Output,
      Body.R1 -> BodyRole.Fixed
    )
  )

  /** Ring-to-sun cascade: shared carrier + `R1 ≡ S2`. */
  val ringToSunCascade: Topology = compound(
    Body.S1, Body.R2, Body.R1, Coupling.CarrierToCarrier, Coupling.RingToSun)

  /** Ravigneaux family: `C1 ≡ S2` + `R1 ≡ R2`. */
  val ravigneaux: Topology = compound(
    Body.S1, Body.C2, Body.R1, Coupling.CarrierToSun, Coupling.RingToRing)

  /** Simpson family: `C1 ≡ R2` + `S1 ≡ S2`. */
  val simpson: Topology = compound(
    Body.S1, Body.C2, Body.R1, Coupling.CarrierToRing, Coupling.SunToSun)

  /** Two-stage topology with the three pinned bodies given and every other body free. */
  private def compound(in: Body, out: Body, fixed: Body, first: Coupling, second: Coupling): Topology =
    val pins = Map(in -> BodyRole.Input, out -> BodyRole.Output, fixed -> BodyRole.Fixed)
    Topology(2, Body.all(2).map(b => b -> pins.getOrElse(b, BodyRole.Free)).toMap, first, second)

  val default: Topology = standardCompound

  /** Map the CLI/URL `Mode` alias onto its canonical `Topology`. */
  def fromMode(m: Mode): Topology = m match
    case Mode.Standard    => standardCompound
    case Mode.CDCP        => cdcpCompound
    case Mode.NSS         => nssCompound
    case Mode.SingleStage => singleStage

  /** `t` with the sun of stage 1 and/or 2 removed (ADR 0015 §2). `Left` with
    * the reason when that is not a legal topology (pinned or welded sun, no
    * such stage); never throws. */
  def withAbsentSuns(t: Topology, s1: Boolean, s2: Boolean): Either[String, Topology] =
    if s2 && t.stages == 1 then Left("a single-stage topology has no second sun")
    else
      val removed = (if s1 then Seq(Body.S1) else Nil) ++ (if s2 then Seq(Body.S2) else Nil)
      removed.find(b => t.roles(b) != BodyRole.Absent && t.roles(b) != BodyRole.Free) match
        case Some(b) => Left(s"cannot remove ${Body.label(b)}: it is the ${t.roles(b)} body")
        case None =>
          try Right(t.copy(roles = t.roles ++ removed.map(_ -> BodyRole.Absent)))
          catch case e: IllegalArgumentException => Left(e.getMessage.stripPrefix("requirement failed: "))

  /** Reverse lookup: `Mode` for the four canonical tuples, `None` otherwise. */
  def toLegacyModeOpt(t: Topology): Option[Mode] = t match
    case `standardCompound` => Some(Mode.Standard)
    case `cdcpCompound`     => Some(Mode.CDCP)
    case `nssCompound`      => Some(Mode.NSS)
    case `singleStage`      => Some(Mode.SingleStage)
    case _                  => None
