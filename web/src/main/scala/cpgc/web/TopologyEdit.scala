package cpgc.web

import cpgc.constraints.{Body, BodyRole, Coupling, Topology}

// Topology editing: gearbox presets, role assignment, sun toggles and coupling rules.
/** Which of the two inter-stage weld dropdowns a coupling edit comes from. */
enum CouplingSlot:
  case First, Second

/** Named gearbox families. Each maps to a complete `Topology`; `Custom` is the
  * derived state of a topology that matches no family (never selectable). */
enum GearboxPreset:
  case SingleStage
  case SunDriven       // Standard compound: stacked planet, sun in, R2 out
  case CarrierDriven   // CDCP: stacked planet, carrier in, R2 out
  case SunlessCdcp     // CDCP without suns
  case NoSecondSun     // NSS: stacked planet, no S2
  case RingToSunCascade
  case Ravigneaux
  case Simpson
  case Custom

object TopologyEdit:

  /** Presets the user can pick (everything but the derived `Custom`). */
  val selectablePresets: Seq[GearboxPreset] = GearboxPreset.values.toSeq.filter(_ != GearboxPreset.Custom)

  /** Map a preset → its canonical `Topology`. `Custom` has none. */
  def presetTopology(p: GearboxPreset): Topology = p match
    case GearboxPreset.SingleStage       => Topology.singleStage
    case GearboxPreset.SunDriven         => Topology.standardCompound
    case GearboxPreset.CarrierDriven     => Topology.cdcpCompound
    case GearboxPreset.SunlessCdcp       => Topology.sunlessCdcpCompound
    case GearboxPreset.NoSecondSun       => Topology.nssCompound
    case GearboxPreset.RingToSunCascade  => Topology.ringToSunCascade
    case GearboxPreset.Ravigneaux        => Topology.ravigneaux
    case GearboxPreset.Simpson           => Topology.simpson
    case GearboxPreset.Custom            => Topology.standardCompound // unused: not selectable

  /** Reverse-lookup: which preset (if any) does this `Topology` match? */
  def topologyPreset(t: Topology): GearboxPreset =
    selectablePresets.find(p => presetTopology(p) == t).getOrElse(GearboxPreset.Custom)

  // ── Topology edits ─────────────────────────────────────────────────────────

  private def reason(e: IllegalArgumentException): String =
    Option(e.getMessage).getOrElse("invalid topology").stripPrefix("requirement failed: ")

  /** Move `body` to `newRole`, displacing whoever previously held `newRole`
    * into `body`'s previous role. Invariants of `Topology` are preserved, and
    * the user's couplings are carried through. Total: an unknown or absent
    * body, or an illegal result, leaves the topology unchanged. */
  def assignRole(t: Topology, body: Body, newRole: BodyRole): Topology =
    t.roles.get(body) match
      case None | Some(BodyRole.Absent) => t
      case Some(`newRole`)              => t
      case Some(previousOwnRole) =>
        val previousHolder = t.roles.collectFirst { case (b, r) if r == newRole => b }
        val updated = previousHolder match
          case Some(b) if b != body => t.roles.updated(body, newRole).updated(b, previousOwnRole)
          case _                    => t.roles.updated(body, newRole)
        try Topology(t.stages, updated, t.firstCoupling, t.secondCoupling)
        catch case _: IllegalArgumentException => t

  /** Make stage `stage`'s sun present or absent. A pinned sun first hands its
    * pin role to a free body. `Left(reason)` when that is impossible (no
    * free body, the sun is welded by a coupling, single stage, ...). */
  def setSunPresent(t: Topology, stage: Int, present: Boolean): Either[String, Topology] =
    val sun = if stage == 1 then Body.S1 else Body.S2
    t.roles.get(sun) match
      case None => Left("a single-stage gearbox has no second sun")
      case Some(role) =>
        if present then
          if role != BodyRole.Absent then Right(t)
          else Right(Topology(t.stages, t.roles.updated(sun, BodyRole.Free), t.firstCoupling, t.secondCoupling))
        else if role == BodyRole.Absent then Right(t)
        else
          val pinned = role != BodyRole.Free
          // Free bodies that could take over the pin, nearest to the usual drive line first.
          val takers: Seq[Option[Body]] =
            if !pinned then Seq(None)
            else Seq(Body.C1, Body.C2, Body.R1, Body.R2, Body.S1, Body.S2)
              .filter(b => b != sun && t.roles.get(b).contains(BodyRole.Free)).map(Some(_))
          if takers.isEmpty then
            Left(s"${Body.label(sun)} is the ${role.toString.toLowerCase} body and no free body can take over")
          else
            var last = "invalid topology"
            val it = takers.iterator
            var res: Option[Topology] = None
            while res.isEmpty && it.hasNext do
              val roles0 = it.next().fold(t.roles)(b => t.roles.updated(b, role))
              try res = Some(Topology(t.stages, roles0.updated(sun, BodyRole.Absent), t.firstCoupling, t.secondCoupling))
              catch case e: IllegalArgumentException => last = reason(e)
            res.toRight(last)

  /** Why the stage-`stage` sun toggle cannot be flipped (None when it can). */
  def sunToggleDisabledReason(t: Topology, stage: Int): Option[String] =
    val sun     = if stage == 1 then Body.S1 else Body.S2
    val present = t.roles.get(sun).exists(_ != BodyRole.Absent)
    setSunPresent(t, stage, !present).left.toOption

  /** Toggle S2; unchanged when impossible. */
  def setS2Present(t: Topology, present: Boolean): Topology =
    setSunPresent(t, 2, present).getOrElse(t)

  /** Switch between single-stage and compound. We reset to the canonical
    * `singleStage` / `standardCompound` rather than try to preserve roles —
    * the body set changes, so role preservation isn't generally meaningful. */
  def setStageCount(t: Topology, n: Int): Topology = n match
    case 1 => Topology.singleStage
    case 2 => if t.stages == 2 then t else Topology.standardCompound
    case _ => t

  /** Replace one of the two inter-stage welds. `Left(reason)` when the choice
    * is not allowed: the same weld as the other slot, `PlanetStack` without a
    * `CarrierToCarrier` partner, or a weld on an absent sun. Moving the
    * partner of `PlanetStack` away from `CarrierToCarrier` deselects
    * `PlanetStack`: the displaced shared-carrier weld takes its place. */
  def trySetCoupling(t: Topology, slot: CouplingSlot, c: Coupling): Either[String, Topology] =
    val (mine, other) = slot match
      case CouplingSlot.First  => (t.firstCoupling, t.secondCoupling)
      case CouplingSlot.Second => (t.secondCoupling, t.firstCoupling)
    if t.stages != 2 then Left("a single-stage gearbox has no inter-stage welds")
    else if c == mine then Right(t)
    else if c == other then Left("already used by the other coupling")
    else if c == Coupling.PlanetStack && other != Coupling.CarrierToCarrier then
      Left("stacked planets need the shared carrier (Carrier 1 ↔ Carrier 2) as the other coupling")
    else
      val newOther = if other == Coupling.PlanetStack && c != Coupling.CarrierToCarrier then mine else other
      val (f, s) = slot match
        case CouplingSlot.First  => (c, newOther)
        case CouplingSlot.Second => (newOther, c)
      try Right(t.copy(firstCoupling = f, secondCoupling = s))
      catch case e: IllegalArgumentException => Left(reason(e))

  /** `trySetCoupling`, unchanged on rejection. */
  def setCoupling(t: Topology, slot: CouplingSlot, c: Coupling): Topology =
    trySetCoupling(t, slot, c).getOrElse(t)

  /** True when the two coupling dropdowns are inactive. Couplings matter for
    * every compound, with or without a second sun (ADR 0015). */
  def couplingSelectorsDisabled(t: Topology): Boolean = t.stages != 2

  /** Whether option `c` of the `slot` dropdown is greyed out. */
  def couplingOptionDisabled(t: Topology, slot: CouplingSlot, c: Coupling): Boolean =
    trySetCoupling(t, slot, c).isLeft

  /** Why option `c` is greyed out (for the option's tooltip). */
  def couplingOptionReason(t: Topology, slot: CouplingSlot, c: Coupling): Option[String] =
    trySetCoupling(t, slot, c).left.toOption
