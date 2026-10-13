package cpgc.domain

import cpgc.constraints.Topology

/** One search result. `topology` is the gearbox it was found for; `modeName`
  * is the display name of the legacy alias. */
final case class CompoundDesign(
    stage1: Stage,
    stage2: Option[Stage],
    topology: Topology,
    ratio: Ratio,
    efficiency: Double,
    spacingOk: Boolean,
    profileShiftHint: Option[ProfileShift],
    validity: Validity = Validity.Ok
):
  /** "Standard" / "CDCP" / "NSS" / "SingleStage" for canonical topologies, else "Custom". */
  def modeName: String = topology.legacyMode.fold("Custom")(_.toString)
