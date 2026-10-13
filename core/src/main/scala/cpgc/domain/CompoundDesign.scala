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
    validity: Validity = Validity.Ok,
    /** ADR 0018 assembly mode: stacked planets are cut with a per-planet twist (N different
      * planet parts) instead of being N copies of one part. Only meaningful when stacked. */
    phasedPinions: Boolean = false
):
  /** "Standard" / "CDCP" / "NSS" / "SingleStage" for canonical topologies,
    * "CDCP (sun-less)" for the sun-less CDCP, else "Custom". */
  def modeName: String =
    topology.legacyMode.fold(if topology == Topology.sunlessCdcpCompound then "CDCP (sun-less)" else "Custom")(_.toString)
