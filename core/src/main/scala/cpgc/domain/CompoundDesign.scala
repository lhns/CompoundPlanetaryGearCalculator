package cpgc.domain

final case class CompoundDesign(
    stage1: Stage,
    stage2: Option[Stage],
    mode: Mode,
    ratio: Ratio,
    efficiency: Double,
    spacingOk: Boolean,
    profileShiftHint: Option[ProfileShift]
)
