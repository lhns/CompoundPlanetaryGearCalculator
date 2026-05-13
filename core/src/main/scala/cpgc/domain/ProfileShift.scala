package cpgc.domain

/** Suggested profile-shift coefficients to restore meshing when the desired center
  * distance does not fall on a standard module.
  */
final case class ProfileShift(x1: Double, x2: Double, workingPressureAngleDeg: Double)
