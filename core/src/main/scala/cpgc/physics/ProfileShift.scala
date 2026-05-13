package cpgc.physics

import cpgc.domain.ProfileShift as ProfileShiftHint

/** Profile-shift coefficient hints to restore meshing when the desired center
  * distance does not equal the standard center distance `m*(z1+z2)/2`.
  *
  * Solves the involute equation:
  *   inv(α') = inv(α) + 2*(x1+x2)/(z1+z2) * tan(α)
  * for the working pressure angle α', then splits the total shift symmetrically.
  *
  * `inv(θ) = tan(θ) − θ` (with θ in radians).
  */
object ProfileShift:

  private val PressureAngleDeg = 20.0
  private val PressureAngleRad = math.toRadians(PressureAngleDeg)

  private def inv(theta: Double): Double = math.tan(theta) - theta

  def hint(
      module: Double,
      z1: Int,
      z2: Int,
      desiredCenterDistance: Double
  ): Option[ProfileShiftHint] =
    val a0 = module * (z1 + z2) / 2.0
    if math.abs(desiredCenterDistance - a0) < 1e-9 then None
    else
      // cos(α') = (a0 / a') * cos(α)
      val cosWorking = (a0 / desiredCenterDistance) * math.cos(PressureAngleRad)
      if cosWorking <= 0.0 || cosWorking >= 1.0 then None
      else
        val working   = math.acos(cosWorking)
        val totalShift =
          (inv(working) - inv(PressureAngleRad)) * (z1 + z2) / (2.0 * math.tan(PressureAngleRad))
        val half = totalShift / 2.0
        Some(ProfileShiftHint(half, half, math.toDegrees(working)))
