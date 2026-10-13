package cpgc.render

/** Pure involute-gear geometry: radii and the involute function (tooth phasing is in `Phasing`). Lives in `core` (not `web`) so it can be
  * unit-tested in JVM and reused by any renderer.
  *
  * Units are caller's choice — pass `module` in whatever unit you want the
  * returned radii in. The formulas are all linear in `module`.
  */
object GearGeometry:

  val PressureAngleRad: Double = math.toRadians(20.0)

  /** Involute function: `inv(α) = tan(α) − α` for `α ∈ [0, π/2)`. */
  def inv(alpha: Double): Double = math.tan(alpha) - alpha

  final case class GearRadii(pitch: Double, base: Double, outer: Double, root: Double)

  /** Kinematic gear radii.
    *
    *  - External: outer = pitch + module, root = pitch − 1.25·module
    *    (clipped just inside the base circle to avoid undercut for low
    *    tooth counts).
    *  - Internal: outer = pitch − module (innermost tooth-tip),
    *    root = pitch + 1.25·module (outermost — the back of the tooth).
    */
  def gearRadii(teeth: Int, module: Double, internal: Boolean): GearRadii =
    val rPitch = module * teeth / 2.0
    val rBase  = rPitch * math.cos(PressureAngleRad)
    if internal then
      GearRadii(rPitch, rBase, rPitch - module, rPitch + 1.25 * module)
    else
      // Kinematic root. For low tooth counts the root sits BELOW the base
      // circle (the undercut region); the polygon path handles this with a
      // radial "side wall" from rBase down to rRoot at the flank's end.
      val rRoot = math.max(0.4 * module, rPitch - 1.25 * module)
      GearRadii(rPitch, rBase, rPitch + module, rRoot)
