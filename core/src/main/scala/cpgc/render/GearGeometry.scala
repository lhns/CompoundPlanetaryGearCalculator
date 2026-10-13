package cpgc.render

/** Pure involute-gear geometry: radii and mesh-phase offsets. Lives in `core` (not `web`) so it can be
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

  /** Per-planet static body-rotation offset that closes the sun↔planet mesh.
    *
    * Picks `δ_extra` such that the tooth-fractions at the radial contact
    * satisfy the external-mesh phase invariant `f_s + f_p_inward ≡ 0.5 (mod 1)`.
    * With `rotateRad = φ_i + δ_extra` on the planet polygon, the planet's
    * polygon at the sun-facing direction is at flank-at-pitch (i.e. r_pitch),
    * meeting the sun's polygon edge tangentially at the pitch point.
    */
  def planetPhaseOffset(Ts: Int, Tp: Int, phi: Double): Double =
    val twoPi = 2.0 * math.Pi
    val fsRaw = Ts.toDouble * phi / twoPi
    val fs    = fsRaw - math.floor(fsRaw)
    val currentFp = if Tp % 2 == 0 then 0.0 else 0.5
    val desiredFp = ((0.5 - fs) % 1.0 + 1.0) % 1.0
    var deltaFp = desiredFp - currentFp
    if deltaFp >  0.5 then deltaFp -= 1.0
    if deltaFp < -0.5 then deltaFp += 1.0
    -deltaFp * twoPi / Tp.toDouble

  /** Static rotation to bake into the ring polygon so the planet↔ring mesh
    * satisfies the internal-mesh phase invariant `f_r − f_p_outward ≡ 0.5
    * (mod 1)` at planet 0. Under our assembly condition `(T_s + T_r) % N == 0`,
    * this single rotation aligns the phase for every planet simultaneously.
    */
  def ringPhaseOffset(Ts: Int, Tp: Int, Tr: Int, phi0: Double): Double =
    val twoPi      = 2.0 * math.Pi
    val deltaExtra0 = planetPhaseOffset(Ts, Tp, phi0)
    val fpOutRaw   = Tp.toDouble * (-deltaExtra0) / twoPi
    val fpOut0     = fpOutRaw - math.floor(fpOutRaw)
    val frRaw      = Tr.toDouble * phi0 / twoPi
    val fr0        = frRaw - math.floor(frRaw)
    var deltaFr    = ((0.5 - (fr0 - fpOut0)) % 1.0 + 1.0) % 1.0
    if deltaFr > 0.5 then deltaFr -= 1.0
    -deltaFr * twoPi / Tr.toDouble
