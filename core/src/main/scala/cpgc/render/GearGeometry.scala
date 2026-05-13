package cpgc.render

/** Pure involute-gear geometry: radii, mesh-phase offsets, and a closed-form
  * polygon-outline-radius function. Lives in `core` (not `web`) so it can be
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

  /** Outer-envelope radius of the involute polygon at the given body angle.
    *
    * Closed-form (no polyline sampling): folds the body angle into one tooth
    * pitch, classifies it into tip arc / flank / trough, and returns the
    * polygon's r value at that body angle.
    */
  def polygonOutlineRadius(
      teeth: Int, module: Double, internal: Boolean, bodyAngle: Double
  ): Double =
    val r          = gearRadii(teeth, module, internal)
    val twoPi      = 2.0 * math.Pi
    val toothPitch = twoPi / teeth
    val deltaPitch = math.Pi / (2.0 * teeth)
    val invP       = inv(PressureAngleRad)
    val deltaBase  = deltaPitch + invP

    // Reduce body angle modulo tooth pitch, expressed as |offset from tooth centre|.
    val centred = bodyAngle - math.round(bodyAngle / toothPitch) * toothPitch
    val absDelta = math.abs(centred)

    if internal then
      // Tooth tip body angle: 0 (apex, single point at rOuter = rPitch − m).
      // Flank: from tip apex outward to rRoot at body angle ≈ deltaPitch + (inv(α_root) − invP).
      // Trough: at rRoot, from end of right flank to start of left flank of next tooth.
      //
      // For overlap-correctness purposes we report the polygon's OUTER envelope
      // from the gear's own centre. For internal, "outer envelope" = the radius
      // closest to the gear interior (the back-of-tooth / trough at rRoot is the
      // outermost; tooth tips are innermost).
      //
      // We need an `r(bodyAngle)` that reflects the polygon's outline curve at
      // that angle — for an internal gear traced as tip → flank → trough → flank
      // → next tip, at any body angle the polygon has exactly one r value.
      if absDelta <= 1e-12 then r.outer            // exactly at tip apex
      else
        // Find r on the flank: inv(α(r)) − invP = (absDelta − epsilon).
        // The flank parameter spans from 0 (at base circle) to inv(α_root) − invP
        // at the trough start. The trough then continues at r.root until the next
        // tooth's flank begins.
        val flankSpanAtRoot = invertInvolute(r.root, r.base, internal = true) - invP
        val flankAngularSpan = flankSpanAtRoot   // angular extent of the flank from apex going outward
        // Actually the flank covers absDelta ∈ [0, deltaPitch + flankSpanAtRoot]
        // We need a cleaner formulation. See external case below — simpler.
        val flankEnd = deltaPitch + flankSpanAtRoot
        if absDelta < flankEnd then
          // On flank — solve for r from `inv(α(r)) = absDelta + invP − deltaPitch`
          val invAlpha = absDelta + invP - deltaPitch
          if invAlpha <= 0.0 then
            // Near the apex: linear interpolation between tip apex (rOuter) and
            // base flank start at angular offset (deltaPitch − invP).
            val frac = absDelta / (deltaPitch - invP).max(1e-9)
            r.outer + frac * (r.base - r.outer)
          else
            radiusFromInvolute(invAlpha, r.base)
        else r.root
    else
      // External: tooth tip at body angle 0 (r = r_outer over the tip-arc range),
      // right flank from |delta| = tipExtent to deltaBase, trough at r_root beyond.
      val invOuter   = invAtRadius(r.outer, r.base)
      val tipExtent  = deltaBase - invOuter
      if absDelta <= tipExtent then r.outer
      else if absDelta <= deltaBase then
        val invAlpha = deltaBase - absDelta
        radiusFromInvolute(invAlpha, r.base)
      else r.root

  /** Inverse of `inv(α)`: given `inv(α)`, return `α`. Newton iteration starting
    * from a closed-form initial guess. Converges in ~3 iterations for typical
    * gear pressure angles.
    */
  private def invertInvolute(invAlpha: Double): Double =
    if invAlpha <= 0.0 then 0.0
    else
      // Initial guess: from cubic Taylor expansion inv(α) ≈ α³/3.
      var alpha = math.cbrt(3.0 * invAlpha)
      var i = 0
      while i < 8 do
        val f  = math.tan(alpha) - alpha - invAlpha
        val df = math.tan(alpha) * math.tan(alpha)  // d/dα(tan α − α) = sec² − 1 = tan²
        if math.abs(df) < 1e-12 then i = 8
        else
          alpha -= f / df
          i += 1
      alpha

  /** `α(r) = acos(r_base / r)`, the pressure angle at radius r. */
  private def alphaAtRadius(r: Double, rBase: Double): Double =
    if r <= rBase then 0.0
    else math.acos((rBase / r).min(1.0))

  /** `inv(α(r))` at given r. */
  private def invAtRadius(r: Double, rBase: Double): Double =
    val a = alphaAtRadius(r, rBase)
    if a == 0.0 then 0.0 else inv(a)

  /** Helper for internal-flank inverse — same idea but for the internal
    * direction (r > r_pitch).
    */
  private def invertInvolute(r: Double, rBase: Double, internal: Boolean): Double =
    invAtRadius(r, rBase)

  /** Given inv(α) and the base radius, return r = rBase / cos(α). */
  private def radiusFromInvolute(invAlpha: Double, rBase: Double): Double =
    val alpha = invertInvolute(invAlpha)
    rBase / math.cos(alpha)
