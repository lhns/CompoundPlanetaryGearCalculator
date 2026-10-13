package cpgc.domain

/** One planetary stage as built: gear set, module and the ring wall that sets
  * the ring's outer diameter (ADR 0016). */
final case class Stage(set: GearSet, module: Module, ringWall: Double = cpgc.constraints.SearchConfig.DefaultRingWall):
  /** Center-to-center distance between sun axis and a planet axis. */
  def centerToCenter: Double = Stage.centerDistance(module.value, set)

  /** Outer diameter of the ring gear: tooth roots reach `m·(Tr + 2.5)`, plus a
    * wall of `ringWall` on each side. */
  def ringOuterDiameter: Double = Stage.ringOuterDiameter(module.value, set.ring.toDouble, ringWall)

object Stage:
  /** Centre distance `m·(Ts + Tp)/2` (virtual sun for a sun-less stage). */
  def centerDistance(module: Double, set: GearSet): Double =
    module * (set.sun.toDouble + set.planet.toDouble) / 2.0

  /** Ring outer diameter `m·(Tr + 2.5) + 2·wall`; the one place this is spelled out. */
  def ringOuterDiameter(module: Double, ringTeeth: Double, wall: Double): Double =
    module * (ringTeeth + 2.5) + 2.0 * wall

  /** Inverse of `ringOuterDiameter` in the module: `(OD − 2·wall) / (Tr + 2.5)`. */
  def moduleForOuterDiameter(od: Double, ringTeeth: Double, wall: Double): Double =
    (od - 2.0 * wall) / (ringTeeth + 2.5)
