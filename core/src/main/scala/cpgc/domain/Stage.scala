package cpgc.domain

/** One planetary stage as built: gear set, module and the ring wall that sets
  * the ring's outer diameter (ADR 0016). */
final case class Stage(set: GearSet, module: Module, ringWall: Double = Stage.DefaultRingWall):
  /** Center-to-center distance between sun axis and a planet axis. */
  def centerToCenter: Double =
    module.value * (set.sun.toDouble + set.planet.toDouble) / 2.0

  /** Outer diameter of the ring gear: tooth roots reach `m·(Tr + 2.5)`, plus a
    * wall of `ringWall` on each side. */
  def ringOuterDiameter: Double =
    module.value * (set.ring.toDouble + 2.5) + 2.0 * ringWall

  /** Outer diameter of a planet gear. */
  def planetOuterDiameter: Double =
    module.value * (set.planet.toDouble + 2.0)

  /** Outer diameter of the sun gear. */
  def sunOuterDiameter: Double =
    module.value * (set.sun.toDouble + 2.0)

object Stage:
  /** Matches `SearchConfig.DefaultRingWall`. */
  val DefaultRingWall: Double = 2.0
