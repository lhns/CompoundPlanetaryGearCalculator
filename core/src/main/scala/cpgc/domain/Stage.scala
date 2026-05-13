package cpgc.domain

final case class Stage(set: GearSet, module: Module):
  /** Center-to-center distance between sun axis and a planet axis. */
  def centerToCenter: Double =
    module.value * (set.sun.toDouble + set.planet.toDouble) / 2.0

  /** Outer diameter of the ring gear (addendum = module). */
  def ringOuterDiameter: Double =
    module.value * (set.ring.toDouble + 2.0)

  /** Outer diameter of a planet gear. */
  def planetOuterDiameter: Double =
    module.value * (set.planet.toDouble + 2.0)

  /** Outer diameter of the sun gear. */
  def sunOuterDiameter: Double =
    module.value * (set.sun.toDouble + 2.0)
