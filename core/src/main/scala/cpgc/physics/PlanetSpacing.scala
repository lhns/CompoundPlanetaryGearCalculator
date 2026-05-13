package cpgc.physics

import cpgc.domain.GearSet

object PlanetSpacing:

  /** True when planet gears do not overlap each other around the sun.
    *
    * Adjacent planet centers form a chord of length `2 * C2C * sin(pi/n)` where
    * `C2C = (sun + planet) / 2` (in tooth units). Planets fit when each planet's
    * outer diameter `planet + 2` (addendum = module on each side) is less than that
    * chord. Module is a common scaling factor so it cancels.
    */
  def planetsFitWithoutOverlap(set: GearSet): Boolean =
    val c2c        = (set.sun.toDouble + set.planet.toDouble) / 2.0
    val chord      = 2.0 * c2c * math.sin(math.Pi / set.numPlanets)
    val planetOD   = set.planet.toDouble + 2.0
    planetOD < chord
