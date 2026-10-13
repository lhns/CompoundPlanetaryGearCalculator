package cpgc.physics

import cpgc.domain.GearSet

object PlanetSpacing:

  /** True when adjacent planets keep at least `clearance` modules of tip gap
    * (ADR 0016; `clearance = 0` is the plain no-overlap check of ADR 0005).
    *
    * Adjacent planet centres form a chord of length `(Ts + Tp)·sin(π/N)` (tooth
    * units; the module cancels). A planet's outer diameter is `Tp + 2`, so the
    * planets fit when `Tp + 2 + clearance < (Ts + Tp)·sin(π/N)`. A sun-less
    * stage uses its virtual sun `Tr − 2·Tp`.
    */
  def planetsFitWithoutOverlap(set: GearSet, clearance: Double): Boolean =
    val chord    = (set.sun.toDouble + set.planet.toDouble) * math.sin(math.Pi / set.numPlanets)
    val planetOD = set.planet.toDouble + 2.0
    planetOD + clearance < chord
