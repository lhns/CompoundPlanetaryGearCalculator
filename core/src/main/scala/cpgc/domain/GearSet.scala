package cpgc.domain

/** One planetary stage: a sun gear meshing with N planet gears inside a ring gear.
  *
  * Standard tooth relation for meshing geometry: ring = sun + 2 * planet.
  */
final case class GearSet(sun: Teeth, planet: Teeth, numPlanets: Int):
  require(numPlanets >= 2, s"numPlanets must be >= 2, got $numPlanets")

  val ring: Teeth = Teeth(sun.toInt + 2 * planet.toInt)

  /** Whether the planets can be equally spaced around the sun. */
  def evenSpacing: Boolean =
    (sun.toInt + ring.toInt) % numPlanets == 0

  /** Sun-input, ring-fixed, carrier-output reduction. */
  def sunCarrierRatio: Double =
    sun.toDouble / (ring.toDouble + sun.toDouble)
