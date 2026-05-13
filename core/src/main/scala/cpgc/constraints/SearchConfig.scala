package cpgc.constraints

import cpgc.domain.{Mode, Ratio}

final case class TeethBounds(min: Int, max: Int):
  require(min >= 2, s"min teeth must be >= 2, got $min")
  require(max >= min, s"max teeth ($max) must be >= min ($min)")
  def range: Range = min to max

final case class SearchConfig(
    mode: Mode = Mode.Standard,
    sunTeeth: TeethBounds = TeethBounds(8, 80),
    planetTeeth: TeethBounds = TeethBounds(8, 20),
    ringTeeth: TeethBounds = TeethBounds(40, 200),
    planets: TeethBounds = TeethBounds(3, 5),
    minModule: Double = 0.5,
    firstStageOD: Double = 150.0,
    sunPlanetFactor: Double = 3.0,
    targets: Seq[Ratio] = Seq.empty,
    topPerTarget: Int = 10,
    tolerance: Double = 1e-9
)
