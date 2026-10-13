package cpgc.constraints

import cpgc.domain.Ratio

final case class TeethBounds(min: Int, max: Int):
  require(min >= 2, s"min teeth must be >= 2, got $min")
  require(max >= min, s"max teeth ($max) must be >= min ($min)")
  def range: Range = min to max

/** Search parameters. The `topology` is the single description of the gearbox
  * (ADR 0015): spacing, stacking, efficiency, kinematics and labels all derive
  * from it. `Mode` survives only as a CLI/URL alias (`Topology.fromMode`).
  *
  * Geometry (ADR 0016): `firstStageOD` bounds the ring's outside, so
  * `m1 = (OD − 2·ringWall) / (Tr1 + 2.5)`. `planetClearance` is the minimum tip
  * gap between adjacent planets, in modules.
  */
final case class SearchConfig(
    topology: Topology = Topology.standardCompound,
    sunTeeth: TeethBounds = TeethBounds(8, 80),
    planetTeeth: TeethBounds = TeethBounds(8, 20),
    ringTeeth: TeethBounds = TeethBounds(40, 200),
    planets: TeethBounds = TeethBounds(3, 5),
    minModule: Double = 0.5,
    firstStageOD: Double = 150.0,
    sunPlanetFactor: Double = 3.0,
    targets: Seq[Ratio] = Seq.empty,
    topPerTarget: Int = 10,
    ringWall: Double = SearchConfig.DefaultRingWall,
    planetClearance: Double = SearchConfig.DefaultPlanetClearance,
    /** ADR 0018: stacked planets may be cut with a planet-specific twist (N different
      * planet parts). Drops the identical-pinion cross term of the assembly rule. */
    phasedPinions: Boolean = false
)

object SearchConfig:
  /** Default ring wall thickness in mm. */
  val DefaultRingWall: Double = 2.0
  /** Default minimum tip clearance between adjacent planets, in modules. */
  val DefaultPlanetClearance: Double = 0.5
