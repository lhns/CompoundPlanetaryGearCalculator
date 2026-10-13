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
  *
  * Build user-supplied values with `SearchConfig.validate` (CLI and web both do).
  */
final case class SearchConfig(
    topology: Topology = Topology.standardCompound,
    sunTeeth: TeethBounds = TeethBounds(8, 80),
    planetTeeth: TeethBounds = TeethBounds(8, 20),
    ringTeeth: TeethBounds = TeethBounds(40, 200),
    planets: TeethBounds = TeethBounds(3, 5),
    minModule: Double = 0.5,
    firstStageOD: Double = 150.0,
    sunPlanetFactor: Double = SearchConfig.DefaultSunPlanetFactor,
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
  /** Default cap on planet/sun teeth. */
  val DefaultSunPlanetFactor: Double = 3.0

  /** Largest accepted tooth count and planet count (the dedup key packs them, see `Scorer.dedupKey`). */
  val MaxTeeth: Int = 4095
  val MaxPlanets: Int = 255

  /** Raw, unchecked user input; ranges are `(lo, hi)`. */
  final case class Input(
      topology: Topology,
      sun: (Int, Int), planet: (Int, Int), ring: (Int, Int), planets: (Int, Int),
      firstStageOD: Double, minModule: Double, ringWall: Double, planetClearance: Double,
      sunPlanetFactor: Double,
      targets: Seq[Double],
      topPerTarget: Int,
      phasedPinions: Boolean
  )

  /** The one validation of user input for the CLI and the web UI. Collects every problem as a
    * human-readable message; never throws. A sun range is not checked when no stage has a sun. */
  def validate(in: Input): Either[List[String], SearchConfig] =
    def range(name: String, r: (Int, Int)): Either[String, TeethBounds] =
      val (lo, hi) = r
      if lo < 2 then Left(s"$name min must be at least 2, got $lo")
      else if hi < lo then Left(s"$name max ($hi) must be at least min ($lo)")
      else if hi > MaxTeeth then Left(s"$name max must be at most $MaxTeeth, got $hi")
      else Right(TeethBounds(lo, hi))
    def planetRange(r: (Int, Int)): Either[String, TeethBounds] =
      range("# planets", r).flatMap(b => Either.cond(b.max <= MaxPlanets, b, s"# planets max must be at most $MaxPlanets, got ${b.max}"))
    def positive(name: String, v: Double): Either[String, Double] =
      Either.cond(v.isFinite && v > 0, v, s"$name must be a number greater than 0")
    def nonNeg(name: String, v: Double): Either[String, Double] =
      Either.cond(v.isFinite && v >= 0, v, s"$name must be a number, 0 or more")

    val sunUsed = in.topology.s1Present || in.topology.s2Present
    val checks: List[Either[String, Any]] = List(
      if sunUsed then range("Sun teeth", in.sun) else Right(()),
      range("Planet teeth", in.planet),
      range("Ring teeth", in.ring),
      planetRange(in.planets),
      positive("First-stage OD", in.firstStageOD),
      nonNeg("Min module", in.minModule),
      nonNeg("Ring wall", in.ringWall),
      nonNeg("Planet clearance", in.planetClearance),
      positive("Max planet/sun ratio", in.sunPlanetFactor),
      Either.cond(in.targets.forall(_.isFinite), (), "Target ratios must be finite numbers"),
      Either.cond(in.topPerTarget >= 1, (), s"Top-K must be at least 1, got ${in.topPerTarget}"),
      Either.cond(
        !(in.firstStageOD.isFinite && in.ringWall.isFinite) || in.firstStageOD > 2 * in.ringWall, (),
        "First-stage OD must be larger than twice the ring wall")
    )
    val errors = checks.collect { case Left(e) => e }
    if errors.nonEmpty then Left(errors)
    else
      def tb(r: (Int, Int)) = TeethBounds(r._1, r._2)
      Right(SearchConfig(
        topology        = in.topology,
        sunTeeth        = if sunUsed then tb(in.sun) else SearchConfig().sunTeeth,
        planetTeeth     = tb(in.planet),
        ringTeeth       = tb(in.ring),
        planets         = tb(in.planets),
        minModule       = in.minModule,
        firstStageOD    = in.firstStageOD,
        sunPlanetFactor = in.sunPlanetFactor,
        targets         = in.targets.map(Ratio.apply),
        topPerTarget    = in.topPerTarget,
        ringWall        = in.ringWall,
        planetClearance = in.planetClearance,
        phasedPinions   = in.phasedPinions
      ))
