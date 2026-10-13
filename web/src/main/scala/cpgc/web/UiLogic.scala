package cpgc.web

import cpgc.constraints.{SearchConfig, TeethBounds, Topology}
import cpgc.domain.Ratio
import cpgc.search.Scorer.SortBy

/** Raw form state (what the inputs hold), before it becomes a `SearchConfig`. */
final case class FormValues(
    topology: Topology,
    sunMin: Int, sunMax: Int,
    planetMin: Int, planetMax: Int,
    ringMin: Int, ringMax: Int,
    planetsMin: Int, planetsMax: Int,
    od: Double,
    minModule: Double,
    sunPlanetFactor: String,
    target: String,
    phasedPinions: Boolean = false,
    ringWall: Double = SearchConfig.DefaultRingWall,
    clearance: Double = SearchConfig.DefaultPlanetClearance
)

/** Pure, DOM-free UI logic (parsing, topology edits, config building, URL
  * state) so it can be unit-tested without a browser. `App` only wires these
  * into Laminar. */
object UiLogic:

  // ── Numbers ────────────────────────────────────────────────────────────────

  private val NumberRe = """[+-]?(\d+([.,]\d*)?|[.,]\d+)([eE][+-]?\d+)?""".r

  /** Strict decimal parse: accepts a decimal comma, rejects `1d`, `0x1p3`,
    * `1,000.5` and the like. May return Infinity (`1e999`); see `parseFinite`. */
  def parseDouble(s: String): Option[Double] =
    val t = s.trim
    if NumberRe.matches(t) then t.replace(',', '.').toDoubleOption else None

  /** Like `parseDouble` but only finite results. */
  def parseFinite(s: String): Option[Double] = parseDouble(s).filter(_.isFinite)

  /** Should a text input be rewritten to show `current`? Not when its text
    * already parses to that value (the user is typing, e.g. "0."). */
  def shouldRewrite(text: String, current: Double): Boolean = !parseFinite(text).contains(current)
  def shouldRewrite(text: String, current: Int): Boolean    = !text.trim.toIntOption.contains(current)


  // ── Targets ────────────────────────────────────────────────────────────────

  /** Target text → (accepted ratios, dropped pieces). `;` or whitespace
    * separates targets; a comma between digits is a decimal comma
    * (`53,12` = 53.12); "100, 53.12" is two targets. Non-finite or malformed
    * pieces are dropped. */
  def parseTargetsDetailed(s: String): (Seq[Ratio], Seq[String]) =
    val pieces = s.split("[;\\s]+").toList.map(_.stripPrefix(",").stripSuffix(",")).filter(_.nonEmpty)
    val parsed = pieces.map(p => p -> parseFinite(p))
    (parsed.collect { case (_, Some(v)) => Ratio(v) }, parsed.collect { case (p, None) => p })

  def parseTargets(s: String): Seq[Ratio] = parseTargetsDetailed(s)._1

  /** Hint for target pieces that were ignored, if any. */
  def targetHint(s: String): Option[String] =
    val dropped = parseTargetsDetailed(s)._2
    Option.when(dropped.nonEmpty)(s"Ignored (not a finite number): ${dropped.mkString(", ")}")

  // ── Config ─────────────────────────────────────────────────────────────────

  /** Normalise an inverted (lo > hi) pair before passing it to TeethBounds, so
    * the user can drag/type freely without tripping the `max >= min` invariant. */
  def bounds(lo: Int, hi: Int): TeethBounds =
    TeethBounds(math.min(lo, hi), math.max(lo, hi))

  /** Validate the form and build the search configuration (`SearchConfig.validate`, shared with the
    * CLI). Inverted ranges are swapped; a blank max planet/sun ratio means the `SearchConfig` default.
    * Never throws. */
  def buildConfig(f: FormValues, topPerTarget: Int): Either[String, SearchConfig] =
    def sorted(lo: Int, hi: Int) = (math.min(lo, hi), math.max(lo, hi))
    val factor: Either[String, Double] =
      if f.sunPlanetFactor.trim.isEmpty then Right(SearchConfig.DefaultSunPlanetFactor)
      else parseFinite(f.sunPlanetFactor).filter(_ > 0)
        .toRight("Max planet/sun ratio must be a number greater than 0 (or blank)")
    factor.flatMap { spf =>
      SearchConfig.validate(SearchConfig.Input(
        topology        = f.topology,
        sun             = sorted(f.sunMin, f.sunMax),
        planet          = sorted(f.planetMin, f.planetMax),
        ring            = sorted(f.ringMin, f.ringMax),
        planets         = sorted(f.planetsMin, f.planetsMax),
        firstStageOD    = f.od,
        minModule       = f.minModule,
        ringWall        = f.ringWall,
        planetClearance = f.clearance,
        sunPlanetFactor = spf,
        targets         = parseTargets(f.target).map(_.value),
        topPerTarget    = topPerTarget,
        phasedPinions   = f.phasedPinions
      )).left.map(_.mkString("; "))
    }

  // ── Search bookkeeping ─────────────────────────────────────────────────────

  /** A finished, untruncated run that kept every design it found (`found <= k`)
    * can be re-ranked by another criterion without searching again. */
  def rerankable(done: Boolean, truncated: Boolean, found: Long, k: Int): Boolean =
    done && !truncated && found <= k

  /** Pill label; without a target the engine ranks "best fit" by |ratio|. */
  def sortLabel(by: SortBy, hasTargets: Boolean): String = by match
    case SortBy.BestFit    => if hasTargets then "Best fit" else "Lowest ratio (default)"
    case SortBy.Ratio      => "Ratio"
    case SortBy.Efficiency => "Efficiency"
    case SortBy.Simplicity => "Simplicity"

  def sortTooltip(by: SortBy, hasTargets: Boolean): String = by match
    case SortBy.BestFit =>
      if hasTargets then "Closest match to your target ratio (sign ignored)."
      else "No target given, so designs are ranked by absolute ratio, smallest first."
    case SortBy.Ratio      => "Smallest absolute reduction ratio first. Pick this to browse from low to high reduction."
    case SortBy.Efficiency => "Most mechanically efficient designs first. Pick this to minimise transmission losses."
    case SortBy.Simplicity => "Fewest total teeth first. Pick this to favour compact, printable, cheaper designs."

  /** Planet count for display: "3", or "3/4" when the two stages differ (N1/N2). */
  def planetCountLabel(d: cpgc.domain.CompoundDesign): String =
    val n1 = d.stage1.set.numPlanets
    d.stage2.map(_.set.numPlanets) match
      case Some(n2) if n2 != n1 => s"$n1/$n2"
      case _                    => n1.toString

  /** Efficiency as a percentage: the one unit used in the table and schematic. */
  def fmtEfficiency(e: Double): String = f"${e * 100}%.1f%%"
