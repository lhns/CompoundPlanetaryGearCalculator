package cpgc.cli

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits.*
import com.monovore.decline.*
import cpgc.Calculator
import cpgc.constraints.{SearchConfig, Topology}
import cpgc.domain.Mode
import cpgc.search.Scorer.SortBy
import cpgc.`export`.{Csv, Json, OpenScad, Pretty}

import java.io.{FileDescriptor, FileOutputStream, PrintStream}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

object Main extends CommandApp(
  name    = "cpgc",
  header  = "Compound Planetary Gear Calculator",
  main    = Search.command
)

object Search:

  private val modeOpt = Opts.option[String](
    "mode", help = "standard | cdcp | nss | single"
  ).withDefault("standard").mapValidated { s =>
    Mode.parse(s).toValidatedNel
  }

  private val sunlessOpt = Opts.option[String](
    "sunless", help = "remove the sun gear of stage 1, 2 or both: 1 | 2 | both"
  ).orNone.mapValidated {
    case None         => Validated.validNel((false, false))
    case Some(s) => s.trim.toLowerCase match
      case "1"    => Validated.validNel((true, false))
      case "2"    => Validated.validNel((false, true))
      case "both" => Validated.validNel((true, true))
      case other  => Validated.invalidNel(s"--sunless must be 1, 2 or both, got '$other'")
  }

  /** Teeth range option pair; validated together in `validate`. */
  private def bounds(name: String, dMin: Int, dMax: Int): Opts[(Int, Int)] =
    (
      Opts.option[Int](s"min-$name", s"minimum $name teeth").withDefault(dMin),
      Opts.option[Int](s"max-$name", s"maximum $name teeth").withDefault(dMax)
    ).tupled

  private val planets =
    (
      Opts.option[Int]("min-planets", "min number of planets").withDefault(3),
      Opts.option[Int]("max-planets", "max number of planets").withDefault(4)
    ).tupled

  private val targets: Opts[List[Double]] =
    Opts.options[Double]("target", "Target gear ratio (repeatable)")
      .map(_.toList)
      .orNone
      .map(_.getOrElse(List.empty))

  private val firstStageOD = Opts.option[Double]("od", "ring outer diameter of stage 1 (mm)").withDefault(150.0)
  private val minModule    = Opts.option[Double]("min-module", "minimum gear module (mm/tooth)").withDefault(0.5)
  private val ringWall     = Opts.option[Double]("ring-wall", "ring wall thickness (mm)").withDefault(SearchConfig.DefaultRingWall)
  private val planetClear  = Opts.option[Double]("planet-clearance", "minimum tip gap between adjacent planets (modules)").withDefault(SearchConfig.DefaultPlanetClearance)
  private val topK         = Opts.option[Int]("top", "top-K results per target").withDefault(10)
  private val sunPlanet    = Opts.option[Double]("sun-planet-factor", "max planet/sun teeth ratio").withDefault(SearchConfig.DefaultSunPlanetFactor)
  private val csvOut       = Opts.option[Path]("csv", "write CSV to this path").orNone
  private val jsonOut      = Opts.option[Path]("json", "write JSON to this path").orNone
  private val scadOut      = Opts.option[Path]("scad", "emit OpenSCAD for the top design").orNone
  private val noTable      = Opts.flag("no-table", "suppress pretty table on stdout").orFalse
  private val phased       = Opts.flag("phased-pinions", "stacked planets are cut with a planet-specific twist (drops the identical-pinion assembly rule, ADR 0018)").orFalse
  private val sortOpt      = Opts.option[String]("sort", "ranking: best | ratio | efficiency | simplicity (best = closest to the targets, or smallest ratio without targets)")
    .withDefault("best").mapValidated(s => Validated.fromOption(SortBy.parse(s), cats.data.NonEmptyList.one(s"--sort must be best, ratio, efficiency or simplicity, got '$s'")))

  /** The topology for `mode`, with the requested suns removed. Invalid
    * combinations (e.g. removing a pinned sun) become an error, not a throw. */
  private def topologyFor(mode: Mode, sunless: (Boolean, Boolean)): ValidatedNel[String, Topology] =
    Topology.withAbsentSuns(Topology.fromMode(mode), sunless._1, sunless._2).toValidatedNel

  private final case class Raw(
      mode: Mode, sunless: (Boolean, Boolean),
      sun: (Int, Int), planet: (Int, Int), ring: (Int, Int), planets: (Int, Int),
      od: Double, minModule: Double, ringWall: Double, clearance: Double, spf: Double,
      targets: List[Double], top: Int,
      csv: Option[Path], json: Option[Path], scad: Option[Path], noTable: Boolean,
      phased: Boolean, sort: SortBy)

  private final case class Run(cfg: SearchConfig, sort: SortBy, csv: Option[Path], json: Option[Path], scad: Option[Path], noTable: Boolean)

  /** The topology comes from the mode and `--sunless`; all numeric validation is
    * `SearchConfig.validate`, shared with the web UI. */
  private def validate(r: Raw): ValidatedNel[String, Run] =
    topologyFor(r.mode, r.sunless).andThen { topo =>
      val input = SearchConfig.Input(
        topology = topo, sun = r.sun, planet = r.planet, ring = r.ring, planets = r.planets,
        firstStageOD = r.od, minModule = r.minModule, ringWall = r.ringWall, planetClearance = r.clearance,
        sunPlanetFactor = r.spf, targets = r.targets, topPerTarget = r.top, phasedPinions = r.phased)
      SearchConfig.validate(input) match
        case Right(cfg) => Validated.validNel(Run(cfg, r.sort, r.csv, r.json, r.scad, r.noTable))
        case Left(errs) => Validated.invalid(NonEmptyList.fromListUnsafe(errs))
    }

  val command: Opts[Unit] = (
    (modeOpt, sunlessOpt, bounds("sun", 8, 80), bounds("planet", 8, 20), bounds("ring", 40, 200), planets).tupled,
    (firstStageOD, minModule, ringWall, planetClear, sunPlanet, targets, topK).tupled,
    (csvOut, jsonOut, scadOut, noTable, phased, sortOpt).tupled
  ).mapN { case ((mode, sunless, sun, planet, ring, pl), (od, mm, wall, clr, spf, ts, top), (csv, json, scad, noTbl, ph, srt)) =>
    Raw(mode, sunless, sun, planet, ring, pl, od, mm, wall, clr, spf, ts, top, csv, json, scad, noTbl, ph, srt)
  }.mapValidated(validate).map { run =>
    val cfg = run.cfg
    val top = cfg.topPerTarget

    // One engine run: per-target top-K for the default ranking with targets,
    // otherwise the overall top-K by the chosen ranking.
    val engine = Calculator.engine(cfg, run.sort, top).run()
    val designs =
      if cfg.targets.nonEmpty && run.sort == SortBy.BestFit then
        val best = engine.perTarget
        cfg.targets.distinct.flatMap(t => best.getOrElse(t, Vector.empty).map(_.design)).toVector
      else engine.top

    // The table uses box-drawing characters; the JVM's default stdout charset on Windows is a legacy code page.
    val out = new PrintStream(new FileOutputStream(FileDescriptor.out), true, StandardCharsets.UTF_8)
    if !run.noTable then
      if designs.isEmpty then out.println("No designs satisfied the constraints.")
      else out.println(Pretty.table(designs))

    run.csv.foreach(p => Files.writeString(p, Csv.render(designs)))
    run.json.foreach(p => Files.writeString(p, Json.render(designs)))
    run.scad.foreach { p =>
      designs.headOption match
        case Some(d) => Files.writeString(p, OpenScad.emit(d))
        case None    => System.err.println("nothing to emit to .scad")
    }
  }
