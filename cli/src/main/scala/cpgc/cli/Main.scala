package cpgc.cli

import cats.implicits.*
import com.monovore.decline.*
import cpgc.Calculator
import cpgc.constraints.{SearchConfig, TeethBounds}
import cpgc.domain.{Mode, Ratio}
import cpgc.`export`.{Csv, Json, OpenScad, Pretty}

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

  private def bounds(name: String, dMin: Int, dMax: Int): Opts[TeethBounds] =
    (
      Opts.option[Int](s"min-$name", s"minimum $name teeth").withDefault(dMin),
      Opts.option[Int](s"max-$name", s"maximum $name teeth").withDefault(dMax)
    ).mapN(TeethBounds.apply)

  private val planets =
    (
      Opts.option[Int]("min-planets", "min number of planets").withDefault(3),
      Opts.option[Int]("max-planets", "max number of planets").withDefault(4)
    ).mapN(TeethBounds.apply)

  private val targets: Opts[List[Ratio]] =
    Opts.options[Double]("target", "Target gear ratio (repeatable)")
      .map(_.toList.map(Ratio.apply))
      .orNone
      .map(_.getOrElse(List.empty))

  private val firstStageOD = Opts.option[Double]("od", "first-stage outer diameter (mm)").withDefault(150.0)
  private val minModule    = Opts.option[Double]("min-module", "minimum gear module (mm/tooth)").withDefault(0.5)
  private val topK         = Opts.option[Int]("top", "top-K results per target").withDefault(10)
  private val sunPlanet    = Opts.option[Double]("sun-planet-factor", "max planet/sun teeth ratio").withDefault(3.0)
  private val csvOut       = Opts.option[Path]("csv", "write CSV to this path").orNone
  private val jsonOut      = Opts.option[Path]("json", "write JSON to this path").orNone
  private val scadOut      = Opts.option[Path]("scad", "emit OpenSCAD for the top design").orNone
  private val noTable      = Opts.flag("no-table", "suppress pretty table on stdout").orFalse

  val command: Opts[Unit] = (
    modeOpt,
    bounds("sun", 8, 80),
    bounds("planet", 8, 20),
    bounds("ring", 40, 200),
    planets,
    firstStageOD,
    minModule,
    sunPlanet,
    targets,
    topK,
    csvOut,
    jsonOut,
    scadOut,
    noTable
  ).mapN { (mode, sun, planet, ring, planets, od, mm, spf, ts, top, csv, json, scad, noTbl) =>
    val cfg = SearchConfig(
      mode            = mode,
      sunTeeth        = sun,
      planetTeeth     = planet,
      ringTeeth       = ring,
      planets         = planets,
      minModule       = mm,
      firstStageOD    = od,
      sunPlanetFactor = spf,
      targets         = ts,
      topPerTarget    = top
    )

    val designs =
      if cfg.targets.isEmpty then Calculator.search(cfg).take(top).toVector
      else
        val best = Calculator.best(cfg)
        cfg.targets.flatMap(t => best.getOrElse(t, Vector.empty).map(_.design)).toVector

    if !noTbl then
      if designs.isEmpty then println("No designs satisfied the constraints.")
      else println(Pretty.table(designs))

    csv.foreach(p => Files.writeString(p, Csv.render(designs)))
    json.foreach(p => Files.writeString(p, Json.render(designs)))
    scad.foreach { p =>
      designs.headOption match
        case Some(d) => Files.writeString(p, OpenScad.emit(d))
        case None    => System.err.println("nothing to emit to .scad")
    }
  }
