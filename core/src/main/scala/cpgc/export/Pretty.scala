package cpgc.`export`

import cpgc.domain.{CompoundDesign, Stage}

object Pretty:

  private val Columns: Vector[String] = Vector(
    "Mode", "Ratio", "Eff", "N", "Spacing",
    "S1", "P1", "R1", "M1",
    "S2", "P2", "R2", "M2"
  )

  def table(designs: Iterable[CompoundDesign]): String =
    val rows = designs.toVector.map(row)
    val widths = Columns.indices.map { i =>
      (Columns(i).length +: rows.map(_(i).length)).max
    }
    val sep = widths.map("─" * _).mkString("┼─", "─┼─", "─┤")
    val top = widths.map("─" * _).mkString("┌─", "─┬─", "─┐")
    val bot = widths.map("─" * _).mkString("└─", "─┴─", "─┘")
    val header = renderRow(Columns, widths)
    val body   = rows.map(renderRow(_, widths)).mkString("\n")
    s"$top\n$header\n├─$sep\n$body\n$bot"

  private def fmt(d: Double): String =
    Numeric.fixed(d, 3)

  private def row(d: CompoundDesign): Vector[String] =
    val s1 = d.stage1
    // A sun-less stage shows "—" instead of its virtual sun.
    def sunCell(s: Stage, present: Boolean): String = if present then s.set.sun.toInt.toString else "—"
    val (s2s, s2p, s2r, s2m) = d.stage2 match
      case Some(s) => (sunCell(s, d.topology.s2Present), s.set.planet.toInt.toString, s.set.ring.toInt.toString, fmt(s.module.value))
      case None    => ("-", "-", "-", "-")
    Vector(
      d.modeName,
      fmt(d.ratio.value),
      fmt(d.efficiency),
      s1.set.numPlanets.toString,
      if d.spacingOk then "ok" else "no",
      sunCell(s1, d.topology.s1Present),
      s1.set.planet.toInt.toString,
      s1.set.ring.toInt.toString,
      fmt(s1.module.value),
      s2s, s2p, s2r, s2m
    )

  private def renderRow(cells: Vector[String], widths: Seq[Int]): String =
    cells.zip(widths).map((c, w) => c.padTo(w, ' ')).mkString("│ ", " │ ", " │")
