package cpgc.`export`

import cpgc.domain.{CompoundDesign, Stage}

object Pretty:

  private val Columns: Vector[String] = Vector(
    "Mode", "Ratio", "Eff", "N", "N2",
    "S1", "P1", "R1", "M1",
    "S2", "P2", "R2", "M2",
    "Layout", "No sun"
  )

  def table(designs: Iterable[CompoundDesign]): String =
    val rows = designs.toVector.map(row)
    val widths = Columns.indices.map { i =>
      (Columns(i).length +: rows.map(_(i).length)).max
    }
    def border(l: String, mid: String, r: String) =
      widths.map("─" * _).mkString(l + "─", "─" + mid + "─", "─" + r)
    val header = renderRow(Columns, widths)
    val body   = rows.map(renderRow(_, widths))
    (Vector(border("┌", "┬", "┐"), header, border("├", "┼", "┤")) ++ body :+ border("└", "┴", "┘")).mkString("\n")

  private def fmt(d: Double): String =
    Numeric.fixed(d, 3)

  /** "S1→R2 | R1 fixed | CarrierToCarrier+PlanetStack" (couplings only for compounds). */
  private def layout(d: CompoundDesign): String =
    val t = d.topology
    val base = s"${TopologyInfo.input(t)}→${TopologyInfo.output(t)} | ${TopologyInfo.fixed(t)} fixed"
    (TopologyInfo.firstCoupling(t), TopologyInfo.secondCoupling(t)) match
      case (Some(a), Some(b)) => s"$base | $a+$b"
      case _                  => base

  private def row(d: CompoundDesign): Vector[String] =
    val s1 = d.stage1
    // A sun-less stage shows "—" instead of its virtual sun.
    def sunCell(s: Stage, present: Boolean): String = if present then s.set.sun.toInt.toString else "—"
    val (s2s, s2p, s2r, s2m) = d.stage2 match
      case Some(s) => (sunCell(s, d.topology.s2Present), s.set.planet.toInt.toString, s.set.ring.toInt.toString, fmt(s.module.value))
      case None    => ("-", "-", "-", "-")
    val absent = TopologyInfo.absentSuns(d.topology)
    Vector(
      d.modeName,
      fmt(d.ratio.value),
      fmt(d.efficiency),
      s1.set.numPlanets.toString,
      d.stage2.fold("-")(_.set.numPlanets.toString),
      sunCell(s1, d.topology.s1Present),
      s1.set.planet.toInt.toString,
      s1.set.ring.toInt.toString,
      fmt(s1.module.value),
      s2s, s2p, s2r, s2m,
      layout(d),
      if absent.isEmpty then "-" else absent.mkString(";")
    )

  private def renderRow(cells: Vector[String], widths: Seq[Int]): String =
    cells.zip(widths).map((c, w) => c.padTo(w, ' ')).mkString("│ ", " │ ", " │")
