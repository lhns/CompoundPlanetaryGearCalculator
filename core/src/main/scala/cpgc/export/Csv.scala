package cpgc.`export`

import cpgc.domain.{CompoundDesign, Stage}

object Csv:

  /** Columns 1..13 are stable; topology columns are appended. */
  private val Header: String =
    "mode,ratio,efficiency,planets,spacingOk," +
      "sun1,planet1,ring1,module1," +
      "sun2,planet2,ring2,module2," +
      "input,output,fixed,firstCoupling,secondCoupling,absentSuns"

  def renderRow(d: CompoundDesign): String =
    val s1 = d.stage1
    val s2Cells = d.stage2 match
      case Some(s) => stageCells(s, d.topology.s2Present)
      case None    => ",,,"
    val t = d.topology
    s"${d.modeName},${fmt(d.ratio.value)},${fmt(d.efficiency)}," +
      s"${s1.set.numPlanets},${d.spacingOk}," +
      stageCells(s1, t.s1Present) + "," + s2Cells + "," +
      Seq(
        TopologyInfo.input(t), TopologyInfo.output(t), TopologyInfo.fixed(t),
        TopologyInfo.firstCoupling(t).getOrElse(""), TopologyInfo.secondCoupling(t).getOrElse(""),
        TopologyInfo.absentSuns(t).mkString(";")
      ).mkString(",")

  def render(designs: Iterable[CompoundDesign]): String =
    (Header +: designs.toSeq.map(renderRow)).mkString("\n")

  /** A sun-less stage has no sun gear, so its sun cell is empty. */
  private def stageCells(s: Stage, sunPresent: Boolean): String =
    val sun = if sunPresent then s.set.sun.toInt.toString else ""
    s"$sun,${s.set.planet.toInt},${s.set.ring.toInt},${fmt(s.module.value)}"

  private def fmt(d: Double): String = Numeric.fixed(d, 6)
