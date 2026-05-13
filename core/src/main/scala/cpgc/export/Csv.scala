package cpgc.`export`

import cpgc.domain.{CompoundDesign, Stage}

object Csv:

  private val Header: String =
    "mode,ratio,efficiency,planets,spacingOk," +
      "sun1,planet1,ring1,module1," +
      "sun2,planet2,ring2,module2"

  def renderRow(d: CompoundDesign): String =
    val s1 = d.stage1
    val s2Cells = d.stage2 match
      case Some(s) => stageCells(s)
      case None    => ",,,"
    s"${d.mode},${fmt(d.ratio.value)},${fmt(d.efficiency)}," +
      s"${s1.set.numPlanets},${d.spacingOk}," +
      stageCells(s1) + "," + s2Cells

  def render(designs: Iterable[CompoundDesign]): String =
    (Header +: designs.toSeq.map(renderRow)).mkString("\n")

  private def stageCells(s: Stage): String =
    s"${s.set.sun.toInt},${s.set.planet.toInt},${s.set.ring.toInt},${fmt(s.module.value)}"

  private def fmt(d: Double): String = Numeric.fixed(d, 6)
