package cpgc.`export`

import cpgc.domain.{CompoundDesign, Stage}

/** Minimal hand-rolled JSON to avoid a JSON dependency in the cross-built core. */
object Json:

  def render(designs: Iterable[CompoundDesign]): String =
    designs.map(renderOne).mkString("[\n  ", ",\n  ", "\n]")

  def renderOne(d: CompoundDesign): String =
    val stage2 = d.stage2.fold("null")(stageJson(_, d.topology.s2Present))
    s"""{"mode":"${d.modeName}","ratio":${fmt(d.ratio.value)},""" +
      s""""efficiency":${fmt(d.efficiency)},""" +
      s""""planets":${d.stage1.set.numPlanets},""" +
      s""""stage1":${stageJson(d.stage1, d.topology.s1Present)},"stage2":$stage2,""" +
      s""""topology":${topologyJson(d)}}"""

  private def topologyJson(d: CompoundDesign): String =
    val t = d.topology
    def str(o: Option[String]) = o.fold("null")(s => s""""$s"""")
    s"""{"modeName":"${d.modeName}","input":"${TopologyInfo.input(t)}",""" +
      s""""output":"${TopologyInfo.output(t)}","fixed":"${TopologyInfo.fixed(t)}",""" +
      s""""firstCoupling":${str(TopologyInfo.firstCoupling(t))},""" +
      s""""secondCoupling":${str(TopologyInfo.secondCoupling(t))},""" +
      s""""absentSuns":${TopologyInfo.absentSuns(t).map(b => s""""$b"""").mkString("[", ",", "]")}}"""

  /** `"sun":null` for a sun-less stage. */
  private def stageJson(s: Stage, sunPresent: Boolean): String =
    val sun = if sunPresent then s.set.sun.toInt.toString else "null"
    s"""{"sun":$sun,"planet":${s.set.planet.toInt},""" +
      s""""ring":${s.set.ring.toInt},"module":${fmt(s.module.value)}}"""

  private def fmt(d: Double): String = Numeric.jsonNumber(d, 6)
