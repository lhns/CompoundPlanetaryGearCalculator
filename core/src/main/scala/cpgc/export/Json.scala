package cpgc.`export`

import cpgc.domain.{CompoundDesign, Stage}

/** Minimal hand-rolled JSON to avoid a JSON dependency in the cross-built core. */
object Json:

  def render(designs: Iterable[CompoundDesign]): String =
    designs.map(renderOne).mkString("[\n  ", ",\n  ", "\n]")

  def renderOne(d: CompoundDesign): String =
    val stage2 = d.stage2.fold("null")(stageJson(_, d.topology.s2Present))
    s"""{"mode":"${d.modeName}","ratio":${fmt(d.ratio.value)},""" +
      s""""efficiency":${fmt(d.efficiency)},"spacingOk":${d.spacingOk},""" +
      s""""planets":${d.stage1.set.numPlanets},""" +
      s""""stage1":${stageJson(d.stage1, d.topology.s1Present)},"stage2":$stage2}"""

  /** `"sun":null` for a sun-less stage. */
  private def stageJson(s: Stage, sunPresent: Boolean): String =
    val sun = if sunPresent then s.set.sun.toInt.toString else "null"
    s"""{"sun":$sun,"planet":${s.set.planet.toInt},""" +
      s""""ring":${s.set.ring.toInt},"module":${fmt(s.module.value)}}"""

  private def fmt(d: Double): String =
    if d.isNaN || d.isInfinite then "null"
    else Numeric.fixed(d, 6)
