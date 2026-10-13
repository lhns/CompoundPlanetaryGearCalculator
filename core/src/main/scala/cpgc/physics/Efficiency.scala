package cpgc.physics

import cpgc.constraints.Topology

/** Coarse mechanical-efficiency estimate (ADR 0006, ADR 0015 §7).
  *
  * Each spur-gear mesh loses ~1.5 % (`MeshEff = 0.985`). A planetary stage
  * with a sun has two meshes in the power path (sun-planet, planet-ring); a
  * sun-less stage has only planet-ring. The estimate counts meshes only and
  * ignores power recirculation (bug register D4); it ranks designs but is not
  * a substitute for FEA / dyno data.
  */
object Efficiency:

  val MeshEff: Double = 0.985

  /** Overall efficiency of a topology: `MeshEff^(2 per stage with a sun, 1 per sun-less stage)`. */
  def overall(topology: Topology): Double =
    math.pow(MeshEff, topology.meshCount)
