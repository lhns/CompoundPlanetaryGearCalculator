package cpgc.physics

import cpgc.domain.{GearSet, Mode}

/** Coarse mechanical-efficiency estimate.
  *
  * Each spur-gear mesh loses ~1% (98–99% efficient is the textbook range for
  * well-cut, well-lubricated involute gears). A standard planetary stage has
  * two meshes in the power path (sun↔planet, planet↔ring). A carrier-driven
  * compound replaces one sun mesh with a planet-to-planet coupling and still
  * passes through two ring meshes plus a coupling.
  *
  * This is an estimate for ranking — not a substitute for FEA / dyno data.
  */
object Efficiency:

  private val MeshEff: Double = 0.985

  def stageEfficiency(set: GearSet, mode: Mode): Double = mode match
    case Mode.Standard | Mode.SingleStage => math.pow(MeshEff, 2)
    case Mode.CDCP                        => math.pow(MeshEff, 2)
    case Mode.NSS                         => MeshEff

  def overall(stage1: GearSet, stage2: Option[GearSet], mode: Mode): Double =
    val e1 = stageEfficiency(stage1, mode)
    val e2 = stage2.map(stageEfficiency(_, mode)).getOrElse(1.0)
    e1 * e2
