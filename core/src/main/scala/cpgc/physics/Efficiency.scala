package cpgc.physics

import cpgc.constraints.Coupling
import cpgc.domain.{GearSet, Mode}

/** Coarse mechanical-efficiency estimate.
  *
  * Each spur-gear mesh loses ~1% (98–99% efficient is the textbook range for
  * well-cut, well-lubricated involute gears). A standard planetary stage has
  * two meshes in the power path (sun↔planet, planet↔ring). NSS drops one
  * mesh (no sun-planet mesh on stage 2). Carrier-driven compound (CDCP)
  * routes the same two meshes per stage.
  *
  * Per-coupling power-path mesh counts:
  *  - `PlanetStack` (Std/CDCP) — 4 meshes (2 per stage).
  *  - `PlanetStack` (NSS)      — 3 meshes (drop stage-2 sun-planet).
  *  - `RingToSun`              — 4 meshes (each stage's full planetary path).
  *  - `SunToRing`              — 4 meshes (mirror).
  *  - `RingToRing`             — 4 meshes (rings welded, each stage drives
  *                                  its own planets-to-ring path).
  *  - `SunToSun`               — 4 meshes (suns welded, each stage's full path).
  *
  * This is an estimate for ranking — not a substitute for FEA / dyno data.
  */
object Efficiency:

  private val MeshEff: Double = 0.985

  /** Efficiency of one stage, by power-path mesh count. A stage *with* its own
    * sun has two meshes (sun↔planet, planet↔ring); a sun-less stage (NSS
    * stage 2) has only the planet↔ring mesh.
    *
    * The earlier `stageEfficiency(set, mode)` form keyed solely on `mode`,
    * which could not tell stage 1 from stage 2 — so for NSS it returned the
    * 1-mesh value for *both* stages, undercounting stage 1's sun mesh and
    * overstating overall NSS efficiency. See `docs/verification/
    * correctness-strategy.md` catalogue #13. */
  def stageEfficiency(hasOwnSun: Boolean): Double =
    math.pow(MeshEff, if hasOwnSun then 2 else 1)

  def overall(stage1: GearSet, stage2: Option[GearSet], mode: Mode): Double =
    overall(stage1, stage2, mode, Coupling.PlanetStack)

  /** Coupling-aware overall efficiency.
    *
    * For `PlanetStack`: stage 1 is always a full planetary (2 meshes); stage 2
    * has its own sun for every mode *except* NSS (where it's planet↔ring only).
    * For non-stacked couplings every present stage contributes a full 2-mesh
    * path (4 meshes for a compound, 2 for single-stage). */
  def overall(
      stage1: GearSet, stage2: Option[GearSet],
      mode: Mode, coupling: Coupling
  ): Double = coupling match
    case Coupling.PlanetStack =>
      val e1 = stageEfficiency(hasOwnSun = true)
      val e2 = stage2 match
        case Some(_) => stageEfficiency(hasOwnSun = mode != Mode.NSS)
        case None    => 1.0
      e1 * e2
    case _ =>
      // Each present stage contributes a full 2-mesh path.
      val meshes = if stage2.isDefined then 4 else 2
      math.pow(MeshEff, meshes)
