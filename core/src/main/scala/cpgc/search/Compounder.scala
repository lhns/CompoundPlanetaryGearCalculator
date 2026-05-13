package cpgc.search

import cpgc.constraints.SearchConfig
import cpgc.domain.*
import cpgc.physics.Efficiency

/** Pairs single-stage gear sets into compound designs and computes their ratios. */
object Compounder:

  /** Compute the overall ratio for a two-stage compound (sun-driven). */
  def compoundRatio(s1: GearSet, s2: GearSet, mode: Mode): Option[Ratio] =
    val tr1 = s1.ring.toDouble
    val tp1 = s1.planet.toDouble
    val ts1 = s1.sun.toDouble
    val tr2 = s2.ring.toDouble
    val tp2 = s2.planet.toDouble

    val ringPlanetRatio1 = tr1 / tp1
    val ringPlanetRatio2 = tr2 / tp2
    if ringPlanetRatio1 == ringPlanetRatio2 then return None

    val r12 = (tr2 - ringPlanetRatio1 * tp2) / tr2
    if r12 == 0.0 then return None

    val sunCarrier =
      if mode == Mode.CDCP then 1.0
      else ts1 / (tr1 + ts1)

    val decimal = r12 * sunCarrier
    if decimal == 0.0 then None
    else Some(Ratio(1.0 / decimal))

  /** Single-stage reduction (sun input, ring fixed, carrier output). */
  def singleStageRatio(s: GearSet): Ratio =
    Ratio(1.0 / s.sunCarrierRatio)

  /** Choose the modules for both stages such that the carrier center-to-center
    * distance is shared. The first-stage module is derived from `firstStageOD`.
    */
  def matchModules(cfg: SearchConfig, s1: GearSet, s2: GearSet): (Module, Module) =
    val m1 = cfg.firstStageOD / (s1.ring.toDouble + 2.0)
    val c2c1 = s1.sun.toDouble + s1.planet.toDouble
    val c2c2 = s2.sun.toDouble + s2.planet.toDouble
    val m2 = m1 * (c2c1 / c2c2)
    (Module(m1), Module(m2))

  /** Build a `CompoundDesign` for two enumerated stages, applying module matching,
    * efficiency and profile-shift hints. Returns `None` if the design fails
    * minimum-module or singular-ratio constraints.
    */
  def compound(cfg: SearchConfig, s1: GearSet, s2: GearSet): Option[CompoundDesign] =
    compoundRatio(s1, s2, cfg.mode).flatMap { r =>
      val (m1, m2) = matchModules(cfg, s1, s2)
      if m1.value < cfg.minModule || m2.value < cfg.minModule then None
      else
        val stage1 = Stage(s1, m1)
        val stage2 = Stage(s2, m2)
        val spacingOk =
          (cfg.mode == Mode.CDCP) || (s1.evenSpacing && s2.evenSpacing)
        val eff = Efficiency.overall(s1, Some(s2), cfg.mode)
        Some(CompoundDesign(
          stage1           = stage1,
          stage2           = Some(stage2),
          mode             = cfg.mode,
          ratio            = r,
          efficiency       = eff,
          spacingOk        = spacingOk,
          profileShiftHint = None // standard-module designs match exactly
        ))
    }

  /** Build a single-stage design. */
  def single(cfg: SearchConfig, s: GearSet): Option[CompoundDesign] =
    val m = Module(cfg.firstStageOD / (s.ring.toDouble + 2.0))
    if m.value < cfg.minModule then None
    else
      val stage = Stage(s, m)
      Some(CompoundDesign(
        stage1           = stage,
        stage2           = None,
        mode             = Mode.SingleStage,
        ratio            = singleStageRatio(s),
        efficiency       = Efficiency.overall(s, None, Mode.SingleStage),
        spacingOk        = s.evenSpacing,
        profileShiftHint = None
      ))
