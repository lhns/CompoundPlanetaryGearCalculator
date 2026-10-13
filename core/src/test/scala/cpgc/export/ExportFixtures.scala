package cpgc.`export`

import cpgc.constraints.Topology
import cpgc.domain.{CompoundDesign, GearSet, Module, Ratio, Stage, Teeth}

/** Hand-built designs shared by the export specs. */
object ExportFixtures:

  private def stage(s: Int, p: Int, n: Int, m: Double) = Stage(GearSet(Teeth(s), Teeth(p), n), Module(m))

  /** Stacked Standard design: S 25/P 11/R 47 + S 26/P 10/R 46, N = 3, m = 2.
    * Both sun+ring sums are 72 (divisible by 3); (11·46 − 10·47) = 36 is divisible by 3,
    * so it satisfies the stacked-planet assembly condition. */
  val standard: CompoundDesign = CompoundDesign(
    stage(25, 11, 3, 2.0), Some(stage(26, 10, 3, 2.0)),
    Topology.standardCompound, Ratio(12.5), 0.9
  )

  /** NSS: stage 2 has no sun (virtual 26). */
  val nss: CompoundDesign = standard.copy(topology = Topology.nssCompound)

  /** Sun-less CDCP: neither stage has a sun. */
  val sunless: CompoundDesign = standard.copy(topology = Topology.sunlessCdcpCompound)

  /** Stage 1 has no sun, stage 2 has one. */
  val s1Absent: CompoundDesign =
    standard.copy(topology = Topology.withAbsentSuns(Topology.cdcpCompound, true, false).toOption.get)

  val single: CompoundDesign =
    CompoundDesign(stage(24, 12, 3, 2.5), None, Topology.singleStage, Ratio(3.0), 0.97)

  val all: Vector[(String, CompoundDesign)] = Vector(
    "standard" -> standard, "nss" -> nss, "sunless" -> sunless, "s1Absent" -> s1Absent, "single" -> single
  )
