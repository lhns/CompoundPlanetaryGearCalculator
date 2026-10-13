package cpgc.`export`

import cpgc.constraints.{Body, BodyRole, Topology}
import cpgc.domain.CompoundDesign
import cpgc.render.Phasing

/** The topology fields every export carries, so a row can be tied back to its layout. */
private[`export`] object TopologyInfo:

  def input(t: Topology): String  = t.input.toString
  def output(t: Topology): String = t.output.toString
  def fixed(t: Topology): String  = t.fixed.toString

  /** Couplings only exist for compounds. */
  def firstCoupling(t: Topology): Option[String]  = if t.stages == 2 then Some(t.firstCoupling.toString) else None
  def secondCoupling(t: Topology): Option[String] = if t.stages == 2 then Some(t.secondCoupling.toString) else None

  /** Sun-less bodies (`S1` / `S2`) in body order. */
  def absentSuns(t: Topology): Vector[Body] =
    Body.values.toVector.filter(b => t.roles.get(b).contains(BodyRole.Absent))

  /** ADR 0018 assembly mode: "phased" (per-planet twisted pinions) or "identical" (N copies of one part). */
  def assembly(d: CompoundDesign): String =
    if d.phasedPinions && d.topology.stacked then "phased" else "identical"

  /** Per-planet half-2 twist in degrees; empty unless the design is phased. */
  def twistsDeg(d: CompoundDesign): Vector[Double] =
    if assembly(d) == "phased" then Phasing.solve(d).twists.getOrElse(Vector.empty).map(_ * 360.0)
    else Vector.empty
