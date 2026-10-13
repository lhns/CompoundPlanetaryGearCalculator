package cpgc.`export`

import cpgc.constraints.{Body, BodyRole, Topology}

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
