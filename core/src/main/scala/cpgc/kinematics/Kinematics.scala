package cpgc.kinematics

import cpgc.constraints.Topology
import cpgc.domain.{CompoundDesign, GearSet, Mode}
import cpgc.search.RoleSolver

/** Angles of every rotating body in a `CompoundDesign`, in radians, at a given
  * input phase. Values are full radians (not mod 2π) so consumers can interpolate
  * smoothly across revolutions.
  *
  *   - `sun1`, `sun2`: body rotation of the sun gears (0 if Absent / not present).
  *   - `carrier1`, `carrier2`: per-stage carrier angle. Equal for the classic
  *     shared-carrier compound (`CarrierToCarrier`); distinct for independent-
  *     carrier compounds.
  *   - `planet1Spin`, `planet2Spin`: world-frame body spin of each planet (not orbit).
  *   - `ring1`, `ring2`: body rotation of the ring gears (0 when grounded).
  */
final case class BodyAngles(
    sun1: Double,
    carrier1: Double,
    planet1Spin: Double,
    ring1: Double,
    sun2: Double,
    carrier2: Double,
    planet2Spin: Double,
    ring2: Double
):
  /** Legacy accessor — most consumers want stage 1's carrier. */
  def carrier: Double = carrier1

object Kinematics:

  /** Body angles as a function of the input phase φ (radians).
    *
    * Delegates to [[RoleSolver.velocities]] which solves the small linear
    * system (Willis per stage + planet coupling + Input/Fixed pins) uniformly
    * across every supported topology. The planet angular velocities ωP are
    * recovered from each stage's sun-planet mesh:
    *
    *   ωP = ωC − (ωS − ωC) · Ts / Tp
    *
    * For NSS (S2 absent), ωP2 equals ωP1 because the planets are rigidly
    * coupled on the shared shaft. For single-stage, ωP2 = ωP1 = ωP.
    */
  def at(d: CompoundDesign, phi: Double): BodyAngles =
    at(d, phi, Topology.fromMode(d.mode))

  /** Render-time variant: use an explicit topology rather than the design's
    * legacy `Mode`. Lets the schematic preview novel role assignments even
    * while the search still routes through the legacy-mode fallback. */
  def at(d: CompoundDesign, phi: Double, topology: Topology): BodyAngles =
    val stage2GearSet = d.stage2.map(_.set)
    RoleSolver.velocities(topology, d.stage1.set, stage2GearSet, omegaIn = phi) match
      case Some(v) =>
        val omegaP1 = planetVelocity(d.stage1.set, v.omegaS1, v.omegaC1)
        val omegaP2 = stage2GearSet match
          case Some(s2) if topology.s2Present => planetVelocity(s2, v.omegaS2, v.omegaC2OrC1)
          case _                              => omegaP1  // NSS or single — planets coupled
        BodyAngles(
          sun1        = v.omegaS1,
          carrier1    = v.omegaC1,
          planet1Spin = omegaP1,
          ring1       = v.omegaR1,
          sun2        = if topology.s2Present then v.omegaS2 else 0.0,
          carrier2    = v.omegaC2OrC1,
          planet2Spin = omegaP2,
          ring2       = v.omegaR2
        )
      case None =>
        // Linear system singular at these tooth counts — shouldn't happen for
        // designs the search produced, but keep the rendering animation alive.
        BodyAngles(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

  /** Recover ωP from a sun-bearing stage's external mesh:
    *   (ωS − ωC) · Ts + (ωP − ωC) · Tp = 0
    *   ⇒ ωP = ωC − (ωS − ωC) · Ts / Tp
    */
  private def planetVelocity(s: GearSet, omegaSun: Double, omegaC: Double): Double =
    omegaC - (omegaSun - omegaC) * s.sun.toDouble / s.planet.toDouble

  /** Return the input and output body angles per the design's topology. */
  def io(d: CompoundDesign, b: BodyAngles): (Double, Double) = d.mode match
    case Mode.SingleStage          => (b.sun1, b.carrier)
    case Mode.Standard | Mode.NSS  => (b.sun1, b.ring2)
    case Mode.CDCP                 => (b.carrier, b.ring2)
