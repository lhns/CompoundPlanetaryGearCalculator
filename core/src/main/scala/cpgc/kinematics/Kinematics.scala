package cpgc.kinematics

import cpgc.constraints.{Body, Topology}
import cpgc.domain.CompoundDesign
import cpgc.search.RoleSolver

/** Angles of every rotating body in a `CompoundDesign`, in radians, at a given
  * input phase. Values are full radians (not mod 2π) so consumers can interpolate
  * smoothly across revolutions.
  *
  *   - `sun1`, `sun2`: body rotation of the sun gears (0 if Absent / not present).
  *   - `carrier1`, `carrier2`: per-stage carrier angle. Equal for the classic
  *     shared-carrier compound (`CarrierToCarrier`); distinct for independent-
  *     carrier compounds. For a single stage `carrier2 == carrier1`.
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

  /** Angle of a central body (0 for a body that doesn't exist in the topology). */
  def of(b: Body): Double = b match
    case Body.S1 => sun1
    case Body.C1 => carrier1
    case Body.R1 => ring1
    case Body.S2 => sun2
    case Body.C2 => carrier2
    case Body.R2 => ring2

object Kinematics:

  /** Body angles as a function of the input phase φ (radians) for the design's
    * own topology. Velocities, including both planets' spins, come from
    * [[RoleSolver.velocities]]. */
  def at(d: CompoundDesign, phi: Double): BodyAngles = at(d, phi, d.topology)

  /** Render-time variant with an explicit topology, for previewing a role
    * assignment other than the one the design was found for. */
  def at(d: CompoundDesign, phi: Double, topology: Topology): BodyAngles =
    RoleSolver.velocities(topology, d.stage1.set, d.stage2.map(_.set), omegaIn = phi) match
      case Some(v) =>
        BodyAngles(
          sun1        = v.omegaS1,
          carrier1    = v.omegaC1,
          planet1Spin = v.omegaP1,
          ring1       = v.omegaR1,
          sun2        = v.omegaS2,
          carrier2    = v.omegaC2OrC1,
          planet2Spin = v.omegaP2,
          ring2       = v.omegaR2
        )
      case None =>
        // Singular at these tooth counts; shouldn't happen for searched designs,
        // but keep the rendering animation alive.
        BodyAngles(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

  /** The (input, output) body angles of the design's topology. */
  def io(d: CompoundDesign, b: BodyAngles): (Double, Double) =
    (b.of(d.topology.input), b.of(d.topology.output))
