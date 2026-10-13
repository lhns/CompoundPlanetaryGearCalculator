package cpgc.verify

import cpgc.constraints.{Body, Coupling, Topology}
import cpgc.domain.GearSet

/** Independent forward kinematic solver — the **N-version** oracle.
  *
  * This deliberately shares NO code with `cpgc.search.RoleSolver`: no `LinAlg`,
  * no matrix assembly, no shared body indexing. It derives ratios from the
  * textbook **superposition (tabular) method**, then combines stages by explicit
  * substitution algebra. Two independent implementations of the same kinematic
  * spec agreeing across a large random sweep is the project's strongest
  * practical correctness evidence (see `docs/verification/correctness-strategy.md`).
  *
  * ## Method
  *
  * For one planetary stage the superposition method gives two basis motions:
  *   B1 = (ωS, ωR, ωC) = (1, 1, 1)            — whole unit rotates as a block
  *   B2 = (ωS, ωR, ωC) = (Tr, −Ts, 0)         — carrier fixed, Willis-consistent
  * so a general motion `a·B1 + b·B2` has:
  *   ωS = a + b·Tr,  ωR = a − b·Ts,  ωC = a
  *
  * In a shared-carrier compound both stages have the same `a = ωC`, leaving
  * three unknowns `(ωC, b1, b2)`. The chosen **second coupling** is one linear
  * equation in `(b1, b2)`, which collapses to `b2 = κ·b1` for a coupling-
  * specific constant κ. That leaves two unknowns `(ωC, b1)`, pinned by the
  * Input (=1) and Fixed (=0) rows — solved by an explicit 2×2 formula, not
  * Gauss elimination.
  *
  * ## Coverage
  *
  * Handles the cases the tabular method covers cleanly:
  *   - single stage
  *   - compounds where one coupling is `CarrierToCarrier` and the other is
  *     `PlanetStack`, `RingToSun`, `SunToRing`, `RingToRing` or `SunToSun`, with
  *     either or both suns Absent (a sun-less stage is solved with its virtual
  *     sun `Ts = Tr − 2·Tp`, which does not change ring or planet motion)
  *
  * Returns `None` for independent-carrier compounds (no `CarrierToCarrier`) —
  * those are out of the tabular method's clean reach and are covered by the
  * round-trip + self-consistency oracles instead.
  */
object TabularSolver:

  /** Forward ratio `ω_in / ω_out`, or `None` if out of coverage / singular. */
  def ratio(topology: Topology, s1: GearSet, s2: Option[GearSet]): Option[Double] =
    coefficients(topology, s1, s2).flatMap { coef =>
      val ci = coef(topology.input)
      val co = coef(topology.output)
      val cf = coef(topology.fixed)
      // Input pin:  ωC + b1·ci = 1
      // Fixed pin:  ωC + b1·cf = 0
      val denom = ci - cf
      if math.abs(denom) < 1e-15 then None
      else
        val b1 = 1.0 / denom
        val omegaC = -b1 * cf
        val omegaOut = omegaC + b1 * co
        if math.abs(omegaOut) < 1e-15 then None else Some(1.0 / omegaOut)
    }

  /** Per-body coefficient `coef(body)` such that `ω_body = ωC·1 + b1·coef(body)`.
    * `None` when the topology is outside this solver's coverage. */
  private def coefficients(
      topology: Topology, s1: GearSet, s2: Option[GearSet]
  ): Option[Body => Double] =
    val ts1 = s1.sun.toDouble
    val tp1 = s1.planet.toDouble
    val tr1 = s1.ring.toDouble

    topology.stages match
      case 1 =>
        // Single stage: ωS1 = ωC + b1·Tr1, ωR1 = ωC − b1·Ts1, ωC1 = ωC.
        Some {
          case Body.S1 => tr1
          case Body.R1 => -ts1
          case Body.C1 => 0.0
          case _       => 0.0
        }

      case 2 =>
        // Only the shared-carrier family is in coverage: one coupling must be
        // CarrierToCarrier; the other one closes the remaining DOF.
        val other: Coupling =
          if topology.firstCoupling == Coupling.CarrierToCarrier then topology.secondCoupling
          else if topology.secondCoupling == Coupling.CarrierToCarrier then topology.firstCoupling
          else return None
        val s2set = s2.getOrElse(return None)
        val ts2 = s2set.sun.toDouble
        val tp2 = s2set.planet.toDouble
        val tr2 = s2set.ring.toDouble

        // A sun-less stage is the same stage with a *virtual* sun Ts = Tr − 2·Tp:
        // the ring and planet motions (ωR − ωC = −Ts·b, planet = −Ts·Tr/Tp·b)
        // do not depend on whether that sun gear physically exists, and
        // `Topology` forbids pinning or welding an absent sun. So the same
        // coefficients apply; only `RoleSolver`'s rows differ (no sun mesh).
        //
        // κ such that b2 = κ·b1, derived per remaining coupling (see class doc).
        val kappa: Option[Double] = other match
          case Coupling.PlanetStack =>
            // b1·Tr1·Ts1/Tp1 = b2·Tr2·Ts2/Tp2
            val d = tr2 * ts2 * tp1
            if d == 0.0 then None else Some((tr1 * ts1 * tp2) / d)
          case Coupling.RingToSun =>   // −b1·Ts1 = b2·Tr2
            if tr2 == 0.0 then None else Some(-ts1 / tr2)
          case Coupling.SunToRing =>   // b1·Tr1 = −b2·Ts2
            if ts2 == 0.0 then None else Some(-tr1 / ts2)
          case Coupling.RingToRing =>  // b1·Ts1 = b2·Ts2
            if ts2 == 0.0 then None else Some(ts1 / ts2)
          case Coupling.SunToSun =>    // b1·Tr1 = b2·Tr2
            if tr2 == 0.0 then None else Some(tr1 / tr2)
          case _ => None // carrier-involving couplings: out of coverage
        kappa.map { k =>
          {
            case Body.S1 => tr1
            case Body.R1 => -ts1
            case Body.C1 => 0.0
            case Body.S2 => k * tr2
            case Body.R2 => -k * ts2
            case Body.C2 => 0.0
          }
        }

      case _ => None
