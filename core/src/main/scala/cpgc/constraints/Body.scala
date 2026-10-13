package cpgc.constraints

/** A central rotational body in the gear train. Six user-facing bodies; the
  * planets are explicit unknowns of the solver (ADR 0015) but are not pinned or
  * welded as bodies, so they are not part of this enum.
  *
  * Each stage has its own carrier: `C1` (stage 1) and `C2` (stage 2). In the
  * classic compound the two carriers are welded together — expressed by the
  * `Coupling.CarrierToCarrier` first-coupling. Independent-carrier compounds
  * (Ravigneaux / Simpson families) leave them as distinct kinematic unknowns
  * and weld a carrier to a non-carrier of the other stage instead.
  */
enum Body:
  case S1, C1, R1, S2, C2, R2

object Body:

  /** Bodies that exist in a gearbox with `stages` stages (before removing
    * `Absent` suns): stage 1 has {S1, C1, R1}; a second stage adds {S2, C2, R2}.
    * Either sun may be Absent (ADR 0015); the carriers and rings always exist.
    */
  def all(stages: Int): Set[Body] = stages match
    case 1 => Set(S1, C1, R1)
    case 2 => Set(S1, C1, R1, S2, C2, R2)
    case n => throw new IllegalArgumentException(s"Unsupported stages: $n (expected 1 or 2)")

  /** Bodies physically present: `all(stages)` minus the absent ones. */
  def present(stages: Int, absent: Set[Body]): Set[Body] = all(stages) -- absent

  /** Display label for the body, used in UI dropdowns and the schematic. */
  def label(b: Body): String = b match
    case Body.S1 => "Sun 1"
    case Body.C1 => "Carrier 1"
    case Body.R1 => "Ring 1"
    case Body.S2 => "Sun 2"
    case Body.C2 => "Carrier 2"
    case Body.R2 => "Ring 2"
