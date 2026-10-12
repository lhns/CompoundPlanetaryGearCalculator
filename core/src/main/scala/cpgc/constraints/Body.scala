package cpgc.constraints

/** A rotational body in the gear train. Six user-facing bodies; the planets
  * are kinematic derivatives of the surrounding sun/ring/carrier via the mesh
  * equations and are therefore not part of this enum.
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

  /** Bodies physically present in a topology with the given stage count and
    * S2-presence flag.
    *
    *   stages = 1            → {S1, C1, R1}                  (stage-2 bodies don't exist)
    *   stages = 2, s2 = true → {S1, C1, R1, S2, C2, R2}      (full compound)
    *   stages = 2, s2 = false → {S1, C1, R1, R2}             (NSS: stage 2 has no sun;
    *                                                          carrier is shared, modelled as C1)
    */
  def present(stages: Int, s2Present: Boolean): Set[Body] = stages match
    case 1 => Set(S1, C1, R1)
    case 2 => if s2Present then Set(S1, C1, R1, S2, C2, R2) else Set(S1, C1, R1, R2)
    case n => throw new IllegalArgumentException(s"Unsupported stages: $n (expected 1 or 2)")

  /** Display label for the body, used in UI dropdowns and the schematic. */
  def label(b: Body): String = b match
    case Body.S1 => "Sun 1"
    case Body.C1 => "Carrier 1"
    case Body.R1 => "Ring 1"
    case Body.S2 => "Sun 2"
    case Body.C2 => "Carrier 2"
    case Body.R2 => "Ring 2"
