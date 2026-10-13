package cpgc.constraints

/** A rigid weld between one body of stage 1 and one body of stage 2.
  *
  * A compound gearbox is formed by **two** such welds (`Topology.firstCoupling`
  * and `Topology.secondCoupling`). The classic compound uses
  * `{CarrierToCarrier, PlanetStack}`; novel compounds (Ravigneaux / Simpson
  * families) pick other pairs — e.g. `{CarrierToSun, RingToRing}`.
  *
  * The forward solver emits one linear-system row per coupling:
  *
  *  - `PlanetStack`: stage-1 and stage-2 planets are one physical pinion,
  *    row `ωP1 − ωP2 = 0`.
  *  - every other case welds two central-body shafts: row `ω_a − ω_b = 0` for
  *    the body pair returned by `bodyPair`.
  *
  * ## Physical realisability
  *
  * A single stacked pinion can't sit at two different orbital positions, so
  * `PlanetStack` requires `CarrierToCarrier` as its partner. `Topology`
  * enforces this at construction (ADR 0015 §4): the eight kinematic chimeras
  * `{PlanetStack, X != CarrierToCarrier}` can no longer be built. See
  * `docs/verification/correctness-strategy.md` Edge-Case Catalogue #19.
  */
enum Coupling:
  case PlanetStack       // P1 ≡ P2 (stacked planet pinions)
  case RingToSun         // R1 ≡ S2
  case SunToRing         // S1 ≡ R2
  case RingToRing        // R1 ≡ R2
  case SunToSun          // S1 ≡ S2
  case CarrierToCarrier  // C1 ≡ C2  (the classic shared carrier)
  case CarrierToSun      // C1 ≡ S2
  case CarrierToRing     // C1 ≡ R2
  case SunToCarrier      // S1 ≡ C2
  case RingToCarrier     // R1 ≡ C2

object Coupling:

  /** Default *first* coupling for compound topologies — the classic shared
    * carrier. */
  val defaultFirst: Coupling = CarrierToCarrier

  /** Default *second* coupling — the classic stacked-planet closure, matching
    * the four pre-existing legacy `Mode`s. */
  val defaultSecond: Coupling = PlanetStack

  /** The two welded bodies, for the central-shaft welds. `PlanetStack` returns
    * `None` — its solver row is the special planet-coupling equation, not a
    * plain `ω_a − ω_b = 0`. */
  def bodyPair(c: Coupling): Option[(Body, Body)] = c match
    case PlanetStack      => None
    case RingToSun        => Some((Body.R1, Body.S2))
    case SunToRing        => Some((Body.S1, Body.R2))
    case RingToRing       => Some((Body.R1, Body.R2))
    case SunToSun         => Some((Body.S1, Body.S2))
    case CarrierToCarrier => Some((Body.C1, Body.C2))
    case CarrierToSun     => Some((Body.C1, Body.S2))
    case CarrierToRing    => Some((Body.C1, Body.R2))
    case SunToCarrier     => Some((Body.S1, Body.C2))
    case RingToCarrier    => Some((Body.R1, Body.C2))

  /** True when a single physical pinion bridges both stages (so planet count
    * and module must match). Only `PlanetStack`. */
  def stacksPlanets(c: Coupling): Boolean = c == PlanetStack

  /** Assembly-phase feasibility: can a gear pair `(s1, s2)` actually be built
    * with this inter-stage coupling? For every coupling in the current model
    * the requirement reduces to each stage's own even-spacing rule
    * `(S + R) % N == 0` — the welded shafts don't intermesh with each other,
    * so the install phase is a free parameter. `Compounder.satisfiesSpacing`
    * further relaxes this per-mode for the legacy CDCP / NSS topologies.
    * Validated against a brute-force tooth-mesh enumerator in
    * `AssemblyPhaseSpec`. */
  def assemblable(c: Coupling, s1Even: Boolean, s2Even: Boolean): Boolean =
    s1Even && s2Even

  /** Display label for the coupling picker UI. */
  def label(c: Coupling): String = c match
    case PlanetStack      => "Stacked planets (P1 ↔ P2)"
    case RingToSun        => "Ring 1 ↔ Sun 2"
    case SunToRing        => "Sun 1 ↔ Ring 2"
    case RingToRing       => "Ring 1 ↔ Ring 2"
    case SunToSun         => "Sun 1 ↔ Sun 2"
    case CarrierToCarrier => "Carrier 1 ↔ Carrier 2 (shared carrier)"
    case CarrierToSun     => "Carrier 1 ↔ Sun 2"
    case CarrierToRing    => "Carrier 1 ↔ Ring 2"
    case SunToCarrier     => "Sun 1 ↔ Carrier 2"
    case RingToCarrier    => "Ring 1 ↔ Carrier 2"

  /** Short note on manufacturing complexity / bearing topology, shown as a
    * sub-line under the picker. Empty for the standard pieces. */
  def manufacturingNote(c: Coupling): String = c match
    case PlanetStack      => "Single stepped pinion per planet; both stages share one carrier."
    case CarrierToCarrier => ""
    case RingToSun        => "Stage-1 ring is hub-driven; stage-2 sun rides on the same shaft."
    case SunToRing        => "Stage-1 sun extends as a shaft; stage-2 ring rotates on it."
    case RingToRing       => "Both rings welded as one housing — sleeve/bearing for the carriers inside."
    case SunToSun         => "Both suns on a single through-shaft; carriers each need their own bearing."
    case CarrierToSun     => "Stage-1 carrier drives stage-2 sun (Ravigneaux-family)."
    case CarrierToRing    => "Stage-1 carrier drives stage-2 ring (Simpson-family)."
    case SunToCarrier     => "Stage-1 sun drives stage-2 carrier."
    case RingToCarrier    => "Stage-1 ring drives stage-2 carrier."
